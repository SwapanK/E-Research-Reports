library(shiny)
library(shinythemes)
library(dplyr)

# All chunking/rendering/manifest logic lives in pipeline_helpers_single.R.
# Unlike the original app, there is no worker.R, no callr, no queue - a job
# is processed chunk-by-chunk directly inside this Shiny session via an
# invalidateLater() loop, one job at a time.
source("pipeline_helpers_single.R")

# =============================================================================
# Caption
# =============================================================================

caption_text <- paste0(
  format(Sys.Date(), "%d %b %Y"),
  "   |  (c) DR SKM, ",
  format(Sys.Date(), "%Y")
)

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),

  titlePanel("Sequential QR Generator: Text Stitcher (single-job, no worker)"),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "1. Input & Settings",
      fluidRow(
        column(
          width = 5,
          uiOutput("job_panel")
        ),
        column(
          width = 7,
          uiOutput("right_panel"),
          hr(),
          div(
            style = "padding:14px 16px; border:1px solid #e1e4e8; border-radius:8px; background:#fafbfc;",
            h4(style = "margin-top:0;", "Resume from a saved file"),
            fileInput("resume_upload", "Load a saved task .json to match/resume:", accept = ".json"),
            uiOutput("resume_match_report")
          )
        )
      )
    ),

    tabPanel(
      "2. QR Sequence",
      fluidRow(
        column(
          width = 4,
          h3(textOutput("carousel_title", inline = TRUE)),
          div(
            style = "background:#ecf0f1;border-radius:10px;height:12px;overflow:hidden;margin-top:6px;",
            uiOutput("carousel_progress_bar")
          ),
          br(),
          div(
            style = "display:flex; gap:8px; flex-wrap:wrap;",
            actionButton("prev_btn", "\u23ee Prev"),
            actionButton("play_btn", "\u25b6 Play", class = "btn-success"),
            actionButton("pause_btn", "\u23f8 Pause", class = "btn-warning"),
            actionButton("next_btn", "Next \u23ed")
          ),
          br(),
          sliderInput("interval_slider", "Seconds per code:", min = 1, max = 10, value = 3, step = 0.5, width = "100%"),
          checkboxInput("loop_toggle", "Loop continuously (recommended)", value = TRUE),
          checkboxInput("auto_return_toggle", "Auto-return to Input when a full pass finishes", value = TRUE),
          helpText("Keep 'Seconds per code' at or above the receiver's cooldown setting so it has time to catch each frame. Looping lets any missed chunk be re-caught on the next pass."),
          hr(),
          div(
            style = "background:#f8f9fa; border:1px solid #e1e4e8; border-radius:6px; padding:10px 12px; margin-bottom:10px;",
            strong(style = "font-size:12px; color:#7f8c8d; text-transform:uppercase; letter-spacing:.03em;", "Text preview (current chunk)"),
            div(
              style = "font-size:13px; color:#2c3e50; margin-top:6px; max-height:160px; overflow-y:auto; white-space:pre-wrap; word-break:break-word;",
              textOutput("carousel_text_preview", inline = TRUE)
            )
          ),
          hr(),
          div(
            style = "font-size:15px; color:#2c3e50;",
            strong(textOutput("carousel_segment_label", inline = TRUE))
          ),
          div(
            style = "font-size:14px; color:#34495e; margin-top:4px;",
            textOutput("carousel_payload_label", inline = TRUE)
          ),
          div(
            style = "font-size:12px; color:#7f8c8d; margin-top:10px;",
            textOutput("carousel_caption_label", inline = TRUE)
          )
        ),
        column(
          width = 8, style = "text-align:center;",
          div(
            style = "max-width: 720px; margin: 0 auto;",
            uiOutput("carousel_qr_plot")
          )
        )
      )
    ),

    tabPanel(
      "3. Data Cleanser",
      fluidRow(
        column(
          width = 3,
          wellPanel(
            h3("QR Data Tools"),
            actionButton("check_qr_support", "Check data for QR supports", class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("clean_qr_support", "Clean data for QR supports", class = "btn-warning", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("generate_from_clean", "Generate & Stream from clean data", class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
            actionButton("process_from_clean", "Process clean data (resumable)", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            hr(),
            helpText("Recommended workflow: Check -> Clean -> Generate/Process from clean data.")
          )
        ),
        column(
          width = 9,
          tabsetPanel(
            id = "cleanser_subtabs",
            tabPanel("Summary", br(), verbatimTextOutput("cleaning_summary")),
            tabPanel("Detected Characters", br(), tableOutput("detected_table")),
            tabPanel("Clean Data", br(), textAreaInput("clean_text_panel", "Cleaned QR-safe text:", value = "", rows = 18, width = "100%"))
          )
        )
      )
    ),

    tabPanel(
      "4. History",
      fluidRow(
        column(
          width = 12,
          div(
            style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
            h3("Completed Jobs"),
            actionButton("refresh_history", "\u27f3 Refresh", class = "btn-default")
          ),
          helpText("Finished jobs only. There's no in-progress list here because only one job runs at a time - check Tab 1 for that."),
          uiOutput("history_list")
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------
  # Single-job state
  # ---------------------------------------------------------------------
  active_task_name <- reactiveVal(NULL)   # name of the one job being tracked
  processing       <- reactiveVal(FALSE)  # is the render loop allowed to tick
  render_tick      <- reactiveVal(0)      # bumped after every rendered chunk, to refresh the UI

  # On app start: if a job was left in_progress (app closed/crashed mid-job),
  # pick it up and resume immediately - no click needed. This is the
  # "auto start from previous point" behavior replacing the background worker.
  isolate({
    prev <- find_active_task()
    if (!is.null(prev)) {
      active_task_name(prev$name)
      processing(TRUE)
    }
  })
  observe({
    prev_name <- isolate(active_task_name())
    if (!is.null(prev_name)) {
      m <- isolate(read_manifest(prev_name))
      if (!is.null(m) && identical(m$status, "in_progress")) {
        showNotification(
          sprintf("Resuming previous job '%s' from %d/%d chunks - no action needed.",
                  prev_name, length(unlist(m$completed_chunks)), m$total_chunks),
          type = "message", duration = 6
        )
      }
    }
  })

  cleaned_text_value <- reactiveVal("")
  cleaning_summary_value <- reactiveVal("No check/clean action has been run yet.")
  detected_chars_value <- reactiveVal(
    data.frame(character = character(0), unicode = character(0), count = integer(0),
               suggested_action = character(0), stringsAsFactors = FALSE)
  )

  generated_chunks_value <- reactiveVal(NULL)
  current_index <- reactiveVal(1)
  autoplay <- reactiveVal(FALSE)

  # ---- Shared: load an already-rendered stream into the carousel ----
  start_sequence <- function(chunks_obj) {
    generated_chunks_value(chunks_obj)
    current_index(1)
    autoplay(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "2. QR Sequence")
  }

  # ---- Synchronous render ("Generate & Stream Now") - unchanged behavior,
  # independent of the job/resume system below; for quick one-off pastes. ----
  render_and_stream <- function(txt, chunk_size) {
    chunks_obj <- build_chunks(txt, chunk_size)
    if (is.null(chunks_obj)) {
      showNotification("Nothing to encode - please paste some text first.", type = "warning")
      return(invisible(NULL))
    }
    rendered_chunks <- withProgress(message = "Rendering QR codes...", value = 0, session = session, {
      n <- length(chunks_obj$chunks)
      render_all_images(
        chunks_obj$chunks, session,
        progress_fn = function(i, n) incProgress(1 / n)
      )
    })
    stream_obj <- list(sid = chunks_obj$sid, name = NULL, chunks = rendered_chunks)
    start_sequence(stream_obj)
  }

  # =============================================================================
  # Single-job start/resume logic
  # =============================================================================

  # Starts a new resumable job, or - if the pasted content exactly matches an
  # existing job by content-uid - resumes/loads that one instead. Refuses to
  # start a second job while one is already in_progress, since this app
  # deliberately sticks to one job at a time (no queue).
  start_or_resume_job <- function(txt, chunk_size, name) {
    if (is.null(txt) || !nzchar(trimws(txt))) {
      showNotification("Nothing to encode - please paste some text first.", type = "warning")
      return(invisible(NULL))
    }

    cur <- active_task_name()
    if (!is.null(cur)) {
      m <- read_manifest(cur)
      if (!is.null(m) && identical(m$status, "in_progress")) {
        showNotification(
          sprintf("Job '%s' is already in progress. Pause and discard it first if you want to start something else.", cur),
          type = "error", duration = 6
        )
        return(invisible(NULL))
      }
    }

    uid <- content_uid(txt, chunk_size)
    match_name <- find_library_match_by_uid(uid)

    if (!is.na(match_name)) {
      m <- read_manifest(match_name)
      active_task_name(match_name)
      if (identical(m$status, "complete")) {
        processing(FALSE)
        showNotification(sprintf("This exact text was already completed as '%s'.", match_name), type = "message")
      } else {
        processing(TRUE)
        showNotification(sprintf("Matches existing job '%s' - resuming from %d/%d chunks.",
                                  match_name, length(unlist(m$completed_chunks)), m$total_chunks),
                          type = "message")
      }
      return(invisible(NULL))
    }

    manifest <- start_task(txt, chunk_size, name)
    if (identical(manifest$status, "error")) {
      showNotification("Nothing to encode after chunking - please check your text.", type = "warning")
      return(invisible(NULL))
    }
    active_task_name(manifest$name)
    processing(TRUE)
    showNotification(
      sprintf("Started job '%s' (%d chunks). It keeps processing while this app stays open; pause any time.",
              manifest$name, manifest$total_chunks),
      type = "message", duration = 6
    )
  }

  observeEvent(input$go_process, {
    req(input$long_text)
    start_or_resume_job(input$long_text, input$chunk_size, input$task_name)
  })

  observeEvent(input$process_from_clean, {
    clean_txt <- input$clean_text_panel
    if (is.null(clean_txt) || !nzchar(clean_txt)) clean_txt <- cleaned_text_value()
    if (is.null(clean_txt) || !nzchar(clean_txt)) {
      showNotification("Clean data is empty. Please click 'Clean data for QR supports' first.", type = "warning")
      return()
    }
    start_or_resume_job(clean_txt, input$chunk_size, input$task_name)
    updateTabsetPanel(session, "main_tabs", selected = "1. Input & Settings")
  })

  observeEvent(input$pause_job, { processing(FALSE) })
  observeEvent(input$resume_job, {
    nm <- active_task_name()
    req(nm)
    m <- read_manifest(nm)
    if (!is.null(m) && identical(m$status, "in_progress")) processing(TRUE)
  })

  observeEvent(input$discard_job, {
    showModal(modalDialog(
      title = "Discard current job?",
      "This permanently deletes the in-progress job and its rendered chunks so far. This cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_discard_job", "Discard", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_discard_job, {
    nm <- active_task_name()
    if (!is.null(nm)) {
      processing(FALSE)
      delete_task(nm)
      active_task_name(NULL)
    }
    removeModal()
    showNotification("Job discarded.", type = "message")
  })

  observeEvent(input$play_completed_job, {
    nm <- active_task_name()
    req(nm)
    stream_obj <- read_rds_safe(stream_path(nm))
    if (is.null(stream_obj)) {
      showNotification("Stream not ready yet.", type = "warning")
      return()
    }
    start_sequence(stream_obj)
  })

  observeEvent(input$start_new_after_complete, {
    active_task_name(NULL)
    processing(FALSE)
  })

  # =============================================================================
  # The render loop - replaces worker.R. Ticks only while processing() is
  # TRUE and there is an active job. Each tick renders exactly one chunk and
  # checkpoints to disk (same manifest format as before), then re-schedules
  # itself with invalidateLater() so pause/discard clicks in between ticks
  # are honored right away instead of the loop running to completion first.
  # =============================================================================
  observe({
    nm <- active_task_name()
    req(nm)
    if (!isTRUE(processing())) return()
    invalidateLater(120, session)

    isolate({
      manifest <- read_manifest(nm)
      if (is.null(manifest)) { processing(FALSE); return() }

      if (identical(manifest$status, "complete")) {
        processing(FALSE)
        return()
      }
      if (identical(manifest$status, "error")) {
        processing(FALSE)
        showNotification(sprintf("Job '%s' has no renderable content.", nm), type = "error")
        return()
      }

      ok <- tryCatch(render_next_chunk_for(manifest), error = function(e) {
        showNotification(paste("Render error:", conditionMessage(e)), type = "error", duration = 8)
        FALSE
      })

      render_tick(isolate(render_tick()) + 1)

      if (!ok) {
        processing(FALSE)
        showNotification("Processing stopped after an error. You can Resume to try again.", type = "error")
      } else {
        m2 <- read_manifest(nm)
        if (!is.null(m2) && identical(m2$status, "complete")) {
          processing(FALSE)
          showNotification(sprintf("Job '%s' finished - %d chunks rendered.", nm, m2$total_chunks), type = "message")
        }
      }
    })
  })

  # ---- Live job status, polled by render_tick() so it updates the instant
  # a chunk finishes, with no separate file-polling timer needed. ----
  job_status <- reactive({
    render_tick()
    nm <- active_task_name()
    if (is.null(nm)) return(NULL)
    m <- read_manifest(nm)
    if (is.null(m)) return(NULL)
    m
  })

  CARD_STYLE <- "padding:16px 18px; border-radius:10px; border:1px solid #e1e4e8;"

  output$job_panel <- renderUI({
    m <- job_status()

    # ---- Idle: no job at all ----
    if (is.null(m)) {
      return(div(
        style = paste0(CARD_STYLE, "background:#eaf2f8; color:#2874a6;"),
        h4(style = "margin-top:0;", "\u2699\ufe0f No job running"),
        p(style = "color:#2c3e50; margin-bottom:10px;",
          "Paste your text on the right and click ", strong("Process"), " to start a resumable job."),
        tags$ul(
          style = "margin:0 0 0 18px; font-size:13px; color:#34495e; line-height:1.6;",
          tags$li("Renders one QR chunk at a time, right here in the app - no separate worker process."),
          tags$li("Pause any time without losing progress."),
          tags$li("Close the app and reopen it later - an unfinished job resumes automatically.")
        )
      ))
    }

    completed <- length(unlist(m$completed_chunks))
    total <- m$total_chunks %||% 0
    pct <- if (total > 0) round(100 * completed / total) else 0

    # ---- Complete ----
    if (identical(m$status, "complete")) {
      elapsed <- format_duration(m$total_render_seconds %||% 0)
      return(tagList(
        div(
          style = paste0(CARD_STYLE, "background:#eafaf1; color:#1e8449;"),
          h4(style = "margin-top:0;", sprintf("\u2713 '%s' complete", m$name)),
          p(style = "color:#186a3b; margin-bottom:12px; font-size:13px;",
            sprintf("%d/%d chunks rendered%s.", completed, total,
                    if (!is.na(elapsed)) sprintf(" in %s", elapsed) else "")),
          div(style = "display:flex; gap:8px; flex-wrap:wrap;",
              actionButton("play_completed_job", "\u25b6 Play", class = "btn-success"),
              actionButton("start_new_after_complete", "Start a new job", class = "btn-default")
          )
        )
      ))
    }

    # ---- In progress / paused ----
    status_color <- if (isTRUE(processing())) "#2980b9" else "#e67e22"
    status_bg <- if (isTRUE(processing())) "#eef6fb" else "#fef5e7"
    status_label <- if (isTRUE(processing())) "Processing\u2026" else "Paused"

    remaining <- max(total - completed, 0)
    total_secs <- m$total_render_seconds %||% 0
    avg_secs <- if (completed > 0) total_secs / completed else NA_real_
    eta_secs <- if (!is.na(avg_secs)) avg_secs * remaining else NA_real_
    eta_str <- format_duration(eta_secs)

    eta_line <- if (completed == 0) {
      "Estimating time remaining\u2026 (needs at least one rendered chunk)"
    } else if (!isTRUE(processing())) {
      sprintf("Paused - about %s of work left (avg %.1fs/chunk) once resumed.",
              ifelse(is.na(eta_str), "an unknown amount", eta_str), avg_secs)
    } else {
      sprintf("About %s remaining (avg %.1fs/chunk).",
              ifelse(is.na(eta_str), "an unknown amount of time", eta_str), avg_secs)
    }

    tagList(
      div(
        style = paste0(CARD_STYLE, sprintf("background:%s; color:%s;", status_bg, status_color)),
        h4(style = "margin-top:0;", sprintf("%s", m$name)),
        div(style = "font-size:13px; color:#2c3e50; margin-bottom:2px;",
            strong(status_label), sprintf(" \u2013 %d/%d chunks", completed, total)),
        div(
          style = "background:#ecf0f1;border-radius:8px;height:10px;overflow:hidden;margin:8px 0;",
          div(style = sprintf("background:%s;height:10px;width:%d%%;transition:width 0.2s;", status_color, pct))
        ),
        div(style = "font-size:12.5px; color:#34495e; margin-bottom:12px;", eta_line),
        div(style = "display:flex; gap:8px; flex-wrap:wrap;",
            if (isTRUE(processing())) {
              actionButton("pause_job", "\u23f8 Pause", class = "btn-warning")
            } else {
              actionButton("resume_job", "\u25b6 Resume", class = "btn-success")
            },
            actionButton("discard_job", "Discard", class = "btn-danger")
        )
      ),
      helpText("Keeps processing while this app is open, with no separate worker process. Close the app and reopen it later to auto-resume right where it left off.")
    )
  })

  # ---- Right column: while a job is running, the paste box is replaced by
  # a live, read-only preview of what's being processed (so the space is
  # never empty); otherwise it's the normal input form. ----
  output$right_panel <- renderUI({
    m <- job_status()
    busy <- !is.null(m) && identical(m$status, "in_progress")

    if (busy) {
      src <- get_source_text(active_task_name())
      tagList(
        div(
          style = "padding:16px 18px; border:1px solid #d6eaf8; border-radius:10px; background:#f7fbfe;",
          strong(style = "color:#2874a6; font-size:14px;", "\U0001F4C4 Currently processing this text"),
          div(
            style = "margin-top:10px; font-size:13px; color:#2c3e50; max-height:380px; overflow-y:auto; white-space:pre-wrap; word-break:break-word; background:white; border:1px solid #eaeff2; border-radius:6px; padding:12px;",
            src %||% "(source text unavailable)"
          )
        ),
        helpText("Read-only while the job is running. Pause or discard it (left) to edit or paste something new.")
      )
    } else {
      uiOutput("input_form")
    }
  })

  # ---- The paste-text input form: shown whenever no job is blocking it
  # (idle, or a completed job sitting there waiting for "Start a new job"). ----
  output$input_form <- renderUI({
    tagList(
      div(
        style = "padding:16px 18px; border:1px solid #e1e4e8; border-radius:10px; background:#fafbfc;",
        h4(style = "margin-top:0;", "New job"),
        textInput("task_name", "Task name (optional):", value = "",
                   placeholder = "defaults to task_YYYYMMDD_HHMMSS"),
        sliderInput(
          "chunk_size",
          "Characters per QR Code (incl. header overhead):",
          min = 500, max = 3000, value = 1500, step = 100
        ),
        helpText("A small header (session ID, chunk index, checksum) is added to every code automatically, so the actual text payload per code is slightly smaller than this value. For Unicode or mixed text, smaller chunks are safer."),
        textAreaInput(
          "long_text", "Paste Full Text Below:", rows = 12, width = "100%",
          placeholder = "The app will split this text based on the slider value..."
        ),
        uiOutput("support_report"),
        hr(),
        actionButton("go_process", "Process (resumable)", class = "btn-primary btn-lg", style = "width: 100%;"),
        helpText("Renders one chunk at a time inside this app, checkpointing to disk. Pause any time from the panel on the left; reopening the app auto-resumes an unfinished job."),
        br(),
        actionButton("generate", "Generate & Stream Now", class = "btn-success", style = "width: 100%;"),
        helpText("Renders everything immediately in this browser session and jumps straight to playback. Fine for a quick one-off; for long texts, use Process instead so you can pause."),
        br(),
        actionButton("go_cleanser", "Go to Data Cleanser", class = "btn-warning", style = "width: 100%;")
      )
    )
  })

  # ---- Live, non-blocking support report (informational only) ----
  long_text_r <- reactive({ input$long_text })
  debounced_text <- debounce(long_text_r, 700)

  output$support_report <- renderUI({
    txt <- debounced_text()
    if (is.null(txt) || !nzchar(txt)) {
      return(div(style = "color:#7f8c8d; font-size:13px; margin-top:8px;",
                 "Support check: paste text above to see a live report."))
    }
    detected <- detect_qr_unsafe_chars(txt)
    if (nrow(detected) == 0) {
      div(style = "margin-top:10px; padding:10px; border-radius:6px; background:#eafaf1; color:#1e8449; font-size:13px;",
          strong("\u2713 Support check: "), "no unsupported characters detected. Safe to generate as-is.")
    } else {
      top <- head(detected, 5)
      div(style = "margin-top:10px; padding:10px; border-radius:6px; background:#fef5e7; color:#9c640c; font-size:13px;",
          strong(sprintf("\u26a0 Support check: %d unsupported character type(s) found (%d occurrence(s) total). ",
                         nrow(detected), sum(detected$count))),
          "This is informational only - you can still Generate as-is, or use the Data Cleanser tab to fix them.",
          tags$ul(style = "margin:6px 0 0 18px;",
                  lapply(seq_len(nrow(top)), function(i) {
                    tags$li(sprintf("[%s] %s - seen %d time(s) - %s",
                                    top$character[i], top$unicode[i], top$count[i], top$suggested_action[i]))
                  })
          )
      )
    }
  })

  observeEvent(input$go_cleanser, {
    updateTabsetPanel(session, "main_tabs", selected = "3. Data Cleanser")
  })

  # ---- Check ----
  observeEvent(input$check_qr_support, {
    req(input$long_text)
    detected <- detect_qr_unsafe_chars(input$long_text)
    detected_chars_value(detected)

    summary_text <- c(
      "QR SUPPORT CHECK SUMMARY",
      "============================================================",
      paste0("Character count : ", nchar(input$long_text, type = "chars")),
      paste0("Byte count      : ", nchar(input$long_text, type = "bytes")),
      paste0("Unsafe unique characters detected : ", nrow(detected)),
      paste0("Unsafe total occurrences detected : ", ifelse(nrow(detected) > 0, sum(detected$count), 0)),
      "", "Note:",
      "- ASCII text is safest for QR stitching.",
      "- Unicode, emoji, curly quotes, bullets, arrows, hidden characters, and symbols can increase QR density or fail in some scanners."
    )

    if (nrow(detected) > 0) {
      summary_text <- c(summary_text, "", "Action needed:", "- Click 'Clean data for QR supports' to replace/remove risky characters.")
    } else {
      summary_text <- c(summary_text, "", "Result:", "- No risky non-ASCII characters detected.")
    }

    cleaning_summary_value(paste(summary_text, collapse = "\n"))
    updateTabsetPanel(session, "main_tabs", selected = "3. Data Cleanser")
    updateTabsetPanel(session, "cleanser_subtabs", selected = "Detected Characters")
  })

  # ---- Clean ----
  observeEvent(input$clean_qr_support, {
    req(input$long_text)
    raw_txt <- input$long_text
    detected <- detect_qr_unsafe_chars(raw_txt)
    cleaned <- clean_qr_text(raw_txt)

    detected_chars_value(detected)
    cleaned_text_value(cleaned)
    cleaning_summary_value(make_cleaning_summary_text(raw_txt, cleaned, detected))

    updateTextAreaInput(session, "clean_text_panel", value = cleaned)
    updateTabsetPanel(session, "main_tabs", selected = "3. Data Cleanser")
    updateTabsetPanel(session, "cleanser_subtabs", selected = "Clean Data")
  })

  # ---- Generate & Stream Now (from raw text box) ----
  observeEvent(input$generate, {
    req(input$long_text)
    render_and_stream(input$long_text, input$chunk_size)
  })

  # ---- Generate & Stream from clean data ----
  observeEvent(input$generate_from_clean, {
    clean_txt <- input$clean_text_panel
    if (is.null(clean_txt) || !nzchar(clean_txt)) clean_txt <- cleaned_text_value()
    if (is.null(clean_txt) || !nzchar(clean_txt)) {
      showNotification("Clean data is empty. Please click 'Clean data for QR supports' first.", type = "warning")
      return()
    }
    updateTextAreaInput(session, "long_text", value = clean_txt)
    cleaned_text_value(clean_txt)
    render_and_stream(clean_txt, input$chunk_size)
  })

  # ---- Resume-by-reload: match an uploaded json against the Library by content hash ----
  output$resume_match_report <- renderUI({
    req(input$resume_upload)
    payload <- tryCatch(jsonlite::fromJSON(input$resume_upload$datapath, simplifyVector = TRUE),
                         error = function(e) NULL)
    if (is.null(payload) || is.null(payload$text)) {
      return(div(style = "color:#c0392b; font-size:13px;", "Could not read that file as a task json."))
    }
    chunk_size <- payload$chunk_size %||% input$chunk_size
    uid <- payload$uid %||% content_uid(payload$text, chunk_size)
    match_name <- find_library_match_by_uid(uid)

    if (is.na(match_name)) {
      div(style = "margin-top:8px; padding:10px; border-radius:6px; background:#eaf2f8; color:#2874a6; font-size:13px;",
          strong("No match found. "), "This looks like new content - paste it and click Process above to start it.")
    } else {
      m <- read_manifest(match_name)
      status <- m$status %||% "unknown"
      completed <- length(unlist(m$completed_chunks))
      total <- m$total_chunks %||% NA_integer_
      if (identical(status, "complete")) {
        div(style = "margin-top:8px; padding:10px; border-radius:6px; background:#eafaf1; color:#1e8449; font-size:13px;",
            strong(sprintf("Matched '%s' - already complete (%d/%d chunks). ", match_name, completed, total)),
            "Click Play in the job panel above.")
      } else {
        active_task_name(match_name)
        div(style = "margin-top:8px; padding:10px; border-radius:6px; background:#fef5e7; color:#9c640c; font-size:13px;",
            strong(sprintf("Matched '%s' - %d/%d chunks done, status: %s. ", match_name, completed, total, status)),
            "Loaded into the job panel above - click Resume to continue.")
      }
    }
  })

  # ---- Carousel navigation ----
  observeEvent(input$play_btn, {
    gc <- generated_chunks_value()
    if (is.null(gc) || length(gc$chunks) == 0) {
      showNotification(
        "This task has no renderable chunks (empty or malformed data). Try processing it again.",
        type = "error"
      )
      return(invisible(NULL))
    }
    autoplay(TRUE)
  })
  observeEvent(input$pause_btn, { autoplay(FALSE) })

  observeEvent(input$prev_btn, {
    gc <- generated_chunks_value(); req(gc)
    n <- length(gc$chunks)
    req(n > 0)
    i <- current_index() - 1
    current_index(if (i < 1) n else i)
  })

  observeEvent(input$next_btn, {
    gc <- generated_chunks_value(); req(gc)
    n <- length(gc$chunks)
    req(n > 0)
    i <- current_index() + 1
    current_index(if (i > n) 1 else i)
  })

  # ---- Auto-advance timer ----
  observe({
    req(autoplay())
    invalidateLater(input$interval_slider * 1000, session)
    isolate({
      gc <- generated_chunks_value()
      if (is.null(gc)) return()
      n <- length(gc$chunks)
      if (n <= 1) return()
      i <- current_index() + 1
      if (i > n) {
        if (isTRUE(input$loop_toggle)) {
          i <- 1
        } else {
          i <- n
          autoplay(FALSE)
          if (isTRUE(input$auto_return_toggle)) {
            updateTabsetPanel(session, "main_tabs", selected = "1. Input & Settings")
          }
          return()
        }
      }
      current_index(i)
    })
  })

  # ---- Carousel outputs ----
  current_chunk_set <- reactive({
    gc <- generated_chunks_value()
    req(gc)
    req(length(gc$chunks) > 0)
    gc
  })

  output$carousel_title <- renderText({
    gc <- current_chunk_set()
    n <- length(gc$chunks)
    label <- gc$name %||% gc$sid %||% "-"
    sprintf("Part %d of %d   |   %s   |   %s", current_index(), n, label,
            if (isTRUE(autoplay())) "Playing" else "Paused")
  })

  output$carousel_progress_bar <- renderUI({
    gc <- current_chunk_set()
    n <- length(gc$chunks)
    pct <- round(100 * current_index() / n)
    div(style = sprintf("background:#2ecc71;height:12px;width:%d%%;transition:width 0.3s;", pct))
  })

  output$carousel_segment_label <- renderText({
    gc <- current_chunk_set()
    n <- length(gc$chunks)
    sprintf("Segment %d of %d", current_index(), n)
  })

  output$carousel_payload_label <- renderText({
    gc <- current_chunk_set()
    idx <- current_index()
    sprintf("Payload length: %d chars", nchar(gc$chunks[[idx]]$wrapped, type = "chars"))
  })

  output$carousel_text_preview <- renderText({
    gc <- current_chunk_set()
    idx <- current_index()
    gc$chunks[[idx]]$raw %||% ""
  })

  output$carousel_caption_label <- renderText({
    req(generated_chunks_value())
    caption_text
  })

  output$carousel_qr_plot <- renderUI({
    gc <- current_chunk_set()
    idx <- current_index()
    tags$img(
      src = gc$chunks[[idx]]$image,
      style = "max-width:100%; height:auto; display:block; margin:0 auto;"
    )
  })

  # ---- Cleanser outputs ----
  output$cleaning_summary <- renderText({ cleaning_summary_value() })
  output$detected_table <- renderTable({ detected_chars_value() })

  # =============================================================================
  # History tab (completed jobs only - read-only, no queue semantics)
  # =============================================================================

  history_refresh <- reactiveVal(0)
  observeEvent(input$refresh_history, { history_refresh(isolate(history_refresh()) + 1) })
  observeEvent(render_tick(), { history_refresh(isolate(history_refresh()) + 1) })

  output$history_list <- renderUI({
    history_refresh()
    df <- list_completed_tasks()
    if (nrow(df) == 0) {
      return(div(style = "color:#7f8c8d; padding:20px; text-align:center;",
                  "No completed jobs yet."))
    }

    rows <- lapply(seq_len(nrow(df)), function(i) {
      nm <- df$name[i]
      div(
        style = "display:flex; align-items:center; gap:12px; padding:10px 14px; border-bottom:1px solid #ecf0f1;",
        div(style = "flex:1; min-width:0; font-weight:600; color:#2c3e50;", nm),
        span(style = "color:#7f8c8d; font-size:12px;", sprintf("%s chunks", df$total[i])),
        actionButton(paste0("hist_play_", nm), "\u25b6 Play", class = "btn-success btn-sm"),
        actionButton(paste0("hist_delete_", nm), "Delete", class = "btn-danger btn-sm")
      )
    })
    do.call(tagList, rows)
  })

  # Dynamic Play/Delete buttons for history rows, wired once per name seen.
  wired_history_buttons <- reactiveVal(character(0))
  observe({
    history_refresh()
    df <- list_completed_tasks()
    new_names <- setdiff(df$name, wired_history_buttons())
    if (!length(new_names)) return()

    lapply(new_names, function(nm) {
      observeEvent(input[[paste0("hist_play_", nm)]], {
        stream_obj <- read_rds_safe(stream_path(nm))
        if (is.null(stream_obj)) {
          showNotification(sprintf("'%s' isn't ready to play.", nm), type = "warning")
          return()
        }
        start_sequence(stream_obj)
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("hist_delete_", nm)]], {
        delete_task(nm)
        history_refresh(isolate(history_refresh()) + 1)
        showNotification(sprintf("Deleted '%s'.", nm), type = "message")
      }, ignoreInit = TRUE)
    })

    wired_history_buttons(union(wired_history_buttons(), new_names))
  })
}

shinyApp(ui = ui, server = server)
