library(shiny)
library(shinythemes)
library(dplyr)
library(callr)   # transitive dependency of ensure_worker_running(), declared
                  # here too so `library()` at the top tells the whole story

# All chunking/rendering/queue/library logic lives in pipeline_helpers.R so
# the same functions are usable from worker.R without pulling in Shiny.
source("pipeline_helpers.R")

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

  titlePanel("Sequential QR Generator: Text Stitcher"),

  tabsetPanel(
    id = "main_tabs",

    tabPanel(
      "1. Input & Settings",
      sidebarLayout(
        sidebarPanel(
          textInput("task_name", "Task name (optional):", value = "",
                     placeholder = "defaults to task_YYYYMMDD_HHMMSS"),
          sliderInput(
            "chunk_size",
            "Characters per QR Code (incl. header overhead):",
            min = 500, max = 3000, value = 1500, step = 100
          ),
          helpText("A small header (session ID, chunk index, checksum) is added to every code automatically, so the actual text payload per code is slightly smaller than this value. For Unicode or mixed text, smaller chunks are safer."),
          hr(),
          actionButton("add_to_queue", "Add to Queue", class = "btn-primary btn-lg", style = "width: 100%;"),
          helpText("Saves this text to the Library and lets the background worker render it. Returns immediately - keep pasting more codes right away."),
          br(),
          actionButton("generate", "Generate & Stream Now", class = "btn-success", style = "width: 100%;"),
          helpText("Renders in this browser session and jumps straight to playback. Fine for a quick one-off; for several big codes, use Add to Queue instead."),
          br(),
          actionButton("go_cleanser", "Go to Data Cleanser", class = "btn-warning", style = "width: 100%;")
        ),
        mainPanel(
          textAreaInput(
            "long_text", "Paste Full Text Below:", rows = 20, width = "100%",
            placeholder = "The app will split this text based on the slider value..."
          ),
          uiOutput("support_report"),
          hr(),
          h4("Resume from a saved file"),
          fileInput("resume_upload", "Load a saved task .json to match/resume:", accept = ".json"),
          uiOutput("resume_match_report")
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
          checkboxInput("auto_return_toggle", "Auto-return to Library when a full pass finishes", value = TRUE),
          helpText("Keep 'Seconds per code' at or above the receiver's cooldown setting so it has time to catch each frame. Looping lets any missed chunk be re-caught on the next pass. With auto-return on and looping off, finishing the last chunk sends you back to the Library to pick the next task - handy when streaming several codes back-to-back."),
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
            actionButton("queue_from_clean", "Add clean data to Queue", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
            hr(),
            helpText("Recommended workflow: Check -> Clean -> Generate/Queue from clean data.")
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
      "4. Library",
      fluidRow(
        column(
          width = 12,
          div(
            style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
            h3("Task Library"),
            div(
              actionButton("refresh_library", "\u27f3 Refresh", class = "btn-default"),
              actionButton("delete_selected", "Delete selected", class = "btn-danger")
            )
          ),
          helpText("Updates automatically every few seconds. Queued and in-progress tasks are being worked on by the background renderer - you can close this tab or the browser and it keeps going."),
          uiOutput("worker_status_report"),
          uiOutput("library_list")
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {

  # Start (or confirm) the background worker once per app process. Guarded
  # by the pid-alive check inside ensure_worker_running(), so a second
  # concurrent session (or an app restart with a still-running worker) will
  # not spawn a duplicate. The result is surfaced in the Library tab (and as
  # a startup notification on failure) so a dead/crashed worker is visible
  # immediately instead of tasks silently sitting at "queued" forever.
  worker_status <- reactiveVal(ensure_worker_running())
  observe({
    ws <- isolate(worker_status())
    if (!isTRUE(ws$alive)) {
      showNotification(ws$message, type = "error", duration = NULL)
    }
  })

  cleaned_text_value <- reactiveVal("")
  cleaning_summary_value <- reactiveVal("No check/clean action has been run yet.")
  detected_chars_value <- reactiveVal(
    data.frame(character = character(0), unicode = character(0), count = integer(0),
               suggested_action = character(0), stringsAsFactors = FALSE)
  )

  generated_chunks_value <- reactiveVal(NULL)  # list(sid=, name=, chunks=[list(raw=,wrapped=,image=), ...])
                                                # - same shape produced by the worker's stream.rds, so
                                                #   Play-from-Library and Generate & Stream Now feed the
                                                #   carousel identically.
  current_index <- reactiveVal(1)
  autoplay <- reactiveVal(FALSE)

  selected_library_rows <- reactiveVal(character(0))
  wired_play_buttons <- reactiveVal(character(0))  # names we've already attached a Play observer to

  # ---- Shared: load an already-rendered stream (from a completed Library
  # task, or from a synchronous Generate) into the carousel and switch tabs.
  start_sequence <- function(chunks_obj) {
    generated_chunks_value(chunks_obj)
    current_index(1)
    autoplay(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "2. QR Sequence")
  }

  # ---- Synchronous render (existing behavior, renamed "Generate & Stream Now") ----
  # NOTE: validate()/need() are designed for reactive/render contexts (they
  # signal a special "shiny.silent.error" that render functions catch and
  # turn into a friendly inline message). This function is always called
  # from inside observeEvent() blocks, which don't have that same handling -
  # depending on the installed Shiny version, that mismatch can surface as a
  # raw, uncaught error instead of a message (the "is.character(txt) is not
  # TRUE" crash). Plain if/showNotification()/return() works the same way
  # in every Shiny version and is the correct pattern for observers anyway.
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
    # Same shape as the worker's stream.rds (sid/name/chunks, each chunk a
    # self-contained list(raw=, wrapped=, image=)) so every downstream
    # carousel output only ever has to handle one format.
    stream_obj <- list(sid = chunks_obj$sid, name = NULL, chunks = rendered_chunks)
    start_sequence(stream_obj)
  }

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

  # ---- Add to Queue (from raw text box) ----
  observeEvent(input$add_to_queue, {
    req(input$long_text)
    nm <- queue_task(input$long_text, input$chunk_size, input$task_name)
    updateTextAreaInput(session, "long_text", value = "")
    updateTextInput(session, "task_name", value = "")
    showNotification(sprintf("Added to queue as '%s'. Check the Library tab for progress.", nm),
                      type = "message", duration = 5)
  })

  # ---- Add clean data to Queue ----
  observeEvent(input$queue_from_clean, {
    clean_txt <- input$clean_text_panel
    if (is.null(clean_txt) || !nzchar(clean_txt)) clean_txt <- cleaned_text_value()
    if (is.null(clean_txt) || !nzchar(clean_txt)) {
      showNotification("Clean data is empty. Please click 'Clean data for QR supports' first.", type = "warning")
      return()
    }
    nm <- queue_task(clean_txt, input$chunk_size, input$task_name)
    showNotification(sprintf("Added to queue as '%s'. Check the Library tab for progress.", nm),
                      type = "message", duration = 5)
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
          strong("No match found. "), "This looks like new content - use Add to Queue above to start it.")
    } else {
      m <- read_manifest(match_name)
      status <- m$status %||% "unknown"
      completed <- length(unlist(m$completed_chunks))
      total <- m$total_chunks %||% NA_integer_
      if (identical(status, "complete")) {
        div(style = "margin-top:8px; padding:10px; border-radius:6px; background:#eafaf1; color:#1e8449; font-size:13px;",
            strong(sprintf("Matched '%s' - already complete (%d/%d chunks). ", match_name, completed, total)),
            "Go to the Library tab and click Play.")
      } else {
        div(style = "margin-top:8px; padding:10px; border-radius:6px; background:#fef5e7; color:#9c640c; font-size:13px;",
            strong(sprintf("Matched '%s' - %d/%d chunks done, status: %s. ", match_name, completed, total, status)),
            "The background worker will keep resuming it automatically - no action needed.")
      }
    }
  })

  # ---- Carousel navigation ----
  # NOTE: this used to be `req(generated_chunks_value()); autoplay(TRUE)`, i.e.
  # it only checked for a non-NULL chunk-set object, not for that object
  # actually containing chunks. A stream with zero renderable chunks (empty
  # or malformed source text) passed req() just fine, flipped autoplay(TRUE),
  # and only THEN hit the validate(need(...)) guard inside current_chunk_set()
  # down in the render outputs - which is exactly the crash from the plain
  # if/showNotification() note above: validate()/need() outside a proper
  # render context (or, on some Shiny versions, even the raw stopifnot()
  # inside validate() itself) surfaces as an uncaught
  # "is.character(txt) is not TRUE" error instead of a friendly message.
  # Checking chunk count here, with a plain if/showNotification(), stops the
  # crash at its source - the Play button - instead of relying on
  # validate()/need() to catch it further downstream.
  observeEvent(input$play_btn, {
    gc <- generated_chunks_value()
    if (is.null(gc) || length(gc$chunks) == 0) {
      showNotification(
        "This task has no renderable chunks (empty or malformed data). Try re-queuing it.",
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
  # Phase 6 decision: when looping is off and a full pass finishes, and
  # "auto-return to Library" is on, jump back to the Library tab instead of
  # sitting idle on the last frame - matches the "flip to next track" feel
  # from the stated use case of streaming several codes back-to-back.
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
            updateTabsetPanel(session, "main_tabs", selected = "4. Library")
          }
          return()
        }
      }
      current_index(i)
    })
  })

  # ---- Carousel outputs ----

  # Shared guard: every carousel output needs (a) a non-null chunk-set object
  # and (b) at least one chunk in it. A stream/generated object that is
  # missing $chunks entirely, or has zero chunks, used to slip through req()
  # (which only checks for NULL/empty-string, not length-0 lists or n==0)
  # and reach sprintf("%d", ...) with an Inf/NaN percentage - that's the
  # "invalid format '%d'" crash. Centralizing the check here means every
  # output below fails soft instead of hard (a crashed render).
  #
  # Deliberately plain req() here, NOT validate(need(...)). The zero-chunk
  # case is now caught earlier, at the Play button (see above), with a
  # showNotification(); by the time autoplay is ever TRUE, generated_chunks_value()
  # is guaranteed to have >0 chunks. This req() is just a defensive backstop -
  # if it ever fires, the right behavior is for these outputs to render
  # nothing, not to route a message through validate()/need(), which is what
  # produced the uncaught "is.character(txt) is not TRUE" crash.
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

  # Live text preview - shows the raw (unwrapped) source text for whichever
  # chunk is currently on screen, so you can read along as playback advances.
  # Defaults to raw over the header/session-id/checksum-wrapped payload, since
  # raw is what a human actually wants to read; the wrapped version is
  # visible separately via the Payload length line above.
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
  # Library tab
  # =============================================================================

  # Polls the on-disk library state every 2s. Cheap - list_library_index()
  # only reads small manifest.json files, not any rendered images.
  library_index <- reactivePoll(
    2000, session,
    checkFunc = function() {
      # A fast-changing signature so reactivePoll knows something moved,
      # without doing the full (slightly heavier) index build every tick.
      files <- c(
        list.files(TASK_DIR, full.names = TRUE),
        list.files(LIBRARY_DIR, recursive = TRUE, full.names = TRUE, pattern = "manifest\\.json$")
      )
      if (!length(files)) return("empty")
      paste(files, file.info(files)$mtime, collapse = "|")
    },
    valueFunc = function() list_library_index()
  )

  observeEvent(input$refresh_library, { library_index(); worker_health() })

  # Live worker health check (re-reads the lock file + pid every 2s, same
  # cadence as the library index) so "is the background worker actually
  # running" is visible in the UI instead of something you infer from tasks
  # sitting at "queued" with no explanation.
  worker_health <- reactivePoll(
    2000, session,
    checkFunc = function() {
      if (file.exists(LOCK_FILE)) as.character(file.info(LOCK_FILE)$mtime) else "no-lock"
    },
    valueFunc = function() {
      if (!file.exists(LOCK_FILE)) return(list(alive = FALSE, pid = NA))
      info <- read_json_safe(LOCK_FILE)
      list(alive = !is.null(info) && pid_alive(info$pid), pid = info$pid %||% NA)
    }
  )

  output$worker_status_report <- renderUI({
    wh <- worker_health()
    if (isTRUE(wh$alive)) {
      div(style = "margin-bottom:10px; padding:8px 12px; border-radius:6px; background:#eafaf1; color:#1e8449; font-size:13px;",
          sprintf("\u2713 Background worker running (pid %s).", wh$pid))
    } else {
      err_log <- file.path(STORE_DIR, "worker_stderr.log")
      last_err <- if (file.exists(err_log)) {
        lines <- tryCatch(utils::tail(readLines(err_log, warn = FALSE), 5), error = function(e) character(0))
        if (length(lines)) paste(lines, collapse = " / ") else NULL
      } else NULL
      div(style = "margin-bottom:10px; padding:8px 12px; border-radius:6px; background:#fdecea; color:#c0392b; font-size:13px;",
          strong("\u26a0 Background worker not detected. "),
          "Tasks will sit as 'queued' until it's running. Click Refresh, or restart the app to respawn it.",
          if (!is.null(last_err)) tags$div(style = "margin-top:4px; font-family:monospace; font-size:11px;",
                                            paste("Last worker error:", last_err))
      )
    }
  })

  output$library_list <- renderUI({
    df <- library_index()
    if (nrow(df) == 0) {
      return(div(style = "color:#7f8c8d; padding:20px; text-align:center;",
                  "Nothing queued yet - add a code from the Input tab."))
    }

    selected <- selected_library_rows()

    rows <- lapply(seq_len(nrow(df)), function(i) {
      nm <- df$name[i]
      status <- df$status[i]
      completed <- df$completed[i]
      total <- df$total[i]
      pct <- if (!is.na(total) && total > 0) round(100 * completed / total) else 0

      badge_color <- switch(status,
        "complete"    = "#2ecc71",
        "in_progress" = "#3498db",
        "queued"      = "#95a5a6",
        "error"       = "#e74c3c",
        "#7f8c8d"
      )
      status_label <- switch(status,
        "complete"    = "Complete",
        "in_progress" = sprintf("Rendering %d/%d", completed, ifelse(is.na(total), 0L, total)),
        "queued"      = "Queued",
        "error"       = "Error",
        status
      )

      div(
        style = "display:flex; align-items:center; gap:12px; padding:10px 14px; border-bottom:1px solid #ecf0f1;",
        checkboxInput(paste0("sel_", nm), label = NULL, value = nm %in% selected, width = "20px"),
        div(
          style = "flex:1; min-width:0;",
          div(style = "font-weight:600; color:#2c3e50; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;", nm),
          div(
            style = "background:#ecf0f1;border-radius:8px;height:8px;overflow:hidden;margin-top:6px; max-width:400px;",
            div(style = sprintf("background:%s;height:8px;width:%d%%;transition:width 0.3s;", badge_color, pct))
          )
        ),
        span(style = sprintf("color:%s; font-size:12px; font-weight:600; white-space:nowrap;", badge_color), status_label),
        actionButton(paste0("play_", nm), "\u25b6 Play", class = "btn-success btn-sm",
                     disabled = !identical(status, "complete"))
      )
    })

    do.call(tagList, rows)
  })

  # Track checkbox state for multi-select delete. Each row's checkbox id is
  # dynamic (sel_<name>), so we watch the whole input list on any change.
  observe({
    df <- library_index()
    if (nrow(df) == 0) return()
    chosen <- vapply(df$name, function(nm) {
      val <- input[[paste0("sel_", nm)]]
      isTRUE(val)
    }, logical(1))
    selected_library_rows(df$name[chosen])
  })

  # Dynamic Play buttons: attach exactly one observer per task name, the
  # first time it's seen. reactivePoll refires this ~every 2s, so wiring a
  # fresh observeEvent unconditionally on every tick would stack up
  # duplicate observers over time - every extra tick, every past task,
  # means Play eventually fires once per accumulated observer.
  # wired_play_buttons() is the guard against that (Phase 6 fix).
  observe({
    df <- library_index()
    new_names <- setdiff(df$name, wired_play_buttons())
    if (!length(new_names)) return()

    lapply(new_names, function(nm) {
      btn_id <- paste0("play_", nm)
      observeEvent(input[[btn_id]], {
        stream_obj <- read_rds_safe(stream_path(nm))
        if (is.null(stream_obj)) {
          showNotification(sprintf("'%s' isn't ready to play yet.", nm), type = "warning")
          return()
        }
        start_sequence(stream_obj)
      }, ignoreInit = TRUE)
    })

    wired_play_buttons(union(wired_play_buttons(), new_names))
  })

  observeEvent(input$delete_selected, {
    sel <- selected_library_rows()
    if (!length(sel)) {
      showNotification("Select one or more tasks first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete selected tasks?",
      sprintf("This will permanently remove %d task(s) and their saved data. This cannot be undone.", length(sel)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_selected", "Delete", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete_selected, {
    sel <- selected_library_rows()
    for (nm in sel) delete_task(nm)
    selected_library_rows(character(0))
    wired_play_buttons(setdiff(wired_play_buttons(), sel))
    removeModal()
    showNotification(sprintf("Deleted %d task(s).", length(sel)), type = "message")
  })
}

shinyApp(ui = ui, server = server)
