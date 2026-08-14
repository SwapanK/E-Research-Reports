library(shiny)
library(qrcode)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(base64enc)  # needed to pre-render QR images so switching is instant, not dull/recalculating

# =============================================================================
# Caption
# =============================================================================

caption_text <- paste0(
  format(Sys.Date(), "%d %b %Y"),
  "   |  (c) DR SKM, ",
  format(Sys.Date(), "%Y")
)

# =============================================================================
# Helper 1: Detect non-QR-safe / non-ASCII characters
# =============================================================================

detect_qr_unsafe_chars <- function(x) {
  x <- enc2utf8(x)
  
  if (is.null(x) || !nzchar(x)) {
    return(data.frame(
      character = character(0),
      unicode = character(0),
      count = integer(0),
      suggested_action = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  chars <- strsplit(x, "", useBytes = FALSE)[[1]]
  
  char_code <- vapply(chars, function(ch) {
    code <- utf8ToInt(ch)
    if (length(code) == 0 || is.na(code[1])) return(NA_integer_)
    as.integer(code[1])
  }, integer(1))
  
  allowed_control <- c(9L, 10L, 13L)
  
  unsafe_flag <- (
    is.na(char_code) |
      char_code > 127L |
      (char_code < 32L & !(char_code %in% allowed_control)) |
      char_code == 127L
  )
  
  bad <- chars[unsafe_flag]
  
  if (!length(bad)) {
    return(data.frame(
      character = character(0),
      unicode = character(0),
      count = integer(0),
      suggested_action = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  tab <- sort(table(bad), decreasing = TRUE)
  bad_unique <- names(tab)
  
  get_code <- function(ch) {
    code <- utf8ToInt(ch)
    if (length(code) == 0 || is.na(code[1])) return(NA_integer_)
    as.integer(code[1])
  }
  
  get_action <- function(ch) {
    code <- get_code(ch)
    if (ch %in% c("\u201C", "\u201D")) return("Replace curly double quote with normal quote")
    if (ch %in% c("\u2018", "\u2019")) return("Replace curly single quote with apostrophe")
    if (ch %in% c("\u2013", "\u2014", "\u2212")) return("Replace special dash with hyphen")
    if (ch == "\u00A9") return("Replace copyright symbol with (c)")
    if (ch == "\u00AE") return("Replace registered symbol with (R)")
    if (ch == "\u2122") return("Replace trademark symbol with (TM)")
    if (ch %in% c("\u2022", "\u25E6", "\u25AA", "\u25AB")) return("Replace bullet with hyphen")
    if (ch %in% c("\u2192", "\u21D2")) return("Replace arrow with ->")
    if (ch %in% c("\u2713", "\u2714")) return("Replace check mark with TRUE")
    if (ch %in% c("\u2717", "\u2718")) return("Replace cross mark with FALSE")
    if (ch %in% c("\u200B", "\u200C", "\u200D", "\uFEFF")) return("Remove hidden zero-width/control character")
    if (ch == "\u00A0") return("Replace non-breaking space with normal space")
    if (!is.na(code) && code > 127L) return("Convert/remove non-ASCII Unicode character")
    if (!is.na(code) && code < 32L && !(code %in% allowed_control)) return("Remove hidden ASCII control character")
    if (!is.na(code) && code == 127L) return("Remove DEL control character")
    "Review character"
  }
  
  code_unique <- vapply(bad_unique, get_code, integer(1))
  
  data.frame(
    character = bad_unique,
    unicode = ifelse(is.na(code_unique), "UNKNOWN", sprintf("U+%04X", code_unique)),
    count = as.integer(tab),
    suggested_action = vapply(bad_unique, get_action, character(1)),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# Helper 2: Clean text for QR support
# =============================================================================

clean_qr_text <- function(x) {
  x <- enc2utf8(x)
  if (is.null(x) || !nzchar(x)) return("")
  
  before <- x
  
  x <- gsub("\u201C|\u201D", "\"", x)
  x <- gsub("\u2018|\u2019", "'", x)
  x <- gsub("\u2013|\u2014|\u2212", "-", x)
  x <- gsub("\u00A9", "(c)", x)
  x <- gsub("\u00AE", "(R)", x)
  x <- gsub("\u2122", "(TM)", x)
  x <- gsub("\u2022|\u25E6|\u25AA|\u25AB", "-", x)
  x <- gsub("\u2192|\u21D2", "->", x)
  x <- gsub("\u2713|\u2714", "TRUE", x)
  x <- gsub("\u2717|\u2718", "FALSE", x)
  x <- gsub("[\u200B\u200C\u200D\uFEFF]", "", x)
  x <- gsub("\u00A0", " ", x)
  
  chars <- strsplit(x, "", useBytes = FALSE)[[1]]
  
  if (length(chars) > 0) {
    codes <- vapply(chars, function(ch) {
      code <- utf8ToInt(ch)
      if (length(code) == 0 || is.na(code[1])) return(NA_integer_)
      as.integer(code[1])
    }, integer(1))
    
    allowed_control <- c(9L, 10L, 13L)
    
    keep <- !(
      is.na(codes) |
        (codes < 32L & !(codes %in% allowed_control)) |
        codes == 127L
    )
    
    x <- paste0(chars[keep], collapse = "")
  }
  
  x2 <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  if (is.na(x2)) x2 <- iconv(x, from = "UTF-8", to = "ASCII", sub = "")
  if (is.na(x2)) x2 <- ""
  x <- x2
  
  x <- gsub("[ \t]+", " ", x)
  x <- gsub(" *\n *", "\n", x)
  
  attr(x, "chars_before") <- nchar(before, type = "chars")
  attr(x, "chars_after") <- nchar(x, type = "chars")
  attr(x, "bytes_before") <- nchar(before, type = "bytes")
  attr(x, "bytes_after") <- nchar(x, type = "bytes")
  
  x
}

# =============================================================================
# Helper 3: Cleaning summary
# =============================================================================

make_cleaning_summary_text <- function(raw_text, cleaned_text, detected_df) {
  before_chars <- nchar(raw_text, type = "chars")
  after_chars <- nchar(cleaned_text, type = "chars")
  before_bytes <- nchar(raw_text, type = "bytes")
  after_bytes <- nchar(cleaned_text, type = "bytes")
  char_change <- before_chars - after_chars
  byte_change <- before_bytes - after_bytes
  
  lines <- c(
    "QR DATA CLEANING SUMMARY",
    "============================================================",
    paste0("Original character count : ", before_chars),
    paste0("Cleaned character count  : ", after_chars),
    paste0("Character count change   : ", char_change),
    paste0("Original byte count      : ", before_bytes),
    paste0("Cleaned byte count       : ", after_bytes),
    paste0("Byte count change        : ", byte_change),
    "",
    paste0("Unsafe unique characters detected : ", nrow(detected_df)),
    paste0("Unsafe total occurrences detected : ", ifelse(nrow(detected_df) > 0, sum(detected_df$count), 0)),
    ""
  )
  
  if (nrow(detected_df) > 0) {
    lines <- c(lines, "Detected unsupported / risky characters:", "------------------------------------------------------------")
    for (i in seq_len(nrow(detected_df))) {
      lines <- c(lines, paste0(
        i, ". Character: [", detected_df$character[i], "]",
        " | Unicode: ", detected_df$unicode[i],
        " | Count: ", detected_df$count[i],
        " | Action: ", detected_df$suggested_action[i]
      ))
    }
  } else {
    lines <- c(lines, "No non-ASCII / risky characters detected.")
  }
  
  paste(lines, collapse = "\n")
}

# =============================================================================
# Helper 4: Split text into raw chunks
# =============================================================================

split_text_to_chunks <- function(txt, size) {
  if (is.null(txt) || !nzchar(txt)) return(character(0))
  starts <- seq(1, nchar(txt), by = size)
  chunks <- sapply(starts, function(s) substring(txt, s, s + size - 1))
  chunks[nzchar(trimws(chunks))]
}

# =============================================================================
# Helper 5: Chunk header / checksum / wrapping for reliable reassembly
#   Format: ##QRP|<sid>|<idx>|<total>|<checksum>||<data>
# =============================================================================

HEADER_OVERHEAD <- 40  # reserved chars for header so payload still fits QR capacity

simple_checksum <- function(x) {
  sum(utf8ToInt(enc2utf8(x))) %% 100000
}

generate_sid <- function() {
  paste0(sample(c(LETTERS, letters, 0:9), 6, replace = TRUE), collapse = "")
}

wrap_chunk <- function(sid, idx, total, data) {
  chk <- simple_checksum(data)
  sprintf("##QRP|%s|%d|%d|%d||%s", sid, idx, total, chk, data)
}

# Builds both the raw chunks (for display/char counts) and the header-wrapped
# payloads (what's actually encoded into each QR code). All chunks in one
# call share one session ID (sid) so the receiver can group them together.
build_chunks <- function(txt, chunk_size) {
  effective_size <- max(50, chunk_size - HEADER_OVERHEAD)
  raw <- split_text_to_chunks(txt, effective_size)
  n <- length(raw)
  if (n == 0) return(NULL)
  sid <- generate_sid()
  wrapped <- vapply(seq_along(raw), function(i) wrap_chunk(sid, i, n, raw[i]), character(1))
  list(sid = sid, raw = raw, wrapped = wrapped)
}

# =============================================================================
# Helper 6: Pre-render every QR code to a base64 image up front.
#   Doing this once at Generate time (instead of on every carousel advance)
#   is what keeps the display crisp during playback - Shiny dims/greys an
#   output while it's recalculating, which is the "dull" flash you get if
#   each QR is drawn fresh as the carousel moves to it.
# =============================================================================

render_chunk_image <- function(content, idx, total) {
  qr_obj <- tryCatch(qrcode::qr_code(content), error = function(e) NULL)
  
  tmpfile <- tempfile(fileext = ".png")
  render_ok <- TRUE
  render_err <- NULL
  
  grDevices::png(filename = tmpfile, width = 7, height = 7, units = "in", res = 130, bg = "white")
  tryCatch({
    if (is.null(qr_obj)) {
      print(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = "Error: QR capacity exceeded.\nReduce 'Characters per QR Code'.",
                   size = 6, fontface = "bold") +
          theme_void()
      )
    } else {
      result <- plot(qr_obj)
      
      if (idx == 1) {
        message("[QR diag] class(qr_obj) = ", paste(class(qr_obj), collapse = ", "))
        message("[QR diag] class(plot(qr_obj)) = ", paste(class(result), collapse = ", "))
      }
      
      if (inherits(result, "ggplot")) {
        # plot() built a ggplot object - it still needs an explicit print()
        # to actually render (auto-print doesn't fire inside a function call).
        # No title/subtitle/caption here on purpose: all of that text now
        # lives on the left-hand panel in the UI, so the image itself stays
        # a clean, undistracted scan target.
        print(
          result +
            theme_void() +
            theme(plot.margin = margin(0, 0, 0, 0))
        )
      } else {
        # plot() draws straight to the active device via base graphics as a
        # side effect (that's why the earlier version leaked to RStudio's
        # Plots pane) - it already drew into our offscreen png() device
        # above. Left intentionally bare: no title/sub/caption text is
        # added on top, so the QR pattern is the only thing in the image.
      }
    }
  }, error = function(e) {
    render_ok <<- FALSE
    render_err <<- conditionMessage(e)
  }, finally = {
    grDevices::dev.off()
  })
  
  file_size <- if (file.exists(tmpfile)) file.info(tmpfile)$size else 0
  
  # A real QR pattern compresses much larger than a blank/near-blank canvas -
  # anything this small means nothing actually got drawn.
  if (!render_ok || is.na(file_size) || file_size < 700) {
    message(sprintf(
      "[QR render] chunk %d/%d FAILED/blank - render_ok=%s, file_size=%s, error=%s",
      idx, total, render_ok, file_size, if (is.null(render_err)) "none" else render_err
    ))
    if (file.exists(tmpfile)) unlink(tmpfile)
    
    fb_file <- tempfile(fileext = ".png")
    grDevices::png(filename = fb_file, width = 7, height = 7, units = "in", res = 130, bg = "white")
    plot.new()
    text(0.5, 0.5, paste0("QR render failed for chunk ", idx, "\nSee R console for details"), col = "red", cex = 1.1)
    grDevices::dev.off()
    raw <- readBin(fb_file, "raw", file.info(fb_file)$size)
    unlink(fb_file)
    return(paste0("data:image/png;base64,", base64enc::base64encode(raw)))
  }
  
  message(sprintf("[QR render] chunk %d/%d OK - file_size=%d bytes", idx, total, file_size))
  raw <- readBin(tmpfile, "raw", file_size)
  unlink(tmpfile)
  paste0("data:image/png;base64,", base64enc::base64encode(raw))
}

render_all_images <- function(wrapped, session) {
  n <- length(wrapped)
  images <- vector("list", n)
  withProgress(message = "Rendering QR codes...", value = 0, session = session, {
    for (i in seq_len(n)) {
      images[[i]] <- render_chunk_image(wrapped[i], i, n)
      incProgress(1 / n)
    }
  })
  images
}

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
          sliderInput(
            "chunk_size",
            "Characters per QR Code (incl. header overhead):",
            min = 500, max = 3000, value = 1500, step = 100
          ),
          helpText("A small header (session ID, chunk index, checksum) is added to every code automatically, so the actual text payload per code is slightly smaller than this value. For Unicode or mixed text, smaller chunks are safer."),
          hr(),
          actionButton("generate", "Generate QR Sequence", class = "btn-primary btn-lg", style = "width: 100%;"),
          br(), br(),
          actionButton("go_cleanser", "Go to Data Cleanser", class = "btn-warning", style = "width: 100%;")
        ),
        mainPanel(
          textAreaInput(
            "long_text", "Paste Full Text Below:", rows = 20, width = "100%",
            placeholder = "The app will split this text based on the slider value..."
          ),
          uiOutput("support_report")
        )
      )
    ),
    
    tabPanel(
      "2. QR Sequence",
      fluidRow(
        # ---- Left: all text & controls ----
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
            actionButton("prev_btn", "⏮ Prev"),
            actionButton("play_btn", "▶ Play", class = "btn-success"),
            actionButton("pause_btn", "⏸ Pause", class = "btn-warning"),
            actionButton("next_btn", "Next ⏭")
          ),
          br(),
          sliderInput("interval_slider", "Seconds per code:", min = 1, max = 10, value = 3, step = 0.5, width = "100%"),
          checkboxInput("loop_toggle", "Loop continuously (recommended)", value = TRUE),
          helpText("Keep 'Seconds per code' at or above the receiver's cooldown setting so it has time to catch each frame. Looping lets any missed chunk be re-caught on the next pass."),
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
        # ---- Right: QR code, maximized, clean target ----
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
            actionButton("generate_from_clean", "Generate QR from clean data", class = "btn-success", style = "width: 100%; margin-bottom: 10px;"),
            hr(),
            helpText("Recommended workflow: Check -> Clean -> Generate QR from clean data.")
          )
        ),
        column(
          width = 9,
          tabsetPanel(
            tabPanel("Clean Data", br(), textAreaInput("clean_text_panel", "Cleaned QR-safe text:", value = "", rows = 18, width = "100%")),
            tabPanel("Summary", br(), verbatimTextOutput("cleaning_summary")),
            tabPanel("Detected Characters", br(), tableOutput("detected_table"))
          )
        )
      )
    )
  )
)

# =============================================================================
# Server
# =============================================================================

server <- function(input, output, session) {
  
  cleaned_text_value <- reactiveVal("")
  cleaning_summary_value <- reactiveVal("No check/clean action has been run yet.")
  detected_chars_value <- reactiveVal(
    data.frame(character = character(0), unicode = character(0), count = integer(0),
               suggested_action = character(0), stringsAsFactors = FALSE)
  )
  
  generated_chunks_value <- reactiveVal(NULL)  # list(sid=, raw=, wrapped=)
  current_index <- reactiveVal(1)
  autoplay <- reactiveVal(FALSE)
  
  start_sequence <- function(chunks_obj) {
    chunks_obj$images <- render_all_images(chunks_obj$wrapped, session)
    generated_chunks_value(chunks_obj)
    current_index(1)
    autoplay(FALSE)
    updateTabsetPanel(session, "main_tabs", selected = "2. QR Sequence")
  }
  
  # ---- Live, non-blocking support report (informational only - never blocks Generate) ----
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
          strong("✓ Support check: "), "no unsupported characters detected. Safe to generate as-is.")
    } else {
      top <- head(detected, 5)
      div(style = "margin-top:10px; padding:10px; border-radius:6px; background:#fef5e7; color:#9c640c; font-size:13px;",
          strong(sprintf("⚠ Support check: %d unsupported character type(s) found (%d occurrence(s) total). ",
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
  })
  
  # ---- Generate (from raw text box) ----
  observeEvent(input$generate, {
    req(input$long_text)
    chunks_obj <- build_chunks(input$long_text, input$chunk_size)
    validate(need(!is.null(chunks_obj), "Nothing to encode - please paste some text first."))
    start_sequence(chunks_obj)
  })
  
  # ---- Generate from clean data ----
  observeEvent(input$generate_from_clean, {
    clean_txt <- input$clean_text_panel
    if (is.null(clean_txt) || !nzchar(clean_txt)) clean_txt <- cleaned_text_value()
    
    validate(need(nzchar(clean_txt), "Clean data is empty. Please click 'Clean data for QR supports' first."))
    
    updateTextAreaInput(session, "long_text", value = clean_txt)
    cleaned_text_value(clean_txt)
    
    chunks_obj <- build_chunks(clean_txt, input$chunk_size)
    validate(need(!is.null(chunks_obj), "Nothing to encode."))
    start_sequence(chunks_obj)
  })
  
  # ---- Carousel navigation ----
  observeEvent(input$play_btn, { req(generated_chunks_value()); autoplay(TRUE) })
  observeEvent(input$pause_btn, { autoplay(FALSE) })
  
  observeEvent(input$prev_btn, {
    gc <- generated_chunks_value(); req(gc)
    n <- length(gc$wrapped)
    i <- current_index() - 1
    current_index(if (i < 1) n else i)
  })
  
  observeEvent(input$next_btn, {
    gc <- generated_chunks_value(); req(gc)
    n <- length(gc$wrapped)
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
      n <- length(gc$wrapped)
      if (n <= 1) return()
      i <- current_index() + 1
      if (i > n) {
        if (isTRUE(input$loop_toggle)) {
          i <- 1
        } else {
          i <- n
          autoplay(FALSE)
        }
      }
      current_index(i)
    })
  })
  
  # ---- Carousel outputs ----
  output$carousel_title <- renderText({
    gc <- generated_chunks_value()
    req(gc)
    n <- length(gc$wrapped)
    sprintf("Part %d of %d   |   Session ID: %s   |   %s", current_index(), n, gc$sid,
            if (isTRUE(autoplay())) "Playing" else "Paused")
  })
  
  output$carousel_progress_bar <- renderUI({
    gc <- generated_chunks_value()
    req(gc)
    n <- length(gc$wrapped)
    pct <- round(100 * current_index() / n)
    div(style = sprintf("background:#2ecc71;height:12px;width:%d%%;transition:width 0.3s;", pct))
  })
  
  output$carousel_segment_label <- renderText({
    gc <- generated_chunks_value()
    req(gc)
    n <- length(gc$wrapped)
    sprintf("Segment %d of %d", current_index(), n)
  })
  
  output$carousel_payload_label <- renderText({
    gc <- generated_chunks_value()
    req(gc)
    idx <- current_index()
    sprintf("Payload length: %d chars", nchar(gc$wrapped[idx], type = "chars"))
  })
  
  output$carousel_caption_label <- renderText({
    req(generated_chunks_value())
    caption_text
  })
  
  output$carousel_qr_plot <- renderUI({
    gc <- generated_chunks_value()
    req(gc)
    idx <- current_index()
    # Served from the pre-rendered cache built in start_sequence() - this is
    # just an <img> swap, so there's no recompute/dim flash between codes.
    tags$img(
      src = gc$images[[idx]],
      style = "max-width:100%; height:auto; display:block; margin:0 auto;"
    )
  })
  
  # ---- Cleanser outputs ----
  output$cleaning_summary <- renderText({ cleaning_summary_value() })
  output$detected_table <- renderTable({ detected_chars_value() })
}

shinyApp(ui = ui, server = server)



