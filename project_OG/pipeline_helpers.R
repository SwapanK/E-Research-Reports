# =============================================================================
# pipeline_helpers.R
# Shared by app.R (the Shiny UI) and worker.R (the background renderer).
# Everything here is pure file/data logic - no Shiny reactivity lives in this
# file, so it's safe to source from a plain Rscript background process too.
# =============================================================================

suppressPackageStartupMessages({
  library(qrcode)
  library(ggplot2)
  library(base64enc)
  library(jsonlite)
  library(digest)
  library(callr)   # only used inside ensure_worker_running(), declared here
                    # so it shows up as a real dependency, not a surprise
})

# -----------------------------------------------------------------------
# Storage layout
#   store/Task/                 <- inbox: queued jsons, FIFO by mtime
#   store/Library/<name>/source.json        <- original text + settings
#   store/Library/<name>/manifest.json      <- progress / status
#   store/Library/<name>/chunks_meta.rds    <- precomputed split: sid + list of
#                                               per-chunk list(raw=, wrapped=)
#   store/Library/<name>/chunks/000123.rds  <- one self-contained chunk unit,
#                                               list(raw=, wrapped=, image=)
#                                               (deleted on finish)
#   store/Library/<name>/stream.rds         <- final assembled stream: sid/name
#                                               + list of list(raw=,wrapped=,image=)
#                                               (only when complete)
#   store/.worker.lock                      <- {pid, started_at} of the live background worker
# -----------------------------------------------------------------------

STORE_DIR   <- "store"
TASK_DIR    <- file.path(STORE_DIR, "Task")
LIBRARY_DIR <- file.path(STORE_DIR, "Library")
LOCK_FILE   <- file.path(STORE_DIR, ".worker.lock")

dir.create(TASK_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LIBRARY_DIR, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# Original QR chunking / rendering helpers (unchanged behavior)
# =============================================================================

detect_qr_unsafe_chars <- function(x) {
  x <- enc2utf8(x)

  if (is.null(x) || !nzchar(x)) {
    return(data.frame(
      character = character(0), unicode = character(0),
      count = integer(0), suggested_action = character(0),
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
      character = character(0), unicode = character(0),
      count = integer(0), suggested_action = character(0),
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

split_text_to_chunks <- function(txt, size) {
  if (is.null(txt) || !nzchar(txt)) return(character(0))
  starts <- seq(1, nchar(txt), by = size)
  chunks <- sapply(starts, function(s) substring(txt, s, s + size - 1))
  chunks[nzchar(trimws(chunks))]
}

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

# Builds the header-wrapped payloads (what's actually encoded into each QR
# code) alongside the original raw text for each chunk. All chunks in one
# call share one session ID (sid) so the receiver can group them together.
# `sid_override` lets queued/library tasks use a deterministic sid derived
# from their content uid, instead of a fresh random one every rebuild.
#
# Each chunk is a single self-contained unit - list(raw =, wrapped =) - kept
# together in one list rather than as parallel raw/wrapped vectors, so a
# chunk's text travels with it through rendering and storage instead of
# being reassembled from two lists by matching index.
build_chunks <- function(txt, chunk_size, sid_override = NULL) {
  effective_size <- max(50, chunk_size - HEADER_OVERHEAD)
  raw <- split_text_to_chunks(txt, effective_size)
  n <- length(raw)
  if (n == 0) return(NULL)
  sid <- if (!is.null(sid_override)) sid_override else generate_sid()
  wrapped <- vapply(seq_along(raw), function(i) wrap_chunk(sid, i, n, raw[i]), character(1))
  chunks <- lapply(seq_len(n), function(i) list(raw = raw[i], wrapped = wrapped[i]))
  list(sid = sid, chunks = chunks)
}

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

      if (inherits(result, "ggplot")) {
        print(
          result +
            theme_void() +
            theme(plot.margin = margin(0, 0, 0, 0))
        )
      }
    }
  }, error = function(e) {
    render_ok <<- FALSE
    render_err <<- conditionMessage(e)
  }, finally = {
    grDevices::dev.off()
  })

  file_size <- if (file.exists(tmpfile)) file.info(tmpfile)$size else 0

  if (!render_ok || is.na(file_size) || file_size < 700) {
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

  raw <- readBin(tmpfile, "raw", file_size)
  unlink(tmpfile)
  paste0("data:image/png;base64,", base64enc::base64encode(raw))
}

# Takes the list of list(raw=, wrapped=) chunk units from build_chunks()
# and returns each one with its rendered image attached - list(raw=,
# wrapped=, image=) - so text and image stay paired as a single unit per
# chunk, all the way through to the carousel.
render_all_images <- function(chunks, session = NULL, progress_fn = NULL) {
  n <- length(chunks)
  out <- vector("list", n)
  for (i in seq_len(n)) {
    img <- render_chunk_image(chunks[[i]]$wrapped, i, n)
    out[[i]] <- list(raw = chunks[[i]]$raw, wrapped = chunks[[i]]$wrapped, image = img)
    if (!is.null(progress_fn)) progress_fn(i, n)
  }
  out
}

# =============================================================================
# Atomic write helpers - always write-to-temp-then-rename, so a crash mid
# write never leaves a half-written file that looks valid.
# =============================================================================

atomic_write_json <- function(obj, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid(), "_", as.integer(Sys.time()))
  jsonlite::write_json(obj, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  file.rename(tmp, path)
}

atomic_write_rds <- function(obj, path) {
  tmp <- paste0(path, ".tmp", Sys.getpid(), "_", as.integer(Sys.time()))
  saveRDS(obj, tmp)
  file.rename(tmp, path)
}

read_json_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
}

read_rds_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

# =============================================================================
# Naming / identity
# =============================================================================

content_uid <- function(text, chunk_size) {
  substr(digest::digest(paste0(text, "|", chunk_size), algo = "sha1"), 1, 12)
}

default_task_name <- function() {
  paste0("task_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

sanitize_name <- function(name) {
  name <- trimws(name %||% "")
  if (!nzchar(name)) name <- default_task_name()
  name <- gsub("[^A-Za-z0-9_\\-]+", "_", name)
  name
}

unique_library_name <- function(name) {
  base <- name
  i <- 1
  while (dir.exists(file.path(LIBRARY_DIR, name)) || file.exists(file.path(TASK_DIR, paste0(name, ".json")))) {
    i <- i + 1
    name <- paste0(base, "_", i)
  }
  name
}

# =============================================================================
# Path helpers for a given task name
# =============================================================================

lib_dir          <- function(name) file.path(LIBRARY_DIR, name)
source_path      <- function(name) file.path(lib_dir(name), "source.json")
manifest_path    <- function(name) file.path(lib_dir(name), "manifest.json")
chunks_meta_path <- function(name) file.path(lib_dir(name), "chunks_meta.rds")
chunks_dir       <- function(name) file.path(lib_dir(name), "chunks")
stream_path      <- function(name) file.path(lib_dir(name), "stream.rds")

read_manifest <- function(name) read_json_safe(manifest_path(name))
write_manifest <- function(name, manifest) atomic_write_json(manifest, manifest_path(name))

# =============================================================================
# Queueing - called from the Shiny app (Add to Queue / upload json)
# =============================================================================

queue_task <- function(text, chunk_size, name = NULL, source_file = NA_character_) {
  nm <- unique_library_name(sanitize_name(name))
  uid <- content_uid(text, chunk_size)
  payload <- list(
    name = nm, uid = uid, text = text, chunk_size = chunk_size,
    source_file = source_file, created_at = as.character(Sys.time())
  )
  atomic_write_json(payload, file.path(TASK_DIR, paste0(nm, ".json")))
  nm
}

list_task_queue <- function() {
  files <- list.files(TASK_DIR, pattern = "\\.json$", full.names = TRUE)
  if (!length(files)) return(character(0))
  files[order(file.info(files)$mtime)]
}

# =============================================================================
# Resume-by-reload: match an uploaded json's content-uid against the Library
# =============================================================================

# Scans every Library manifest for one whose uid matches. Returns the
# matching task name, or NA_character_ if nothing matches (i.e. this is new
# content and should be queued fresh - see Scenario 6 in the plan).
find_library_match_by_uid <- function(uid) {
  if (is.null(uid) || is.na(uid) || !nzchar(uid)) return(NA_character_)
  nms <- list.dirs(LIBRARY_DIR, full.names = FALSE, recursive = FALSE)
  nms <- nms[nzchar(nms)]
  for (n in nms) {
    m <- read_manifest(n)
    if (!is.null(m) && identical(m$uid, uid)) return(n)
  }
  NA_character_
}

# =============================================================================
# Worker-side: claim + render one chunk at a time
# =============================================================================

# Turns a freshly-claimed source.json into a manifest + precomputed chunk
# split. Safe to call again on an already-initialized task (idempotent) -
# this is what makes the pipeline self-healing if the process dies between
# claiming a task and finishing its manifest.
init_manifest_from_source <- function(nm) {
  payload <- read_json_safe(source_path(nm))
  if (is.null(payload) || is.null(payload$text)) {
    manifest <- list(name = nm, uid = NA, sid = substr(nm, 1, 6), total_chunks = 0,
                      completed_chunks = list(), status = "error",
                      created_at = as.character(Sys.time()), chunk_size = NA, source_file = NA)
    write_manifest(nm, manifest)
    return(manifest)
  }

  uid <- payload$uid %||% content_uid(payload$text, payload$chunk_size)
  sid <- substr(uid, 1, 6)
  chunks_obj <- build_chunks(payload$text, payload$chunk_size, sid_override = sid)
  total <- if (is.null(chunks_obj)) 0 else length(chunks_obj$chunks)

  if (total > 0) {
    atomic_write_rds(list(sid = chunks_obj$sid, chunks = chunks_obj$chunks),
                      chunks_meta_path(nm))
    dir.create(chunks_dir(nm), recursive = TRUE, showWarnings = FALSE)
  }

  manifest <- list(
    name = nm, uid = uid, sid = sid,
    total_chunks = total, completed_chunks = list(),
    status = if (total == 0) "error" else "in_progress",
    created_at = payload$created_at %||% as.character(Sys.time()),
    chunk_size = payload$chunk_size, source_file = payload$source_file %||% NA
  )
  write_manifest(nm, manifest)
  manifest
}

# Returns the manifest of the task the worker should work on next, or NULL
# if there's nothing to do. Resuming always takes priority over starting a
# new task, so an interrupted job gets finished before the queue advances.
claim_next_task <- function() {
  nms <- list.dirs(LIBRARY_DIR, full.names = FALSE, recursive = FALSE)
  nms <- nms[nzchar(nms)]

  candidates <- list()
  for (n in nms) {
    if (!file.exists(source_path(n))) next   # deleted out from under us - skip
    m <- read_manifest(n)
    if (is.null(m)) m <- init_manifest_from_source(n)  # self-heal orphaned claim
    if (identical(m$status, "in_progress")) candidates[[length(candidates) + 1]] <- m
  }
  if (length(candidates)) {
    ord <- order(vapply(candidates, function(m) m$created_at %||% "", character(1)))
    return(candidates[[ord[1]]])
  }

  queue <- list_task_queue()
  if (!length(queue)) return(NULL)
  f <- queue[1]
  payload <- read_json_safe(f)
  if (is.null(payload) || is.null(payload$text)) { unlink(f); return(NULL) }

  nm <- payload$name %||% tools::file_path_sans_ext(basename(f))
  dir.create(lib_dir(nm), recursive = TRUE, showWarnings = FALSE)
  moved <- file.rename(f, source_path(nm))  # atomic hand-off out of Task/
  if (!isTRUE(moved)) return(NULL)

  init_manifest_from_source(nm)
}

# Renders exactly one more chunk for the given (already-claimed) task and
# checkpoints progress. Returns TRUE on success/no-op, FALSE on a hard error
# (e.g. the task's folder was deleted out from under the worker mid-render -
# this is the deletion-mid-render guard from Phase 6 / gap #2).
render_next_chunk_for <- function(manifest) {
  nm <- manifest$name
  if (!dir.exists(lib_dir(nm))) return(FALSE)  # deleted since we claimed it

  cm <- tryCatch(readRDS(chunks_meta_path(nm)), error = function(e) NULL)
  if (is.null(cm)) return(FALSE)

  done <- unlist(manifest$completed_chunks)
  total <- length(cm$chunks)
  idx <- length(done) + 1

  if (idx > total) {
    if (!identical(manifest$status, "complete")) {
      manifest$status <- "complete"
      write_manifest(nm, manifest)
    }
    return(TRUE)
  }

  chunk_meta <- cm$chunks[[idx]]
  img <- render_chunk_image(chunk_meta$wrapped, idx, total)
  chunk_unit <- list(raw = chunk_meta$raw, wrapped = chunk_meta$wrapped, image = img)

  if (!dir.exists(lib_dir(nm))) return(FALSE)  # deleted mid-render - don't resurrect the folder
  atomic_write_rds(chunk_unit, file.path(chunks_dir(nm), sprintf("%06d.rds", idx)))

  manifest$completed_chunks <- as.list(c(done, idx))

  if (idx == total) {
    chunks_rendered <- lapply(seq_len(total), function(i) readRDS(file.path(chunks_dir(nm), sprintf("%06d.rds", i))))
    stream_obj <- list(sid = cm$sid, name = nm, chunks = chunks_rendered)
    atomic_write_rds(stream_obj, stream_path(nm))
    manifest$status <- "complete"
    unlink(chunks_dir(nm), recursive = TRUE)
  }

  write_manifest(nm, manifest)
  TRUE
}

# =============================================================================
# App-side: read-only summary of everything in the pipeline, for the Library tab
# =============================================================================

# Status rank used to bubble active work to the top of the Library view:
# in_progress and queued (things you're watching) above complete/error/done.
# (This closes "known gap #3" from the original plan.)
.LIBRARY_STATUS_RANK <- c(in_progress = 0L, queued = 1L, error = 2L, complete = 3L)

list_library_index <- function() {
  rows <- list()

  lib_names <- list.dirs(LIBRARY_DIR, full.names = FALSE, recursive = FALSE)
  lib_names <- lib_names[nzchar(lib_names)]
  for (n in lib_names) {
    m <- read_manifest(n)
    if (is.null(m)) {
      rows[[length(rows) + 1]] <- data.frame(
        name = n, uid = NA_character_, status = "queued",
        completed = 0L, total = NA_integer_, created_at = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      rows[[length(rows) + 1]] <- data.frame(
        name = n, uid = m$uid %||% NA_character_, status = m$status %||% "unknown",
        completed = length(unlist(m$completed_chunks)), total = m$total_chunks %||% NA_integer_,
        created_at = m$created_at %||% NA_character_, stringsAsFactors = FALSE
      )
    }
  }

  queued_files <- list_task_queue()
  for (f in queued_files) {
    nm <- tools::file_path_sans_ext(basename(f))
    if (nm %in% lib_names) next  # already claimed, showing above
    rows[[length(rows) + 1]] <- data.frame(
      name = nm, uid = NA_character_, status = "queued",
      completed = 0L, total = NA_integer_, created_at = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) {
    return(data.frame(name = character(0), uid = character(0), status = character(0),
                       completed = integer(0), total = integer(0), created_at = character(0),
                       stringsAsFactors = FALSE))
  }

  df <- do.call(rbind, rows)

  # Active work (in_progress/queued) bubbles to the top; within a status,
  # newest first. NA created_at (orphaned queue entries with no manifest
  # yet) sorts last within its status band rather than blowing up the sort.
  status_rank <- .LIBRARY_STATUS_RANK[df$status]
  status_rank[is.na(status_rank)] <- max(.LIBRARY_STATUS_RANK) + 1L
  created_key <- ifelse(is.na(df$created_at), "", df$created_at)
  ord <- order(status_rank, xtfrm(created_key), decreasing = c(FALSE, TRUE), method = "radix")
  df[ord, , drop = FALSE]
}

delete_task <- function(name) {
  unlink(lib_dir(name), recursive = TRUE)
  tf <- file.path(TASK_DIR, paste0(name, ".json"))
  if (file.exists(tf)) unlink(tf)
  invisible(TRUE)
}

# =============================================================================
# Background worker lifecycle
# =============================================================================

# Cross-platform "is this pid actually alive" check.
#
# The original implementation shelled out to the Unix `kill -0 <pid>`
# command. On Windows there is no `kill` on PATH at all, so system2() failed
# with '"kill"' not found - which the tryCatch turned into "not alive" for
# EVERY pid, on EVERY check. That's why tasks sat at "queued" forever on
# Windows: ensure_worker_running() saw a lock file, believed the process
# behind it was dead... actually worse, it believed a genuinely running
# worker was dead too, since the check itself errored, but the confusing
# part is it *did* still spawn a new worker in that case - the real forever-
# queued symptom traces back to this same broken liveness check feeding
# false info to the UI, not to the spawn logic itself. Either way, a
# platform-correct check removes the ambiguity entirely.
#
# Preferred path: the `ps` package (a transitive dependency of callr/
# processx, so it's normally already installed) exposes ps_pid_exists(),
# which works identically on Windows/macOS/Linux. Fall back to a shelled-out
# platform-specific check only if `ps` isn't available.
pid_alive <- function(pid) {
  pid <- suppressWarnings(as.integer(pid))
  if (length(pid) == 0 || is.na(pid)) return(FALSE)

  if (requireNamespace("ps", quietly = TRUE)) {
    alive <- tryCatch(ps::ps_pid_exists(pid), error = function(e) NA)
    if (!is.na(alive)) return(isTRUE(alive))
  }

  if (.Platform$OS.type == "windows") {
    out <- tryCatch(
      system2("tasklist", c("/FI", shQuote(sprintf("PID eq %d", pid)), "/NH", "/FO", "CSV"),
              stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    return(any(grepl(sprintf(',"%d",', pid), out, fixed = TRUE)) ||
             any(grepl(sprintf('"%d"', pid), out, fixed = TRUE)))
  }

  res <- tryCatch(
    system2("kill", c("-0", as.character(pid)), stdout = FALSE, stderr = FALSE),
    error = function(e) 1L
  )
  identical(res, 0L)
}

# Starts the background worker if one isn't already running, and returns a
# status list the UI can act on immediately instead of inferring health from
# tasks silently sitting in "queued":
#   list(started = <did we just launch a new process>,
#        alive   = <is a worker process alive right now>,
#        pid     = <its pid, or NA>,
#        message = <human-readable status/diagnostic>)
ensure_worker_running <- function() {
  if (file.exists(LOCK_FILE)) {
    info <- read_json_safe(LOCK_FILE)
    if (!is.null(info) && pid_alive(info$pid)) {
      return(list(started = FALSE, alive = TRUE, pid = info$pid,
                   message = sprintf("Worker already running (pid %s).", info$pid)))
    }
    unlink(LOCK_FILE)  # stale lock from a dead process
  }

  app_dir <- getwd()
  out_log <- file.path(STORE_DIR, "worker_stdout.log")
  err_log <- file.path(STORE_DIR, "worker_stderr.log")
  # Clear old logs so a stale error from a previous run of the app isn't
  # mistaken for a failure of *this* launch attempt.
  try(unlink(c(out_log, err_log)), silent = TRUE)

  px <- tryCatch(
    callr::r_bg(
      function(app_dir) { setwd(app_dir); source("worker.R", local = TRUE) },
      args = list(app_dir = app_dir),
      stdout = out_log,
      stderr = err_log,
      supervise = TRUE
    ),
    error = function(e) e
  )

  if (inherits(px, "error")) {
    return(list(started = FALSE, alive = FALSE, pid = NA,
                 message = sprintf("Failed to launch background worker: %s", conditionMessage(px))))
  }

  # Give it a moment to fail fast (missing package, source() typo, etc.)
  # before declaring success, so a worker that dies on startup is reported
  # right away instead of leaving every task queued with no explanation.
  Sys.sleep(0.6)
  if (!px$is_alive()) {
    err_tail <- if (file.exists(err_log)) {
      tryCatch(paste(utils::tail(readLines(err_log, warn = FALSE), 10), collapse = " / "),
                error = function(e) "")
    } else ""
    return(list(
      started = TRUE, alive = FALSE, pid = px$get_pid(),
      message = paste0(
        "Background worker exited immediately after starting.",
        if (nzchar(err_tail)) paste0(" Last error: ", err_tail) else " Check store/worker_stderr.log for details."
      )
    ))
  }

  atomic_write_json(list(pid = px$get_pid(), started_at = as.character(Sys.time())), LOCK_FILE)
  list(started = TRUE, alive = TRUE, pid = px$get_pid(),
       message = sprintf("Background worker started (pid %s).", px$get_pid()))
}
