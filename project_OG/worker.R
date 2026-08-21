# =============================================================================
# worker.R
# Background process entry point. Spawned by ensure_worker_running() via
# callr::r_bg() and NOT sourced directly by the Shiny app process.
#
# Deliberately dumb: no Shiny, no reactivity, just an infinite poll loop.
# That's what makes it safe to run detached from any browser session - it
# keeps working whether or not the app's tab is open, and picks back up
# after a machine restart because all state lives on disk (manifest.json),
# not in this process's memory.
# =============================================================================

source("pipeline_helpers.R")

POLL_IDLE_SECONDS  <- 2   # how long to sleep when there's nothing to do
POLL_ERROR_SECONDS <- 2   # backoff after a render failure, to avoid a hot loop

message(sprintf("[worker] started, pid=%d, watching %s / %s",
                 Sys.getpid(), TASK_DIR, LIBRARY_DIR))

repeat {
  task <- tryCatch(claim_next_task(), error = function(e) {
    message("[worker] claim_next_task() error: ", conditionMessage(e))
    NULL
  })

  if (is.null(task)) {
    Sys.sleep(POLL_IDLE_SECONDS)
    next
  }

  if (identical(task$status, "error")) {
    # init_manifest_from_source() already marked this unrenderable (e.g.
    # empty text after chunking) - nothing more to do, don't retry forever.
    message(sprintf("[worker] '%s' marked error, skipping", task$name %||% "?"))
    Sys.sleep(POLL_ERROR_SECONDS)
    next
  }

  ok <- tryCatch(render_next_chunk_for(task), error = function(e) {
    message(sprintf("[worker] render_next_chunk_for('%s') error: %s",
                     task$name %||% "?", conditionMessage(e)))
    FALSE
  })

  if (identical(task$status, "in_progress")) {
    done_now <- length(unlist(task$completed_chunks)) + 1
    message(sprintf("[worker] '%s' chunk %d/%s rendered=%s",
                     task$name %||% "?", done_now,
                     task$total_chunks %||% "?", ok))
  }

  if (!ok) {
    Sys.sleep(POLL_ERROR_SECONDS)
  }

  # No sleep on the success path - render the next chunk of the current
  # task immediately, so a big document finishes as fast as the machine
  # can render, rather than pacing at one chunk per POLL_IDLE_SECONDS.
}
