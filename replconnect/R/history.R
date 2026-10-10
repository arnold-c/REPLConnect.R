# Session-only input history for the editor's session picker. This is deparsed
# top-level input, not arf's persistent history database. Comments and original
# whitespace are not retained, and requests handled through R_tryEval are not
# recorded as top-level console inputs.

HISTORY_LIMIT <- 500L
HISTORY_TEXT_LIMIT <- 2000L
HISTORY_DEFAULT_LIMIT <- 10L

recording_history <- function() {
  !identical(.state$record_history, FALSE)
}

#' Enable or disable session input history
#'
#' Recording is on by default. Disabling it forgets retained inputs and removes
#' the history capability; clients are notified when the setting changes.
#' Change tracking continues even while history recording is disabled.
#' @param enabled A single TRUE or FALSE.
#' @return The new recording setting, invisibly.
#' @export
record_history <- function(enabled = TRUE) {
  if (!is.logical(enabled) || length(enabled) != 1L || is.na(enabled)) {
    stop("enabled must be TRUE or FALSE")
  }
  changed <- recording_history() != enabled
  .state$record_history <- enabled
  if (!enabled) {
    .state$history <- NULL
  }
  if (changed) {
    announce_capabilities()
  }
  invisible(enabled)
}

record_input <- function(expr) {
  if (!recording_history()) {
    return(invisible(NULL))
  }
  # Bound the deparse as well as retained text. The callback never scans globals.
  input <- paste(
    deparse(expr, width.cutoff = 500L, nlines = 50L),
    collapse = "\n"
  )
  input <- ellipsize(input, HISTORY_TEXT_LIMIT)
  history <- c(.state$history, input)
  n <- length(history)
  if (n > HISTORY_LIMIT) {
    history <- history[(n - HISTORY_LIMIT + 1L):n]
  }
  .state$history <- history
  invisible(NULL)
}

# Most recent entries, returned oldest first. Always a JSON array, including
# when there is only one input or history recording is disabled.
handle_history <- function(params, connection) {
  limit <- params[["limit"]]
  if (is.null(limit)) {
    limit <- HISTORY_DEFAULT_LIMIT
  }
  if (
    !is.numeric(limit) ||
      length(limit) != 1L ||
      !is.finite(limit) ||
      limit < 0 ||
      limit != floor(limit)
  ) {
    rpc_stop(INVALID_PARAMS, "limit must be a nonnegative integer")
  }
  history <- if (recording_history()) .state$history else character()
  recent <- utils::tail(history, min(limit, length(history)))
  list(
    entries = unname(lapply(as.list(recent), function(input) {
      list(input = input)
    }))
  )
}
