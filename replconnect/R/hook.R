# Change tracking: a top-level task callback runs after every complete input
# the user evaluates at the prompt. R has no hook before evaluation starts, so
# the session version is bumped *after* each input (see docs/protocol.md).
# The callback does nothing else: no workspace scan, only a counter bump and
# a bounded input history.

TASK_CALLBACK <- "replconnect"

#' Called by R after each top-level task; must return TRUE to stay registered.
#' @noRd
task_callback <- function(expr, value, ok, visible) {
  tryCatch(
    {
      rs_bump_version()
      record_input(expr)
    },
    error = function(e) NULL
  )
  tryCatch(capture_new_plot(), error = function(e) NULL)
  TRUE
}

# Idempotent.
install_hook <- function() {
  if (!TASK_CALLBACK %in% getTaskCallbackNames()) {
    addTaskCallback(task_callback, name = TASK_CALLBACK)
  }
  invisible(NULL)
}

remove_hook <- function() {
  if (TASK_CALLBACK %in% getTaskCallbackNames()) {
    removeTaskCallback(TASK_CALLBACK)
  }
  invisible(NULL)
}
