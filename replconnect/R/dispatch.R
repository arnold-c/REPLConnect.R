# Request dispatch, called by the Rust transport on R's main thread as
# `dispatch(method, params, connection)`. Returns
# `list(ok = TRUE, result = ...)` or `list(ok = FALSE, code = ..., message = ...)`.
# JSON shapes: length-1 vectors are scalars, `I(x)` keeps arrays, named lists
# are objects (see src/rust/core/src/value.rs).

PARSE_ERROR <- -32700L
INVALID_REQUEST <- -32600L
METHOD_NOT_FOUND <- -32601L
INVALID_PARAMS <- -32602L
INTERNAL_ERROR <- -32603L
BUSY <- -32001L

#' Signal a JSON-RPC error from a handler.
#' @noRd
rpc_stop <- function(code, message) {
  stop(structure(
    class = c("replconnect_rpc_error", "error", "condition"),
    list(message = message, call = NULL, code = code)
  ))
}

# Method name -> function(params, connection).
handlers <- function() {
  list(
    initialize = handle_initialize,
    list = handle_list,
    changes = handle_changes,
    inspect = handle_inspect,
    viewer_page = handle_viewer_page,
    table_page = handle_table_page,
    clipboard_format = handle_clipboard_format,
    delete = handle_delete,
    clear = handle_clear,
    history = handle_history,
    complete = handle_complete,
    plots = handle_plots,
    plots_subscribe = handle_plots_subscribe
  )
}

#' Features this session offers.
#' @noRd
service_capabilities <- function() {
  features <- c("variables", "viewer", "mutations", "shutdown")
  if (recording_history()) {
    features <- c(features, "history")
  }
  if (capturing_plots()) {
    features <- c(features, "plots")
  }
  features
}

handle_initialize <- function(params, connection) {
  info <- rs_info()
  list(
    protocol_version = protocol_version(),
    language = "r",
    language_version = r_version(),
    pid = Sys.getpid(),
    started_at = if (is.null(info)) 0 else info$started_at,
    capabilities = I(service_capabilities())
  )
}

# Run a handler without leaking warnings or messages into the user's console.
quietly <- function(expr) {
  withCallingHandlers(
    expr,
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage")
  )
}

dispatch <- function(method, params, connection) {
  handler <- if (
    is.character(method) && length(method) == 1 && nzchar(method)
  ) {
    handlers()[[method]]
  }
  if (is.null(handler)) {
    return(list(
      ok = FALSE,
      code = METHOD_NOT_FOUND,
      message = paste0("Unknown method: ", method)
    ))
  }
  tryCatch(
    list(ok = TRUE, result = quietly(handler(params, connection))),
    replconnect_rpc_error = function(e) {
      list(ok = FALSE, code = e$code, message = conditionMessage(e))
    },
    error = function(e) {
      list(ok = FALSE, code = INTERNAL_ERROR, message = conditionMessage(e))
    }
  )
}
