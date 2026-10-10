# Deleting variables, refused while R is evaluating (see docs/protocol.md:
# mutations are refused with BUSY while a computation runs).

# Apply the same server-side visibility rule as list and clear. This does
# not depend on the editor's client-side exclusions.
delete_binding <- function(env, name) {
  if (!name %in% visible_names(env)) {
    return(FALSE)
  }
  rm(list = name, envir = env)
  TRUE
}

# Refuse a mutation while R is evaluating: a handler that runs from inside
# `Sys.sleep`, a `readline()` or a `browser()` inside a function must not
# mutate the workspace. The heuristic (and its limits, e.g. a top-level
# `browser()`) is documented in `docs/r-debugging.md`.
require_idle <- function() {
  if (rs_evaluating()) {
    rpc_stop(BUSY, "The REPL is evaluating")
  }
  invisible(NULL)
}

handle_delete <- function(params, connection) {
  require_idle()
  names <- params[["names"]]
  if (!is.list(names) && !is.character(names)) {
    rpc_stop(INVALID_PARAMS, "Missing `names`")
  }
  names <- vapply(as.list(names), function(name) as.character(name)[[1L]], "")
  env <- workspace_env()
  deleted <- names[vapply(
    names,
    function(name) {
      tryCatch(delete_binding(env, name), error = function(e) FALSE)
    },
    logical(1)
  )]
  rs_bump_version()
  I(deleted)
}

handle_clear <- function(params, connection) {
  require_idle()
  env <- workspace_env()
  for (name in visible_names(env)) {
    tryCatch(delete_binding(env, name), error = function(e) NULL)
  }
  rs_bump_version()
  NULL
}
