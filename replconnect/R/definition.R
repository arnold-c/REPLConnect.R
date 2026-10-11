#
# `definition`: where a function variable was defined, for `v` and `gd` in the
# editor's variables pane (docs/definition-hook.md in REPLConnect.nvim).
# Read-only: uses the function's srcref, else its deparsed body. R has no
# multiple dispatch, so there is exactly one location.
#

# `name(arg1, arg2)`; primitives have no formals of their own, so use `args()`.
definition_signature <- function(f, name) {
  fmls <- formals(if (is.primitive(f)) args(f) else f)
  paste0(name, "(", paste(names(fmls), collapse = ", "), ")")
}

# The srcfile's file as an absolute path, or NULL when it is not a readable
# file (typed at the prompt, `eval(parse(text = ))`).
srcfile_path <- function(srcfile) {
  file <- srcfile$filename
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    return(NULL)
  }
  if (!grepl("^(/|~|[A-Za-z]:)", file) && !is.null(srcfile$wd)) {
    file <- file.path(srcfile$wd, file)
  }
  if (file.exists(file) && !dir.exists(file)) normalizePath(file) else NULL
}

deparsed_source <- function(f) paste(deparse(f), collapse = "\n")

# The one location of function `f`, named `name`.
definition_location <- function(f, name) {
  loc <- list(signature = definition_signature(f, name))
  sr <- if (is.primitive(f)) NULL else utils::getSrcref(f)
  if (is.null(sr)) {
    # Packages and keep.source = FALSE: no location, deparsed source.
    loc$source <- deparsed_source(f)
    return(loc)
  }
  srcfile <- attr(sr, "srcfile")
  loc$line <- sr[[1L]]
  loc$source <- paste(as.character(sr, useSource = TRUE), collapse = "\n")
  file <- srcfile_path(srcfile)
  if (is.null(file)) {
    loc$file <- "<console>"
  } else {
    loc$file <- file
    if (inherits(srcfile$timestamp, "POSIXct")) {
      loc$mtime <- as.numeric(srcfile$timestamp)
    }
  }
  loc
}

definition <- function(path, env) {
  f <- resolve(path, env)
  if (!is.function(f)) {
    rpc_stop(INVALID_PARAMS, paste0("Not a function: ", path[[length(path)]]))
  }
  name <- path[[length(path)]]
  location <- tryCatch(
    definition_location(f, name),
    error = function(e) {
      list(signature = paste0(name, "()"), source = deparsed_source(f))
    }
  )
  list(locations = I(list(location)))
}

handle_definition <- function(params, connection) {
  definition(path_param(params), workspace_env())
}
