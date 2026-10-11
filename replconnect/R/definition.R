#
# `definition`: where a function variable was defined, for `v` and `gd` in the
# editor's variables pane (docs/definition-hook.md in REPLConnect.nvim).
# Read-only: uses each function's srcref, else its deparsed body. Plain
# functions have one location; S4 generics have one per method.
#

# Most locations to return for one request (large generics such as `show`).
MAX_DEFINITIONS <- 200L

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

# The plain closure inside S4 function objects (methods, generics).
plain_function <- function(f) {
  if (isS4(f) && methods::is(f, "function")) f@.Data else f
}

deparsed_source <- function(f) {
  paste(deparse(plain_function(f)), collapse = "\n")
}

has_srcref <- function(f) {
  !is.primitive(f) && !is.null(utils::getSrcref(f))
}

# The location of function `f`, labelled `signature`.
location_at <- function(f, signature) {
  loc <- list(signature = signature)
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

# A location that cannot fail: falls back to the deparsed source.
safe_location <- function(f, signature) {
  tryCatch(
    location_at(f, signature),
    error = function(e) {
      list(
        signature = signature,
        source = tryCatch(deparsed_source(f), error = function(e) "")
      )
    }
  )
}

# Locations of `methods` (functions named by their labels), user code (with
# srcref) first, at most MAX_DEFINITIONS.
ordered_locations <- function(methods) {
  user <- vapply(
    methods,
    function(m) tryCatch(has_srcref(m), error = function(e) FALSE),
    NA
  )
  methods <- methods[order(!user)]
  methods <- utils::head(methods, MAX_DEFINITIONS)
  unname(Map(safe_location, methods, names(methods)))
}

#
# S4
#

is_s4_generic <- function(f) methods::is(f, "genericFunction")

# `area(shape = "Circle")` from a method's defined signature.
s4_signature <- function(method, name) {
  defined <- method@defined
  args <- paste0(names(defined), " = \"", as.character(defined), "\"")
  paste0(name, "(", paste(args, collapse = ", "), ")")
}

# The generic's own (non-inherited) methods, labelled by signature.
s4_methods <- function(generic, name) {
  found <- methods::findMethods(generic, inherited = FALSE)
  methods <- found@.Data
  names(methods) <- vapply(methods, s4_signature, "", name = name)
  methods
}

#
# Request
#

# Every location of function `f`: one per method for generics with methods.
function_locations <- function(f, name) {
  methods <- if (is_s4_generic(f)) {
    tryCatch(s4_methods(f, name), error = function(e) list())
  }
  if (length(methods) > 0L) {
    return(ordered_locations(methods))
  }
  signature <- tryCatch(
    definition_signature(f, name),
    error = function(e) paste0(name, "()")
  )
  list(safe_location(f, signature))
}

definition <- function(path, env) {
  f <- resolve(path, env)
  name <- path[[length(path)]]
  if (!is.function(f)) {
    rpc_stop(INVALID_PARAMS, paste0("Not a function: ", name))
  }
  list(locations = I(function_locations(f, name)))
}

handle_definition <- function(params, connection) {
  definition(path_param(params), workspace_env())
}
