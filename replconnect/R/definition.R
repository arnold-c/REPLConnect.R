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
# S3
#

is_s3_generic <- function(f) {
  !is.primitive(f) && isTRUE(unname(utils::isS3stdGeneric(f)))
}

# Methods of S3 generic `generic` (named `key`) that have srcref, i.e. user
# code rather than the many package methods, labelled `print.foo(x, ...)`.
# The generic itself comes first when it has srcref too.
s3_methods <- function(generic, key, env) {
  found <- suppressWarnings(utils::methods(key))
  info <- attr(found, "info")
  classes <- substring(
    rownames(info)[info$generic == key],
    nchar(key) + 2L
  )
  fns <- lapply(classes, function(class) {
    utils::getS3method(key, class, optional = TRUE, envir = env)
  })
  names(fns) <- paste0(key, ".", classes)
  fns <- Filter(function(f) is.function(f) && has_srcref(f), fns)
  if (length(fns) == 0L) {
    return(list())
  }
  if (has_srcref(generic)) {
    fns <- c(stats::setNames(list(generic), key), fns)
  }
  names(fns) <- vapply(
    names(fns),
    function(name) definition_signature(fns[[name]], name),
    ""
  )
  fns
}

#
# R6
#

R6_METHOD_LISTS <- c("public_methods", "private_methods", "active")

is_r6_object <- function(x) inherits(x, "R6")

is_r6_generator <- function(x) inherits(x, "R6ClassGenerator")

# The name to label the function at `path` with: `owner$method` for methods
# of R6 objects (`obj$greet`) and generators
# (`Person$public_methods$greet` -> `Person$greet`), else the last key.
definition_name <- function(path, env) {
  n <- length(path)
  name <- path[[n]]
  owner <- function(k) {
    tryCatch(resolve(path[seq_len(k)], env), error = function(e) NULL)
  }
  if (n >= 2L && is_r6_object(owner(n - 1L))) {
    return(paste0(path[[n - 1L]], "$", name))
  }
  if (
    n >= 3L &&
      path[[n - 1L]] %in% R6_METHOD_LISTS &&
      is_r6_generator(owner(n - 2L))
  ) {
    return(paste0(path[[n - 2L]], "$", name))
  }
  name
}

# `fns` (named functions) labelled as `owner$name(args)`, without the `clone`
# method R6 adds to every class.
labelled_methods <- function(fns, owner) {
  fns <- Filter(is.function, fns[setdiff(names(fns), "clone")])
  if (length(fns) == 0L) {
    return(list())
  }
  names(fns) <- vapply(
    names(fns),
    function(name) definition_signature(fns[[name]], paste0(owner, "$", name)),
    ""
  )
  fns
}

# The functions bound in an R6 object's environment (no active bindings).
env_methods <- function(env) {
  names <- Filter(
    function(name) !bindingIsActive(name, env),
    ls(env, all.names = FALSE)
  )
  fns <- lapply(names, function(name) get(name, envir = env, inherits = FALSE))
  names(fns) <- names
  fns
}

# Methods of an R6 instance: public, private, then each `super` level.
r6_object_methods <- function(obj, label) {
  enclos <- obj$.__enclos_env__
  private <- get0("private", envir = enclos, inherits = FALSE)
  methods <- c(
    labelled_methods(env_methods(obj), label),
    if (is.environment(private)) {
      labelled_methods(env_methods(private), paste0(label, "$private"))
    }
  )
  owner <- label
  super <- get0("super", envir = enclos, inherits = FALSE)
  while (is.environment(super)) {
    owner <- paste0(owner, "$super")
    methods <- c(methods, labelled_methods(env_methods(super), owner))
    super_enclos <- super$.__enclos_env__
    super <- if (is.environment(super_enclos)) {
      get0("super", envir = super_enclos, inherits = FALSE)
    }
  }
  methods
}

# Methods of an R6 class generator and its ancestors, child first.
r6_generator_methods <- function(generator, label) {
  methods <- list()
  owner <- label
  while (is_r6_generator(generator)) {
    for (kind in R6_METHOD_LISTS) {
      prefix <- switch(
        kind,
        public_methods = owner,
        private_methods = paste0(owner, "$private"),
        active = paste0(owner, "$active")
      )
      methods <- c(methods, labelled_methods(generator[[kind]], prefix))
    }
    generator <- generator$get_inherit()
    owner <- if (is_r6_generator(generator) && !is.null(generator$classname)) {
      generator$classname
    } else {
      paste0(owner, "$parent")
    }
  }
  methods
}

# Locations of every method of an R6 generator or object.
r6_locations <- function(x, label) {
  methods <- tryCatch(
    if (is_r6_generator(x)) {
      r6_generator_methods(x, label)
    } else {
      r6_object_methods(x, label)
    },
    error = function(e) list()
  )
  ordered_locations(methods)
}

#
# Request
#

# Every location of function `f`, labelled `name` and bound to `key` in
# `env`: one per method for generics with methods.
function_locations <- function(f, name, key, env) {
  methods <- tryCatch(
    if (is_s4_generic(f)) {
      s4_methods(f, name)
    } else if (is_s3_generic(f)) {
      s3_methods(f, key, env)
    },
    error = function(e) list()
  )
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
  if (is_r6_generator(f) || is_r6_object(f)) {
    return(list(locations = I(r6_locations(f, path[[length(path)]]))))
  }
  if (!is.function(f)) {
    rpc_stop(INVALID_PARAMS, paste0("Not a function: ", path[[length(path)]]))
  }
  name <- definition_name(path, env)
  key <- path[[length(path)]]
  list(locations = I(function_locations(f, name, key, env)))
}

handle_definition <- function(params, connection) {
  definition(path_param(params), workspace_env())
}
