# Variables: description, inspection and change detection, using the
# protocol's Positron vocabulary (see docs/protocol.md and the Julia service).
# Nothing here evaluates user code: paths are resolved structurally, and active
# bindings are described without being read.

MAX_DISPLAY <- 100L
MAX_CHILDREN <- 100L
DISPLAY_ELEMENTS <- 10L
SMALL_VALUE <- 64L
ELLIPSIS <- "\u2026"

# The environment whose variables are inspected.
workspace_env <- function() globalenv()

# Stands for the value of an active binding, which is never read.
ACTIVE <- structure(list(), class = "replconnect_active")

now_ms <- function() round(as.numeric(Sys.time()) * 1000)

ellipsize <- function(s, n = MAX_DISPLAY) {
  if (nchar(s, type = "chars") > n) {
    paste0(substr(s, 1L, n - 1L), ELLIPSIS)
  } else {
    s
  }
}

#
# Per-type behaviour
#

# Positron kinds: boolean, bytes, class, collection, empty, function, map,
# number, other, string, table.
kind <- function(x) {
  if (is.null(x)) {
    "empty"
  } else if (is.data.frame(x)) {
    "table"
  } else if (is.function(x)) {
    "function"
  } else if (is.environment(x)) {
    if (inherits(x, "R6")) "class" else "map"
  } else if (is.list(x)) {
    if (is.null(names(x))) "collection" else "map"
  } else if (is.raw(x)) {
    "bytes"
  } else if (is.atomic(x)) {
    if (length(x) != 1L || !is.null(dim(x)) || is.factor(x)) {
      "collection"
    } else if (is.logical(x)) {
      "boolean"
    } else if (is.character(x)) {
      "string"
    } else if (is.numeric(x) || is.complex(x)) {
      "number"
    } else {
      "other"
    }
  } else {
    "other"
  }
}

env_names <- function(env) ls(env, all.names = FALSE, sorted = TRUE)

# Dimensions to show next to the type, or NULL for scalars and objects.
shape <- function(x) {
  if (is.data.frame(x)) {
    c(nrow(x), ncol(x))
  } else if (!is.null(dim(x))) {
    dim(x)
  } else if (is.function(x) || is.environment(x) || isS4(x) || is.null(x)) {
    NULL
  } else if ((is.atomic(x) || is.list(x)) && length(x) != 1L) {
    length(x)
  } else {
    NULL
  }
}

display_type <- function(x) {
  if (is.function(x)) {
    return("function")
  }
  label <- class(x)[[1L]]
  dims <- shape(x)
  if (is.null(dims)) {
    label
  } else {
    sprintf("%s [%s]", label, paste(dims, collapse = ", "))
  }
}

type_info <- function(x) paste(class(x), collapse = "/")

length_of <- function(x) {
  tryCatch(
    {
      if (is.data.frame(x)) {
        nrow(x)
      } else if (!is.null(dim(x))) {
        dim(x)[[1L]]
      } else if (is.environment(x)) {
        length(env_names(x))
      } else if (is.function(x) || isS4(x)) {
        0
      } else {
        length(x)
      }
    },
    error = function(e) 0
  )
}

# How many children `x` has (children() returns at most MAX_CHILDREN).
n_children <- function(x) {
  tryCatch(
    {
      if (is.environment(x)) {
        length(env_names(x))
      } else if (isS4(x)) {
        length(methods::slotNames(x))
      } else if (is.data.frame(x)) {
        ncol(x)
      } else if (is.list(x)) {
        length(x)
      } else if (
        is.atomic(x) &&
          !is.factor(x) &&
          is.null(dim(x)) &&
          length(x) > 1L
      ) {
        length(x)
      } else {
        0L
      }
    },
    error = function(e) 0L
  )
}

format_elements <- function(x) {
  shown <- utils::head(x, DISPLAY_ELEMENTS)
  if (is.character(shown)) {
    encodeString(shown, quote = '"')
  } else {
    format(shown, trim = TRUE)
  }
}

display_text <- function(x) {
  if (is.null(x)) {
    return("NULL")
  }
  if (is.data.frame(x)) {
    return(sprintf(
      "[%d rows x %d columns] %s",
      nrow(x),
      ncol(x),
      class(x)[[1L]]
    ))
  }
  if (is.function(x)) {
    label <- trimws(deparse(args(x))[[1L]])
    return(if (is.na(label) || !nzchar(label)) "function" else label)
  }
  if (is.environment(x)) {
    return(sprintf("<environment: %d objects>", length(env_names(x))))
  }
  if (isS4(x)) {
    return(sprintf("<S4 object of class \"%s\">", class(x)[[1L]]))
  }
  if (is.list(x)) {
    return(sprintf("list of %d", length(x)))
  }
  if (is.atomic(x)) {
    if (!is.null(dim(x))) {
      return(sprintf("<%s>", display_type(x)))
    }
    if (is.factor(x)) {
      return(sprintf("factor with %d levels", nlevels(x)))
    }
    if (length(x) == 0L) {
      return(sprintf("%s(0)", typeof(x)))
    }
    items <- format_elements(x)
    if (length(x) == 1L) {
      return(items)
    }
    more <- if (length(x) > DISPLAY_ELEMENTS) paste0(", ", ELLIPSIS) else ""
    return(paste0("c(", paste(items, collapse = ", "), more, ")"))
  }
  sprintf("<%s>", class(x)[[1L]])
}

display_value <- function(x) {
  tryCatch(
    ellipsize(display_text(x)),
    error = function(e) "<error displaying value>"
  )
}

byte_size <- function(x) {
  tryCatch(as.numeric(utils::object.size(x)), error = function(e) 0)
}

#
# Variables on the wire
#

variable <- function(key, value, time = now_ms()) {
  if (inherits(value, "replconnect_active")) {
    return(list(
      access_key = key,
      display_name = key,
      display_value = "<active binding>",
      display_type = "active binding",
      type_info = "active binding",
      size = 0,
      kind = "other",
      length = 0L,
      has_children = FALSE,
      has_viewer = FALSE,
      is_truncated = FALSE,
      updated_time = time
    ))
  }
  display <- display_value(value)
  list(
    access_key = key,
    display_name = key,
    display_value = display,
    display_type = display_type(value),
    type_info = type_info(value),
    size = byte_size(value),
    kind = kind(value),
    length = length_of(value),
    has_children = n_children(value) > 0L,
    has_viewer = FALSE,
    is_truncated = endsWith(display, ELLIPSIS),
    updated_time = time
  )
}

# `list(key, value)` pairs described as variables; failures are skipped.
describe_all <- function(pairs, time = now_ms()) {
  out <- lapply(pairs, function(pair) {
    tryCatch(variable(pair$key, pair$value, time), error = function(e) NULL)
  })
  unname(out[!vapply(out, is.null, NA)])
}

#
# Globals
#

visible_names <- function(env) ls(env, all.names = FALSE, sorted = TRUE)

get_binding <- function(env, name) {
  if (bindingIsActive(name, env)) {
    ACTIVE
  } else {
    get(name, envir = env, inherits = FALSE)
  }
}

global_pairs <- function(env, names = visible_names(env)) {
  pairs <- lapply(names, function(name) {
    tryCatch(
      list(key = name, value = get_binding(env, name)),
      error = function(e) NULL
    )
  })
  pairs[!vapply(pairs, is.null, NA)]
}

#
# Children and paths
#

children <- function(x) {
  if (is.environment(x)) {
    names <- utils::head(env_names(x), MAX_CHILDREN)
    return(global_pairs(x, names))
  }
  if (isS4(x)) {
    names <- utils::head(methods::slotNames(x), MAX_CHILDREN)
    return(lapply(names, function(name) {
      list(key = name, value = methods::slot(x, name))
    }))
  }
  n <- min(n_children(x), MAX_CHILDREN)
  if (n == 0L) {
    return(list())
  }
  names <- names(x)
  lapply(seq_len(n), function(i) {
    name <- if (is.null(names)) NA_character_ else names[[i]]
    key <- if (is.na(name) || !nzchar(name)) sprintf("[%d]", i) else name
    list(key = key, value = x[[i]])
  })
}

not_found <- function(key) {
  rpc_stop(INVALID_PARAMS, paste0("Variable not found: ", key))
}

child <- function(x, key) {
  if (is.environment(x)) {
    if (!exists(key, envir = x, inherits = FALSE)) {
      not_found(key)
    }
    return(get_binding(x, key))
  }
  if (isS4(x)) {
    if (!key %in% methods::slotNames(x)) {
      not_found(key)
    }
    return(methods::slot(x, key))
  }
  if (is.list(x) || (is.atomic(x) && !is.null(x))) {
    if (grepl("^\\[[0-9]+\\]$", key)) {
      i <- as.integer(substr(key, 2L, nchar(key) - 1L))
      if (i < 1L || i > length(x)) {
        not_found(key)
      }
      return(x[[i]])
    }
    i <- match(key, names(x))
    if (is.na(i)) {
      not_found(key)
    }
    return(x[[i]])
  }
  not_found(key)
}

# Resolve an access-key path (a variable name, then child keys) to a value.
resolve <- function(path, env) {
  if (length(path) == 0L) {
    rpc_stop(INVALID_PARAMS, "Missing `path`")
  }
  name <- path[[1L]]
  if (!exists(name, envir = env, inherits = FALSE)) {
    not_found(name)
  }
  value <- get_binding(env, name)
  for (key in path[-1L]) {
    value <- child(value, key)
  }
  value
}

path_param <- function(params) {
  path <- params[["path"]]
  if (!is.list(path) && !is.character(path)) {
    rpc_stop(INVALID_PARAMS, "Missing `path`")
  }
  vapply(as.list(path), function(key) as.character(key)[[1L]], "")
}

#
# Change detection: a cheap fingerprint per variable, so only variables that
# changed are described after each input. R has no object identity, so the
# fingerprint combines the object's memory address (a reassigned value is a
# new object), its type, class, length, dimensions and names, plus the value
# itself when small. Never calls show/print or object.size, and never touches
# elements of large objects.
#
# Detected: reassignment, a change of type or size, added or removed list
# elements or columns, any change to small atomic values.
# Missed: in-place changes to a large object that keep its size
# (`x[1] <- 5` on a vector longer than SMALL_VALUE, `df$a[1] <- 1`); they show
# up on the next reassignment or a manual refresh. A freed object's address can
# be reused by a new one of the same shape; this is rare.
#

fingerprint <- function(x) {
  tryCatch(
    list(
      rs_address(x),
      typeof(x),
      class(x),
      if (is.environment(x)) length(env_names(x)) else length(x),
      dim(x),
      if (!is.environment(x) && (is.list(x) || is.atomic(x))) names(x),
      if (
        is.atomic(x) &&
          length(x) <= SMALL_VALUE &&
          (!is.character(x) || sum(nchar(x, "bytes"), na.rm = TRUE) <= 4096L)
      ) {
        x
      }
    ),
    error = function(e) list("unreadable")
  )
}

fingerprint_binding <- function(env, name) {
  if (bindingIsActive(name, env)) {
    "active"
  } else {
    fingerprint(get_binding(env, name))
  }
}

snapshot <- function(env) {
  names <- visible_names(env)
  out <- lapply(names, function(name) {
    tryCatch(fingerprint_binding(env, name), error = function(e) NULL)
  })
  names(out) <- names
  out[!vapply(out, is.null, NA)]
}

names_of <- function(x) if (is.null(names(x))) character() else names(x)

# Names assigned or changed, and names removed, between two snapshots.
diff_snapshots <- function(old, new) {
  changed <- Filter(
    function(name) {
      !(name %in% names_of(old) && identical(old[[name]], new[[name]]))
    },
    names_of(new)
  )
  list(changed = changed, removed = setdiff(names_of(old), names_of(new)))
}

#
# Services: one baseline (version and snapshot) per connection.
#

service_for <- function(connection) {
  services <- .state$services
  if (is.null(services)) {
    services <- new.env(parent = emptyenv())
    .state$services <- services
  }
  key <- sprintf("%.0f", connection)
  service <- services[[key]]
  if (is.null(service)) {
    service <- new.env(parent = emptyenv())
    service$version <- 0
    service$snapshot <- list()
    services[[key]] <- service
  }
  service
}

# Describe every variable and remember the snapshot (`list`).
refresh <- function(service) {
  env <- workspace_env()
  variables <- describe_all(global_pairs(env))
  service$version <- rs_version()
  service$snapshot <- snapshot(env)
  list(
    variables = variables,
    length = length(variables),
    version = service$version
  )
}

# What changed since the snapshot, or NULL.
update <- function(service) {
  env <- workspace_env()
  current <- snapshot(env)
  d <- diff_snapshots(service$snapshot, current)
  if (length(d$changed) == 0L && length(d$removed) == 0L) {
    return(NULL)
  }
  assigned <- describe_all(global_pairs(env, d$changed))
  service$version <- rs_version()
  service$snapshot <- current
  list(assigned = assigned, removed = I(d$removed), version = service$version)
}

# Result of `changes`: a client not at the service's last version cannot be
# diffed and is told to refresh.
changes <- function(service, since) {
  current <- rs_version()
  if (since == current) {
    return(list(version = current))
  }
  if (since != service$version) {
    return(list(version = current, refresh = TRUE))
  }
  result <- update(service)
  if (is.null(result)) {
    service$version <- current
    return(list(version = current))
  }
  result
}

handle_list <- function(params, connection) {
  refresh(service_for(connection))
}

handle_changes <- function(params, connection) {
  since <- params[["since"]]
  if (!is.numeric(since) || length(since) != 1L || is.na(since)) {
    rpc_stop(INVALID_PARAMS, "Missing `since`")
  }
  changes(service_for(connection), since)
}

handle_inspect <- function(params, connection) {
  value <- resolve(path_param(params), workspace_env())
  list(
    children = describe_all(children(value)),
    length = n_children(value)
  )
}
