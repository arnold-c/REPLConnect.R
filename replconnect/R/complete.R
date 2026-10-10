# Runtime completion for the `complete` request, answered by utils' own
# completion engine (the one behind R's console) on the line at the cursor.
# Cursors and ranges on the wire are 0-based UTF-8 byte offsets, `start`
# inclusive and `stop` exclusive (docs/protocol.md).

COMPLETE_LIMIT <- 1000L

# LSP `CompletionItemKind` values, so clients can pass them straight through.
KIND_TEXT <- 1L
KIND_FUNCTION <- 3L
KIND_VARIABLE <- 6L
KIND_FIELD <- 5L
KIND_MODULE <- 9L
KIND_PROPERTY <- 10L
KIND_FILE <- 17L

byte_length <- function(x) nchar(x, type = "bytes")

# The first `n` bytes of a UTF-8 string, backed off to a character boundary.
utf8_prefix <- function(text, n) {
  bytes <- charToRaw(enc2utf8(text))
  n <- min(max(n, 0L), length(bytes))
  repeat {
    prefix <- rawToChar(bytes[seq_len(n)])
    Encoding(prefix) <- "UTF-8"
    if (n == 0L || validUTF8(prefix)) {
      return(prefix)
    }
    n <- n - 1L
  }
}

# Raw utils completions for the end of `line`, with the token they replace.
retrieve_completions <- function(line) {
  utils:::.assignLinebuffer(line)
  utils:::.assignEnd(nchar(line))
  utils:::.guessTokenFromLine()
  utils:::.completeToken()
  list(
    token = utils:::.CompletionEnv[["token"]],
    labels = as.character(utils:::.retrieveCompletions())
  )
}

completion_kind <- function(label) {
  if (grepl("=$", label)) {
    KIND_PROPERTY
  } else if (grepl("::$", label)) {
    KIND_MODULE
  } else if (grepl("[/\\\\]", label) && !grepl("::", label, fixed = TRUE)) {
    KIND_FILE
  } else if (exists(label, mode = "function")) {
    KIND_FUNCTION
  } else {
    KIND_VARIABLE
  }
}

completion_item <- function(label) {
  list(label = label, kind = completion_kind(label))
}

# `df["`, `df[["` and `df[, "`: the table name and the partial string typed.
COLUMN_CONTEXT <- paste0(
  "([A-Za-z.][A-Za-z0-9._]*)\\[\\[?",
  "\\s*(?:[^\\[\\],\"']*,\\s*)?[\"']([^\"']*)$"
)

# Column items for a data frame or list named in `line`, or NULL when the
# cursor is not inside a column string.
column_completions <- function(line) {
  m <- regmatches(line, regexec(COLUMN_CONTEXT, line, perl = TRUE))[[1]]
  if (length(m) != 3L) {
    return(NULL)
  }
  table <- tryCatch(get0(m[[2]], envir = globalenv()), error = function(e) NULL)
  if (!is.data.frame(table) && !is.list(table)) {
    return(NULL)
  }
  names <- names(table)
  names <- names[startsWith(names, m[[3]])]
  list(
    token = m[[3]],
    items = lapply(names, function(name) {
      list(
        label = name,
        kind = KIND_FIELD,
        detail = class(table[[name]])[[1]]
      )
    })
  )
}

#' Completions at a cursor
#' @param code The buffer text.
#' @param cursor Zero-based UTF-8 byte offset of the cursor.
#' @return `list(items, start, stop)`, items as a JSON array.
#' @noRd
complete <- function(code, cursor) {
  before <- utf8_prefix(code, cursor)
  line <- sub("^(.*\n)?", "", before)
  stop_byte <- byte_length(before)
  empty <- list(items = I(list()), start = stop_byte, stop = stop_byte)
  if (!nzchar(line)) {
    return(empty)
  }
  columns <- column_completions(line)
  if (!is.null(columns)) {
    return(list(
      items = I(columns$items),
      start = stop_byte - byte_length(columns$token),
      stop = stop_byte
    ))
  }
  found <- tryCatch(retrieve_completions(line), error = function(e) NULL)
  labels <- utils::head(unique(found$labels), COMPLETE_LIMIT)
  if (length(labels) == 0L) {
    return(empty)
  }
  list(
    items = I(unname(lapply(labels, completion_item))),
    start = stop_byte - byte_length(found$token),
    stop = stop_byte
  )
}

handle_complete <- function(params, connection) {
  code <- params[["code"]]
  cursor <- params[["cursor"]]
  if (
    !is.character(code) ||
      length(code) != 1L ||
      !is.numeric(cursor) ||
      length(cursor) != 1L ||
      !is.finite(cursor) ||
      cursor < 0 ||
      cursor != floor(cursor)
  ) {
    rpc_stop(INVALID_PARAMS, "code and cursor are required")
  }
  complete(code, as.integer(cursor))
}
