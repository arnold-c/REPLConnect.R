# Bounded grid pages for the editor's viewer: data frames and 1-3 dimensional
# arrays (matrices, vectors). The same grid shape as the Julia service
# (julia/REPLConnect/src/viewer.jl). Paths are resolved structurally, as for
# the variables pane; nothing evaluates user code.

VIEW_PAGE_SIZE <- 100L
VIEW_MAX_COLUMNS <- 100L
VIEW_CELL_LENGTH <- 200L

ellipsize_all <- function(x, n) {
  x[is.na(x)] <- "NA"
  long <- nchar(x, type = "chars") > n
  x[long] <- paste0(substr(x[long], 1L, n - 1L), ELLIPSIS)
  x
}

# Single-line cell text: control characters escaped, capped in length.
cell_text <- function(x) {
  x[is.na(x)] <- "NA"
  x <- gsub("\n", "\\n", x, fixed = TRUE)
  x <- gsub("\r", "\\r", x, fixed = TRUE)
  x <- gsub("\t", "\\t", x, fixed = TRUE)
  ellipsize_all(x, VIEW_CELL_LENGTH)
}

# Unbounded text for the elements of a vector or column.
plain_cells <- function(piece) {
  if (is.list(piece)) {
    vapply(piece, display_value, "")
  } else if (is.character(piece)) {
    piece
  } else if (is.factor(piece)) {
    as.character(piece)
  } else {
    format(piece, trim = TRUE)
  }
}

format_cells <- function(piece, n) {
  tryCatch(
    cell_text(plain_cells(piece)),
    error = function(e) rep("<error>", n)
  )
}

validate_page <- function(offset, limit) {
  whole <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && x == round(x)
  }
  if (!whole(offset) || offset < 0) {
    rpc_stop(INVALID_PARAMS, "offset must be a nonnegative integer")
  }
  if (!whole(limit) || limit < 1 || limit > VIEW_PAGE_SIZE) {
    rpc_stop(
      INVALID_PARAMS,
      paste0("limit must be between 1 and ", VIEW_PAGE_SIZE)
    )
  }
  invisible(NULL)
}

# One-based row numbers on the page, after clamping the offset.
page_rows <- function(offset, limit, total) {
  offset <- min(offset, total)
  last <- min(offset + limit, total)
  if (last > offset) seq.int(offset + 1L, last) else integer()
}

grid_page <- function(
  names,
  types,
  rows,
  offset,
  total_rows,
  total_columns,
  row_labels,
  ...
) {
  list(
    names = I(as.character(names)),
    types = I(as.character(types)),
    rows = rows,
    row_labels = I(as.character(row_labels)),
    offset = offset,
    total_rows = total_rows,
    total_columns = total_columns,
    columns_truncated = total_columns > VIEW_MAX_COLUMNS,
    ...
  )
}

# Each row is a list element holding its cells (an array even for one column).
rows_of <- function(columns, n) {
  lapply(seq_len(n), function(i) {
    I(vapply(columns, function(cells) cells[[i]], ""))
  })
}

table_page <- function(value, offset, limit) {
  if (!is.data.frame(value)) {
    rpc_stop(
      INVALID_PARAMS,
      "The viewer supports tables and 1-3 dimensional arrays only"
    )
  }
  validate_page(offset, limit)
  total_rows <- nrow(value)
  total_columns <- ncol(value)
  shown <- seq_len(min(total_columns, VIEW_MAX_COLUMNS))
  offset <- min(offset, total_rows)
  idx <- page_rows(offset, limit, total_rows)
  cells <- lapply(shown, function(j) {
    col <- value[[j]]
    if (is.data.frame(col) || !is.null(dim(col))) {
      rep("<nested>", length(idx))
    } else {
      format_cells(col[idx], length(idx))
    }
  })
  grid_page(
    cell_text(names(value)[shown]),
    ellipsize_all(vapply(shown, function(j) class(value[[j]])[[1L]], ""), 30L),
    rows_of(cells, length(idx)),
    offset,
    total_rows,
    total_columns,
    idx
  )
}

array_page <- function(value, offset, limit, slice) {
  validate_page(offset, limit)
  d <- dim(value)
  nd <- if (is.null(d)) 1L else length(d)
  if (nd > 3L) {
    rpc_stop(INVALID_PARAMS, "Array viewing supports 1, 2 or 3 dimensions")
  }
  n3 <- if (nd == 3L) d[[3L]] else 0L
  if (nd == 3L) {
    if (
      !is.null(slice) &&
        !(is.numeric(slice) &&
          length(slice) == 1L &&
          !is.na(slice) &&
          slice == round(slice))
    ) {
      rpc_stop(INVALID_PARAMS, "slice must be an integer")
    }
    if (n3 == 0L) {
      if (!is.null(slice)) {
        rpc_stop(INVALID_PARAMS, "The third axis has no slices")
      }
    } else {
      if (is.null(slice)) {
        slice <- 1L
      }
      if (slice < 1L || slice > n3) {
        rpc_stop(INVALID_PARAMS, "slice is outside the third axis")
      }
    }
  } else if (!is.null(slice)) {
    rpc_stop(INVALID_PARAMS, "Only 3D arrays have slice tabs")
  }

  total_rows <- if (nd == 1L) length(value) else d[[1L]]
  total_columns <- if (nd == 1L) 1L else d[[2L]]
  offset <- min(offset, total_rows)
  idx <- page_rows(offset, limit, total_rows)
  shown <- seq_len(min(total_columns, VIEW_MAX_COLUMNS))

  grid <- if (nd == 3L && n3 > 0L) {
    sub <- value[,, slice, drop = FALSE]
    dim(sub) <- d[1:2]
    sub
  } else {
    value
  }
  cells <- if (nd == 1L) {
    list(format_cells(value[idx], length(idx)))
  } else if (nd == 3L && n3 == 0L) {
    lapply(shown, function(j) rep("", length(idx)))
  } else {
    lapply(shown, function(j) format_cells(grid[idx, j], length(idx)))
  }
  rows <- if (nd == 3L && n3 == 0L) list() else rows_of(cells, length(idx))

  dimnames <- dimnames(value)
  row_names <- if (nd == 1L) names(value) else dimnames[[1L]]
  col_names <- if (nd == 1L) {
    "value"
  } else if (!is.null(dimnames[[2L]])) {
    cell_text(dimnames[[2L]][shown])
  } else {
    as.character(shown)
  }
  grid_page(
    col_names,
    rep(ellipsize_all(typeof(value), 30L), length(col_names)),
    rows,
    offset,
    total_rows,
    total_columns,
    if (is.null(row_names)) idx else cell_text(row_names[idx]),
    shape = I(if (is.null(d)) length(value) else d),
    slice_count = n3,
    slice_first = 1L,
    slice_last = n3,
    slice = slice
  )
}

viewer_page <- function(
  value,
  offset = 0L,
  limit = VIEW_PAGE_SIZE,
  slice = NULL
) {
  validate_page(offset, limit)
  if (is.data.frame(value)) {
    table_page(value, offset, limit)
  } else if (!is.null(dim(value)) || (is.atomic(value) && !is.null(value))) {
    array_page(value, offset, limit, slice)
  } else {
    rpc_stop(
      INVALID_PARAMS,
      "The viewer supports tables and 1-3 dimensional arrays only"
    )
  }
}

param_or <- function(params, name, default) {
  value <- params[[name]]
  if (is.null(value)) default else value
}

handle_viewer_page <- function(params, connection) {
  viewer_page(
    resolve(path_param(params), workspace_env()),
    offset = param_or(params, "offset", 0L),
    limit = param_or(params, "limit", VIEW_PAGE_SIZE),
    slice = params[["slice"]]
  )
}

# Legacy table-only page.
handle_table_page <- function(params, connection) {
  table_page(
    resolve(path_param(params), workspace_env()),
    offset = param_or(params, "offset", 0L),
    limit = param_or(params, "limit", VIEW_PAGE_SIZE)
  )
}

#
# Clipboard
#

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

html_table <- function(x) {
  df <- as.data.frame(x, stringsAsFactors = FALSE)
  if (ncol(df) == 0L) {
    return("<table></table>")
  }
  cols <- lapply(df, plain_cells)
  header <- paste0("<th>", html_escape(names(df)), "</th>", collapse = "")
  rows <- vapply(
    seq_len(nrow(df)),
    function(i) {
      cells <- html_escape(vapply(
        cols,
        function(col) as.character(col[[i]]),
        ""
      ))
      paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
    },
    ""
  )
  paste0(
    "<table><thead><tr>",
    header,
    "</tr></thead><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table>"
  )
}

# Unlimited text for copying: HTML for tables when requested, else print().
clipboard_text <- function(value, format) {
  if (
    identical(format, "text/html") && (is.data.frame(value) || is.matrix(value))
  ) {
    return(html_table(value))
  }
  old <- options(max.print = .Machine$integer.max, width = 10000L)
  on.exit(options(old))
  paste(utils::capture.output(print(value)), collapse = "\n")
}

handle_clipboard_format <- function(params, connection) {
  value <- resolve(path_param(params), workspace_env())
  list(
    content = clipboard_text(value, param_or(params, "format", "text/plain"))
  )
}
