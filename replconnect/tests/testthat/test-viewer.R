cells <- function(page) unlist(page$rows)
fails <- function(expr) {
  e <- tryCatch(expr, error = identity)
  expect_s3_class(e, "replconnect_rpc_error")
  expect_identical(e$code, INVALID_PARAMS)
}

test_that("table pages are bounded and preserve column order", {
  df <- data.frame(
    alpha = 1:205,
    beta = rep("line\nwith\tspaces", 205)
  )
  page <- viewer_page(df)
  expect_identical(as.character(page$names), c("alpha", "beta"))
  expect_identical(as.character(page$types), c("integer", "character"))
  expect_length(page$rows, 100)
  expect_identical(as.character(page$rows[[1]]), c("1", "line\\nwith\\tspaces"))
  expect_identical(page$total_rows, 205L)
  expect_identical(page$total_columns, 2L)
  expect_identical(as.character(page$row_labels[1:2]), c("1", "2"))
  last <- viewer_page(df, offset = 200L)
  expect_identical(as.character(last$rows[[5]])[[1]], "205")
  expect_length(last$rows, 5)
  expect_length(viewer_page(df, offset = 999L)$rows, 0)
  expect_length(viewer_page(df[0, ])$rows, 0)
  expect_identical(
    nchar(viewer_page(data.frame(x = strrep("a", 500)))$rows[[1]]),
    VIEW_CELL_LENGTH
  )
})

test_that("wide tables show a bounded number of columns", {
  wide <- as.data.frame(matrix(1:101, nrow = 1))
  page <- viewer_page(wide)
  expect_length(page$names, 100)
  expect_identical(page$total_columns, 101L)
  expect_true(page$columns_truncated)
  expect_false(viewer_page(data.frame(a = 1))$columns_truncated)
})

test_that("table cells cover factors, missing values and nested columns", {
  df <- data.frame(f = factor(c("u", "v")), n = c(1.5, NA), s = c(NA, "z"))
  df$nested <- I(list(1:2, "a"))
  page <- viewer_page(df)
  expect_identical(as.character(page$rows[[1]]), c("u", "1.5", "NA", "c(1, 2)"))
  expect_identical(as.character(page$rows[[2]]), c("v", "NA", "z", '"a"'))
  df$m <- matrix(1:4, 2)
  expect_identical(as.character(viewer_page(df)$rows[[1]])[[5]], "<nested>")
})

test_that("vectors and matrices are grids", {
  v <- viewer_page(c(10, 20, 30))
  expect_identical(as.character(v$names), "value")
  expect_identical(cells(v), c("10", "20", "30"))
  expect_identical(as.character(v$row_labels), c("1", "2", "3"))
  named <- viewer_page(c(a = 1, b = 2))
  expect_identical(as.character(named$row_labels), c("a", "b"))

  m <- matrix(1:12, 3, 4)
  page <- viewer_page(m, offset = 1L, limit = 1L)
  expect_identical(as.character(page$rows[[1]]), c("2", "5", "8", "11"))
  expect_identical(as.character(page$names), c("1", "2", "3", "4"))
  expect_identical(as.character(page$shape), c("3", "4"))
  expect_identical(as.character(page$row_labels), "2")

  dimnamed <- matrix(1:4, 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  page <- viewer_page(dimnamed)
  expect_identical(as.character(page$names), c("c1", "c2"))
  expect_identical(as.character(page$row_labels), c("r1", "r2"))

  expect_length(viewer_page(m, offset = 999L)$rows, 0)
  expect_length(viewer_page(matrix(0, 0, 3))$rows, 0)
  empty_columns <- viewer_page(matrix(0, 3, 0))
  expect_length(empty_columns$rows, 3)
  expect_length(empty_columns$rows[[1]], 0)
  big <- viewer_page(matrix(0, 205, 101))
  expect_length(big$rows, 100)
  expect_true(big$columns_truncated)
})

test_that("3D arrays load one slice at a time", {
  a <- array(1:18, c(3, 3, 2))
  first <- viewer_page(a)
  expect_identical(first$slice, 1L)
  expect_identical(first$slice_count, 2L)
  expect_identical(first$slice_first, 1L)
  expect_identical(first$slice_last, 2L)
  expect_identical(as.character(first$rows[[1]]), c("1", "4", "7"))
  second <- viewer_page(a, slice = 2L)
  expect_identical(as.character(second$rows[[1]]), c("10", "13", "16"))
  expect_identical(as.character(second$shape), c("3", "3", "2"))
  fails(viewer_page(a, slice = 0L))
  fails(viewer_page(a, slice = 3L))
  fails(viewer_page(a, slice = "bad"))
  empty <- viewer_page(array(0, c(3, 3, 0)))
  expect_identical(empty$slice_count, 0L)
  expect_null(empty$slice)
  expect_length(empty$rows, 0)
  fails(viewer_page(array(0, c(3, 3, 0)), slice = 1L))
})

test_that("unsupported values and bad bounds are rejected", {
  fails(viewer_page(list(1)))
  fails(viewer_page(sum))
  fails(viewer_page(NULL))
  fails(viewer_page(array(0, c(1, 1, 1, 1))))
  fails(viewer_page(matrix(1:4, 2), slice = 1L))
  fails(viewer_page(1:3, offset = -1L))
  fails(viewer_page(1:3, offset = 1.5))
  fails(viewer_page(1:3, limit = 101L))
  fails(viewer_page(1:3, limit = 0L))
  fails(table_page(1:3, 0L, 10L))
})

test_that("pages have the protocol's JSON shape", {
  json <- rs_to_json(viewer_page(5))
  expect_match(json, '"names":["value"]', fixed = TRUE)
  expect_match(json, '"rows":[["5"]]', fixed = TRUE)
  expect_match(json, '"row_labels":["1"]', fixed = TRUE)
  expect_match(json, '"slice":null', fixed = TRUE)
  expect_match(json, '"shape":[1]', fixed = TRUE)
  empty <- rs_to_json(viewer_page(data.frame(a = integer())))
  expect_match(empty, '"rows":[]', fixed = TRUE)
})

test_that("viewer and clipboard requests resolve variables", {
  env <- new.env()
  assign("df", data.frame(x = 1:3, y = c("a<b", "c", "d")), envir = env)
  assign("a", array(1:18, c(3, 3, 2)), envir = env)
  local_mocked_bindings(workspace_env = function() env)

  page <- dispatch(
    "viewer_page",
    list(path = list("df"), offset = 1L, limit = 2L),
    1
  )
  expect_true(page$ok)
  expect_identical(as.character(page$result$rows[[1]]), c("2", "c"))
  expect_length(page$result$rows, 2)
  column <- dispatch("viewer_page", list(path = list("df", "x")), 1)
  expect_identical(cells(column$result), c("1", "2", "3"))
  sliced <- dispatch("viewer_page", list(path = list("a"), slice = 2L), 1)
  expect_identical(sliced$result$slice, 2L)
  expect_identical(
    dispatch("viewer_page", list(path = list("a"), slice = 99L), 1)$code,
    INVALID_PARAMS
  )
  expect_identical(dispatch("viewer_page", list(), 1)$code, INVALID_PARAMS)
  expect_identical(
    dispatch("table_page", list(path = list("a")), 1)$code,
    INVALID_PARAMS
  )
  expect_true(dispatch("table_page", list(path = list("df")), 1)$ok)

  text <- dispatch(
    "clipboard_format",
    list(path = list("df"), format = "text/plain"),
    1
  )
  expect_match(text$result$content, "x", fixed = TRUE)
  expect_match(text$result$content, "y", fixed = TRUE)
  expect_match(text$result$content, "a<b", fixed = TRUE)
  html <- dispatch(
    "clipboard_format",
    list(path = list("df"), format = "text/html"),
    1
  )
  expect_match(html$result$content, "<table>", fixed = TRUE)
  expect_match(html$result$content, "a&lt;b", fixed = TRUE)
  default <- dispatch("clipboard_format", list(path = list("df")), 1)
  expect_match(default$result$content, "a<b", fixed = TRUE)
})
