labels_of <- function(result) {
  vapply(result$items, function(item) item$label, character(1))
}

# utils completes from the global environment, not the test's frame.
with_global <- function(name, code) {
  assign(name, 1, envir = globalenv())
  on.exit(rm(list = name, envir = globalenv()))
  code
}

test_that("complete replaces the token before the cursor", {
  code <- "x <- alpha_v"
  reply <- with_global(
    "alpha_value",
    dispatch("complete", list(code = code, cursor = nchar(code)), 1)
  )
  expect_true(reply$ok)
  expect_true("alpha_value" %in% labels_of(reply$result))
  expect_identical(
    substr(code, reply$result$start + 1, reply$result$stop),
    "alpha_v"
  )
})

test_that("complete handles functions, packages and multiple lines", {
  result <- complete("1\nmedia", 7L)
  item <- Filter(function(i) i$label == "median", result$items)[[1]]
  expect_identical(item$kind, KIND_FUNCTION)
  expect_identical(result$start, 2L)
  expect_true("stats::median" %in% labels_of(complete("stats::medi", 11L)))
})

test_that("complete offers column names inside strings", {
  assign("col_df", data.frame(alpha = 1, beta = "a"), envir = globalenv())
  on.exit(rm("col_df", envir = globalenv()))
  for (code in c(
    'col_df["a',
    'col_df[["a',
    'col_df[, "a',
    'x <- col_df[1, "a'
  )) {
    result <- complete(code, byte_length(code))
    expect_identical(labels_of(result), "alpha", info = code)
    expect_identical(result$stop - result$start, 1L, info = code)
    expect_identical(result$items[[1]]$kind, KIND_FIELD)
  }
  expect_identical(labels_of(complete('col_df["', 8L)), c("alpha", "beta"))
})

test_that("complete uses byte offsets", {
  code <- "α <- alpha_va"
  result <- with_global("alpha_value", complete(code, byte_length(code)))
  expect_identical(result$start, byte_length("α <- "))
  expect_identical(result$stop, byte_length(code))
})

test_that("complete is empty for nothing to complete and validates params", {
  expect_length(complete("", 0L)$items, 0L)
  expect_identical(
    rs_to_json(handle_complete(list(code = "", cursor = 0L), 1)),
    '{"items":[],"start":0,"stop":0}'
  )
  expect_identical(
    dispatch("complete", list(code = "x"), 1)$code,
    INVALID_PARAMS
  )
})
