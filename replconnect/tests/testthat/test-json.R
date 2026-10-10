test_that("R values have the protocol's JSON shapes", {
  expect_identical(rs_to_json(NULL), "null")
  expect_identical(rs_to_json(TRUE), "true")
  expect_identical(rs_to_json(c(1L, NA)), "[1,null]")
  expect_identical(rs_to_json(1712345678), "1712345678")
  expect_identical(rs_to_json(1.5), "1.5")
  expect_identical(rs_to_json(c(NaN, Inf)), "[null,null]")
  expect_identical(rs_to_json("a"), "\"a\"")
  expect_identical(rs_to_json(NA_character_), "null")
  expect_identical(rs_to_json(I("a")), "[\"a\"]")
  expect_identical(rs_to_json(factor("b")), "\"b\"")
  expect_identical(rs_to_json(list(a = 1L, b = list())), "{\"a\":1,\"b\":[]}")
  expect_identical(rs_to_json(setNames(list(), character())), "{}")
})

test_that("JSON decodes to R values", {
  expect_identical(rs_from_json("{\"since\": 3}"), list(since = 3L))
  expect_identical(
    rs_from_json("{\"path\": [\"df\", 2]}"),
    list(path = list("df", 2L))
  )
  expect_identical(rs_from_json("null"), NULL)
})
