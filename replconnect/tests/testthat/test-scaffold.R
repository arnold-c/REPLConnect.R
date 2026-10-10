test_that("the Rust library is loaded and speaks protocol v1", {
  expect_identical(replconnect:::protocol_version(), 1L)
})
