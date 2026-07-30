test_that("select_correct_range updates selection ranges for independent rows", {
  captured <- NULL
  captured_id <- NULL

  testthat::local_mocked_bindings(
    setSelectionRanges = function(ranges, id = NULL) {
      captured <<- ranges
      captured_id <<- id
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  old_range <- list(
    rstudioapi::document_range(c(1, 1), c(1, 2)),
    rstudioapi::document_range(c(2, 3), c(2, 4))
  )

  select_correct_range(
    old_text = c("ab", "cd"),
    new_text = c("abc", "c"),
    old_range = old_range,
    id = "doc-a"
  )

  expect_equal(captured_id, "doc-a")
  expect_length(captured, 2)

  expect_equal(captured[[1]]$start[["row"]], 1)
  expect_equal(captured[[1]]$start[["column"]], 1)
  expect_equal(captured[[1]]$end[["column"]], 3)

  expect_equal(captured[[2]]$start[["row"]], 2)
  expect_equal(captured[[2]]$start[["column"]], 3)
  expect_equal(captured[[2]]$end[["column"]], 3)
})

test_that("select_correct_range shifts later selections on same row", {
  captured <- NULL

  testthat::local_mocked_bindings(
    setSelectionRanges = function(ranges, id = NULL) {
      captured <<- ranges
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  old_range <- list(
    rstudioapi::document_range(c(1, 2), c(1, 3)),
    rstudioapi::document_range(c(1, 6), c(1, 7))
  )

  select_correct_range(
    old_text = c("ab", "xy"),
    new_text = c("abcd", "x"),
    old_range = old_range,
    id = "doc-b"
  )

  expect_length(captured, 2)

  expect_equal(captured[[1]]$start[["column"]], 2)
  expect_equal(captured[[1]]$end[["column"]], 5)

  expect_equal(captured[[2]]$start[["column"]], 8)
  expect_equal(captured[[2]]$end[["column"]], 8)
})
