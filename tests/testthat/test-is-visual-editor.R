test_that("is_visual_editor reflects empty/non-empty selection context", {
  testthat::local_mocked_bindings(
    getActiveDocumentContext = function() list(selection = list()),
    .package = "rstudioapi"
  )
  expect_true(is_visual_editor())

  testthat::local_mocked_bindings(
    getActiveDocumentContext = function() list(selection = list(list(text = "x"))),
    .package = "rstudioapi"
  )
  expect_false(is_visual_editor())
})

test_that("is_rmd_visual_mode is a backward-compatible alias", {
  testthat::local_mocked_bindings(
    getActiveDocumentContext = function() list(selection = list(list(text = "x"))),
    .package = "rstudioapi"
  )

  expect_warning(
    expect_false(is_rmd_visual_mode()),
    "deprecated",
    fixed = FALSE
  )
})
