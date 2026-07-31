test_that("selection text, length, range, and count behave as expected", {
  context <- make_test_context(
    contents = c("alpha beta", "", "gamma", "delta"),
    selections = list(
      make_test_selection(1, 1, 1, 5, text = "alpha"),
      make_test_selection(3, 1, 3, 5, text = "gamma")
    )
  )

  expect_equal(rs_get_n_selections(context), 2)
  expect_equal(rs_get_selection_text(selection = "all", context = context), c("alpha", "gamma"))
  expect_equal(rs_get_selection_text(selection = "last", context = context), "gamma")
  expect_equal(rs_get_selection_length(selection = "all", context = context), c(5, 5))

  all_ranges <- rs_get_selection_range(selection = "all", as_list = TRUE, context = context)
  expect_length(all_ranges, 2)
  expect_equal(all_ranges[[1]]$start[["row"]], 1)
  expect_equal(all_ranges[[2]]$end[["column"]], 5)

  last_range <- rs_get_selection_range(selection = "last", as_list = TRUE, context = context)
  expect_type(last_range, "list")
  expect_length(last_range, 1)
  expect_equal(last_range[[1]]$start[["row"]], 3)
})

test_that("selection first branch can be tested with mocked selectionGet", {
  context <- make_test_context(
    contents = c("alpha beta"),
    selections = list(make_test_selection(1, 1, 1, 5, text = "alpha")),
    id = "doc-123"
  )

  testthat::local_mocked_bindings(
    selectionGet = function(id) {
      expect_equal(id, "doc-123")
      list(value = "alpha")
    },
    .package = "rstudioapi"
  )

  expect_equal(rs_get_selection_text(selection = "first", context = context), "alpha")
})
