test_that("index helpers return expected rows and columns", {
  context <- make_test_context(
    contents = c("abc", "defgh", "ijk"),
    selections = list(
      make_test_selection(1, 2, 1, 3, text = "bc"),
      make_test_selection(2, 1, 3, 2, text = "defgh\nijk")
    )
  )

  expect_equal(rs_get_index_first_selected_col(selection = "all", context = context), c(2, 1))
  expect_equal(rs_get_index_last_selected_col(selection = "all", context = context), c(3, 2))
  expect_equal(unname(rs_get_index_first_selected_row(selection = "first", context = context)), 1)
  expect_equal(unname(rs_get_index_last_selected_row(selection = "last", context = context)), 3)
  expect_equal(rs_get_index_selected_rows(context), c(1, 2, 3))
})

test_that("position helpers return start and end positions", {
  context <- make_test_context(
    contents = c("abc", "def"),
    selections = list(
      make_test_selection(1, 1, 1, 2, text = "ab"),
      make_test_selection(2, 2, 2, 3, text = "ef")
    )
  )

  starts <- rs_get_position_selection_start(selection = "all", context = context)
  ends <- rs_get_position_selection_end(selection = "all", context = context)

  expect_equal(starts[[1]][["row"]], 1)
  expect_equal(starts[[2]][["column"]], 2)
  expect_equal(ends[[1]][["column"]], 2)
  expect_equal(ends[[2]][["row"]], 2)

  expect_equal(rs_get_index_selection_start(selection = "first", context = context)[["column"]], 1)
  expect_equal(rs_get_index_selection_end(selection = "last", context = context)[["column"]], 3)
})
