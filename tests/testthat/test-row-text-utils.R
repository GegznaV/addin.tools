test_that("row range helpers compute expected ranges", {
  context <- make_test_context(
    contents = c("abc", "", "defgh"),
    selections = list(make_test_selection(2, 1, 2, 1, text = ""))
  )

  range1 <- rs_get_row_range(1, context = context)
  expect_equal(range1$start[["row"]], 1)
  expect_equal(range1$start[["column"]], 1)
  expect_equal(range1$end[["column"]], 4)

  ranges <- rs_get_row_ranges(c(1, 3), context = context)
  expect_length(ranges, 2)
  expect_equal(ranges[[2]]$end[["column"]], 6)

  with_nl <- rs_get_row_range_w_newline(2)
  expect_equal(with_nl$start[["row"]], 2)
  expect_equal(with_nl$end[["row"]], 3)

  first_selected <- rs_get_first_selected_row_range(include_newline = FALSE, context = context)
  expect_equal(first_selected$start[["row"]], 2)
})

test_that("text getters preserve row_numbers attribute", {
  context <- make_test_context(
    contents = c("l1", "l2", "l3", "l4"),
    selections = list(
      make_test_selection(2, 1, 2, 2, text = "l2"),
      make_test_selection(3, 1, 4, 2, text = "l3\nl4")
    )
  )

  txt <- rs_get_text(row = 2, end_row = 4, context = context)
  expect_equal(as.character(txt), c("l2", "l3", "l4"))
  expect_equal(attr(txt, "row_numbers"), 2:4)

  selected <- rs_get_selected_rows(context = context)
  expect_equal(attr(selected, "row_numbers"), c(2, 3, 4))

  first <- rs_get_first_selected_row(context = context)
  expect_equal(as.character(first), "l2")
  expect_equal(rs_get_first_selected_row_length(context = context), 2)

  last <- rs_get_last_selected_row(context = context)
  expect_equal(as.character(last), "l4")
  expect_equal(rs_get_last_selected_row_length(context = context), 2)

  lengths <- rs_get_row_lengths(2, end_row = 3, context = context)
  expect_equal(lengths, c(2, 2))
})
