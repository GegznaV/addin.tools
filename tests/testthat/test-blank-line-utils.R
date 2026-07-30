test_that("blank-line detectors and ensure_blank_line work with context", {
  context <- make_test_context(
    contents = c("header", "selected", "tail"),
    selections = list(make_test_selection(2, 1, 2, 8, text = "selected"))
  )

  expect_true(is_blank_line_needed_above(context = context))
  expect_true(is_blank_line_needed_below(where = "last row", context = context))
  expect_true(is_blank_line_needed_below(where = "first row", context = context))

  out <- ensure_blank_line(
    text = "x",
    context = context,
    above = TRUE,
    below_first_row = TRUE,
    below_selection = FALSE
  )
  expect_equal(out, "\nx\n")
})

test_that("blank-line detectors return FALSE when surrounding lines are blank", {
  context <- make_test_context(
    contents = c("", "selected", "   "),
    selections = list(make_test_selection(2, 1, 2, 8, text = "selected"))
  )

  expect_false(is_blank_line_needed_above(context = context))
  expect_false(is_blank_line_needed_below(where = "last row", context = context))
})

test_that("blank line below is needed when selection is on last line", {
  context <- make_test_context(
    contents = c("line1", "line2"),
    selections = list(make_test_selection(2, 1, 2, 5, text = "line2"))
  )

  expect_true(is_blank_line_needed_below(where = "last row", context = context))
})
