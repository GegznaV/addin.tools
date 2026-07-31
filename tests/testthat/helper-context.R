make_test_selection <- function(start_row, start_col, end_row, end_col, text = "") {
  list(
    range = rstudioapi::document_range(c(start_row, start_col), c(end_row, end_col)),
    text = text
  )
}

make_test_context <- function(contents, selections, id = "test-doc") {
  list(
    id = id,
    path = "test.R",
    contents = contents,
    selection = selections
  )
}
