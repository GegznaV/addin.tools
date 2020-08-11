test_that("rs_get_n_selections works", {
  # rs_document_context is object for testing
  data("rs_document_context", package = "addin.tools")
  expect_equal(rs_get_n_selections(rs_document_context), expected = 7)
  expect_equal(rs_get_n_selections(context = NULL), expected = 0)
})
