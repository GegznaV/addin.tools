test_that("rs_insert_at_row_start sends text to every requested row", {
  captured_location <- NULL
  captured_text <- NULL
  captured_id <- NULL

  testthat::local_mocked_bindings(
    insertText = function(location, text, id = NULL) {
      captured_location <<- location
      captured_text <<- text
      captured_id <<- id
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  rs_insert_at_row_start(rows = c(2, 4), text = "x", id = "doc-1")

  expect_length(captured_location, 2)
  expect_equal(captured_text, "x")
  expect_equal(captured_id, "doc-1")
  expect_equal(captured_location[[1]]$start[["row"]], 2)
  expect_equal(captured_location[[2]]$start[["row"]], 4)
})

test_that("rs_insert_text preserves location and text without spacing", {
  captured_location <- NULL
  captured_text <- NULL
  captured_id <- NULL

  context <- make_test_context(
    contents = c("abc"),
    selections = list(make_test_selection(1, 2, 1, 2, text = "b")),
    id = "doc-2"
  )

  testthat::local_mocked_bindings(
    insertText = function(text, location, id = NULL) {
      captured_text <<- text
      captured_location <<- location
      captured_id <<- id
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  rs_insert_text(text = "X", context = context, spaces = FALSE)

  expect_equal(captured_text, "X")
  expect_equal(captured_id, "doc-2")
  expect_equal(captured_location[[1]]$start[["row"]], 1)
  expect_equal(captured_location[[1]]$start[["column"]], 2)
})

test_that("rs_insert_text adds surrounding spaces when requested", {
  captured_text <- NULL

  context <- make_test_context(
    contents = c("abc"),
    selections = list(make_test_selection(1, 2, 1, 2, text = "b"))
  )

  testthat::local_mocked_bindings(
    insertText = function(text, location, id = NULL) {
      captured_text <<- text
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  rs_insert_text(text = "X", context = context, spaces = TRUE)

  expect_equal(captured_text, " X ")
})

test_that("rs_replace_in_selection updates text and can preserve selection", {
  captured_text <- NULL
  captured_location <- NULL
  captured_ranges <- NULL

  context <- make_test_context(
    contents = c("abc"),
    selections = list(make_test_selection(1, 1, 1, 3, text = "abc")),
    id = "doc-3"
  )

  testthat::local_mocked_bindings(
    modifyRange = function(location, text, id = NULL) {
      captured_location <<- location
      captured_text <<- text
      invisible(NULL)
    },
    setSelectionRanges = function(ranges, id = NULL) {
      captured_ranges <<- ranges
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  rs_replace_in_selection(
    pattern = "b",
    replacement = "z",
    keep_selected = TRUE,
    selection = "first",
    context = context
  )

  expect_equal(captured_text, "azc")
  expect_equal(captured_location[[1]]$start[["row"]], 1)
  expect_length(captured_ranges, 1)
  expect_equal(captured_ranges[[1]]$start[["column"]], 1)
  expect_equal(captured_ranges[[1]]$end[["column"]], 3)
})

test_that("rs_replace_selection replaces whole selection text", {
  captured_text <- NULL

  context <- make_test_context(
    contents = c("abc"),
    selections = list(make_test_selection(1, 1, 1, 3, text = "abc")),
    id = "doc-4"
  )

  testthat::local_mocked_bindings(
    modifyRange = function(location, text, id = NULL) {
      captured_text <<- text
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  rs_replace_selection(
    replacement = "xy",
    keep_selected = FALSE,
    selection = "first",
    context = context
  )

  expect_equal(captured_text, "xy")
})

test_that("rs_select_rows handles vectors, ranges, and deselection", {
  captured_ranges <- NULL
  captured_id <- NULL

  context <- make_test_context(
    contents = c("one", "two", "three", "four"),
    selections = list(make_test_selection(2, 1, 2, 3, text = "two")),
    id = "doc-5"
  )

  testthat::local_mocked_bindings(
    setSelectionRanges = function(ranges, id = NULL) {
      captured_ranges <<- ranges
      captured_id <<- id
      invisible(NULL)
    },
    .package = "addin.tools"
  )

  rs_select_rows(first = c(1, 3), context = context)
  expect_length(captured_ranges, 2)
  expect_equal(captured_id, "doc-5")
  expect_equal(captured_ranges[[1]]$start[["row"]], 1)
  expect_equal(captured_ranges[[2]]$start[["row"]], 3)

  rs_select_rows(first = 2, last = 4, context = context)
  expect_equal(captured_ranges$start[["row"]], 2)
  expect_equal(captured_ranges$end[["row"]], 4)

  rs_select_rows(first = integer(0), context = context)
  expect_equal(captured_ranges$start[["row"]], 2)
  expect_equal(captured_ranges$end[["row"]], 2)
})
