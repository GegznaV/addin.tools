test_that("expect_equal() works", {
  expect_equal(repeat_symbol(".",   10), "..........")
  expect_equal(repeat_symbol("..+", 10), "..+..+..+.")
})

test_that("repeat_symbol_2() works", {
  expect_equal(repeat_symbol_2(".",   10), "..........")
  expect_equal(repeat_symbol_2("..+", 10), "..+..+..+..+..+..+..+..+..+..+")
})

test_that("make_spaces() works", {
  expect_equal(make_spaces(5), "     ")
  expect_equal(make_spaces(c(2, NA)), c("  ", ""))
})
