test_that("equal and %is% work", {
  expect_true(equal(1, 1))
  expect_true(equal(1, 1L))
  expect_true(equal(c("a", "b"), c("a", "b")))
  expect_true(equal(factor(c("a", "b"), levels = c("a", "b", "c")), factor(c("a", "b"), levels = c("a", "b", "d"))))

  expect_false(equal(1, 2))
  expect_false(equal("a", 2))
  expect_false(equal(factor(c("a", "c"), levels = c("a", "b", "c")), factor(c("a", "b"), levels = c("a", "b", "d"))))

  expect_true(1 %is% 1)
  expect_true(1 %is% 1L)
  expect_true(c("a", "b") %is% c("a", "b"))
  expect_true(factor(c("a", "b"), levels = c("a", "b", "c")) %is% factor(c("a", "b"), levels = c("a", "b", "d")))

  expect_false(1 %is% 2)
  expect_false("a" %is% 2)
  expect_false(factor(c("a", "c"), levels = c("a", "b", "c")) %is% factor(c("a", "b"), levels = c("a", "b", "d")))
})

test_that("is_length works", {
  expect_true(is_length("foo", 1))
  expect_true(is_length(4:7, 4))
  expect_true(is_length(list("a"), 1))
})

test_that("not_empty, is_scalar, and is_multiple work", {
  expect_true(not_empty("a"))
  expect_true(not_empty(NA))
  expect_true(not_empty(c("a", "b")))
  expect_false(not_empty(NULL))
  expect_false(not_empty(character(0)))
  expect_false(not_empty(list()))

  expect_true(is_scalar("foo"))
  expect_true(is_scalar(1))
  expect_true(is_scalar(NA))
  expect_true(is_scalar(list("a")))
  expect_true(is_scalar(tibble::tibble(a = 1:4)))
  expect_false(is_scalar(4:7))
  expect_false(is_scalar(y ~ x))

  expect_true(is_multiple(c("a", "b")))
  expect_true(is_multiple(list("a", "b")))
  expect_false(is_multiple("a"))
  expect_false(is_multiple(list()))
  expect_false(is_multiple(NULL))
})

test_that("is_scalarish works", {
  expect_true(is_scalarish("foo"))
  expect_true(is_scalarish(1))
  expect_true(is_scalarish(NA))
  expect_true(is_scalarish(list("a")))
  expect_true(is_scalarish(tibble::tibble(a = 1:4)))
  expect_false(is_scalarish(4:7))

  # this should be the only difference from is_scalar
  expect_true(is_scalarish(y ~ x))
  expect_false(is_scalarish(list(y ~ x, b ~ a)))
})

test_that("is_flat works", {
  expect_true(is_flat(1:4))
  expect_true(is_flat(list("a", 1, y ~ x)))
  expect_true(is_flat(list("a", 1, y ~ x, factor("a"), Sys.Date())))
  expect_false(is_flat(list("a", 1:2, y ~ x)))
  expect_false(is_flat(list("a", list(1), y ~ x)))
})

test_that("as_vec and variants work", {
  expect_error(as_vec(list(1, 1:2)), "must be flat")
  expect_equal(as_vec(list(1, 2)),   1:2)

  expect_equal(as_vec(list(a = 1, b = 2, c = 3)),                          c(a = 1, b = 2, c = 3))
  expect_identical(as_vec(list(a = 1, b = 2, c = 3), .ptype = double(0)),  c(a = 1, b = 2, c = 3))
  expect_identical(as_vec(list(a = 1, b = 2, c = 3), .ptype = integer(0)), c(a = 1L, b = 2L, c = 3L))

  expect_equal(as_vec(list(a = "foo", b = "bar")), c(a = "foo", b = "bar"))
  expect_identical(as_int(list(a = 0, b = 1)),     c(a = 0L, b = 1L))
  expect_identical(as_dbl(list(a = 0, b = 1)),     c(a = 0, b = 1))
  expect_identical(as_lgl(list(a = 0, b = 1)),     c(a = FALSE, b = TRUE))

  expect_equal(as_fct(list(a = "foo", b = "bar")), factor(c(a = "foo", b = "bar")))
  expect_equal(as_fct(list(a = "foo", b = "bar"), levels = c("baz", "foo", "bar")),
               factor(c(a = "foo", b = "bar"), levels = c("baz", "foo", "bar")))
  expect_error(as_fct(list(a = "foo", b = "bar"), levels = c("foo", "baz")), "levels.*must contain all.*x")
})

test_that("is_setdiff works", {
  expect_true(is_setdiff(c("a", "b"), c("c", "d")))
  expect_true(is_setdiff(c("a", "b"), c("b", "c")))
  expect_false(is_setdiff(c("a", "b"), c("a", "b")))
  expect_false(is_setdiff(c("a", "b"), c("a", "b", "c")))
})

test_that("str_subset1 works", {
  v <- c("foo", "bar", "baz")

  expect_equal(str_subset1(v, pattern = "^f"), "foo")
  expect_error(str_subset1(v, pattern = "^b"), "More than one string")
  expect_error(str_subset1(v, pattern = "^z"), "No strings.*match")

  expect_equal(str_subset1(v, pattern = "^z", empty = "return_pattern"), "^z") |>
    expect_message("No values match the pattern")
  expect_equal(str_subset1(v, pattern = "^z", empty = "return_na"),      NA_character_) |>
    expect_message("returning.*NA")
})

test_that("hook works", {
  v <- c("foo", "bar", "baz")

  expect_equal(hook("^f", table = v), "foo")
  expect_error(hook("^b", table = v), "More than one string")
  expect_error(hook("^z", table = v), "No strings.*match")

  expect_equal(hook("^z", table = v, empty = "return_pattern"), "^z") |>
    expect_message("No values match the pattern")
  expect_equal(hook("^z", table = v, empty = "return_na"),      NA_character_) |>
    expect_message("returning.*NA")
})

test_that("hook_each works", {
  v <- c("foo", "bar", "baz")

  expect_equal(hook_each(c("^f", "r$", "z$"), table = v), c("foo", "bar", "baz"))
})

test_that("is_evaluable works", {
  library(rlang)

  x <- c("foo", "bar")

  expect_true(is_evaluable("foo"))
  expect_true(is_evaluable(quo(c("foo", "bar"))))

  # quosure is evaluable because eval_tidy uses it's environment to evaluate x
  expect_true(is_evaluable(quo(x)))
  expect_false(is_evaluable(expr(x)))

  expect_false(is_evaluable(quo(a:b)))
  expect_false(is_evaluable(expr(a:b)))
})

test_that("quo_is_waiver works", {
  library(rlang)

  expect_true(quo_is_waiver(quo(waiver())))
  expect_true(quo_is_waiver(expr(waiver())))
  expect_false(quo_is_waiver(quo("foo")))
  expect_false(quo_is_waiver(expr("foo")))
  expect_false(quo_is_waiver(quo(a:b)))
  expect_false(quo_is_waiver(expr(a:b)))
})

