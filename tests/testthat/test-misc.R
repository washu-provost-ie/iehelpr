test_that("is_length works", {
  expect_true(is_length("foo", 1))
  expect_true(is_length(4:7, 4))
  expect_true(is_length(list("a"), 1))
})

test_that("not_empty, is_single, and is_multiple work", {
  expect_true(not_empty("a"))
  expect_true(not_empty(NA))
  expect_true(not_empty(c("a", "b")))
  expect_false(not_empty(NULL))
  expect_false(not_empty(character(0)))
  expect_false(not_empty(list()))

  expect_true(is_single("foo"))
  expect_true(is_single(1))
  expect_true(is_single(NA))
  expect_true(is_single(list("a")))
  expect_true(is_single(tibble::tibble(a = 1:4)))
  expect_false(is_single(4:7))

  expect_true(is_multiple(c("a", "b")))
  expect_true(is_multiple(list("a", "b")))
  expect_false(is_multiple("a"))
  expect_false(is_multiple(list()))
  expect_false(is_multiple(NULL))
})

test_that("is_scalar works", {
  expect_true(is_scalar("foo"))
  expect_true(is_scalar(1))
  expect_true(is_scalar(NA))
  expect_false(is_scalar(list("a")))
  expect_false(is_scalar(tibble::tibble(a = 1:4)))
  expect_false(is_scalar(4:7))
})

test_that("is_setdiff works", {
  expect_true(is_setdiff(c("a", "b"), c("c", "d")))
  expect_true(is_setdiff(c("a", "b"), c("b", "c")))
  expect_false(is_setdiff(c("a", "b"), c("a", "b")))
  expect_false(is_setdiff(c("a", "b"), c("a", "b", "c")))
})

test_that("is_flat works", {
  expect_true(is_flat(1:4))
  expect_true(is_flat(list("a", 1, y ~ x)))
  expect_false(is_flat(list("a", 1:2, y ~ x)))
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
