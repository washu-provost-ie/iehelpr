test_that("assert_scalar and assert_scalarish work", {
  fn_scalar <- function(foo) {
    assert_scalar(foo)
  }

  fn_scalarish <- function(foo) {
    assert_scalarish(foo)
  }

  expect_true(fn_scalar("a"))
  expect_error(fn_scalar(c("a", "b")), "foo.*must have length 1")
  expect_error(fn_scalar(y ~ x), "foo.*must have length 1")

  expect_true(fn_scalarish("a"))
  expect_error(fn_scalarish(c("a", "b")), "foo.*must have length 1")
  expect_true(fn_scalarish(y ~ x))
})

test_that("assert_scalar_ functions work", {
  fn_scalar_chr <- function(foo) {
    assert_scalar_character(foo)
  }

  expect_true(fn_scalar_chr("a"))
  expect_error(fn_scalar_chr(c("a", "b")), "foo.*must be a single string")
  expect_error(fn_scalar_chr(1),           "foo.*must be a single string")
})
