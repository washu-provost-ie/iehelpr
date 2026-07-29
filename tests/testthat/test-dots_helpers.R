test_that("dots_flat works", {
  expect_equal(dots_flat(1, 2, 3),       list(1, 2, 3))
  expect_equal(dots_flat(list(1, 2, 3)), list(1, 2, 3))
  expect_equal(dots_flat(c(1, 2, 3)),    list(1, 2, 3))

  expect_equal(dots_flat(a = 1, b = 2, c = 3),       list(a = 1, b = 2, c = 3))
  expect_equal(dots_flat(list(a = 1, b = 2, c = 3)), list(a = 1, b = 2, c = 3))
  expect_equal(dots_flat(c(a = 1, b = 2, c = 3)),    list(a = 1, b = 2, c = 3))

  expect_identical(dots_flat(a = 1, b = 2, c = 3),                      list(a = 1, b = 2, c = 3))
  expect_identical(dots_flat(a = 1, b = 2, c = 3, .ptype = integer(0)), c(a = 1L, b = 2L, c = 3L))

  expect_error(dots_flat(1, 2:4, 3),       "could not be flattened")
  expect_error(dots_flat(list(1, 2:4, 3)), "could not be flattened")
})
