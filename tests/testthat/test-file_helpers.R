test_that("file_basename and file_extension work", {
  a <- "foo.xlsx"
  b <- "foo.bar.xlsx"
  c <- "foo.doc"
  d <- "foo.bar.doc"

  expect_equal(file_basename(a), "foo")
  expect_equal(file_basename(b), "foo.bar")
  expect_equal(file_basename(c), "foo")
  expect_equal(file_basename(d), "foo.bar")

  expect_equal(file_extension(a), ".xlsx")
  expect_equal(file_extension(b), ".xlsx")
  expect_equal(file_extension(c), ".doc")
  expect_equal(file_extension(d), ".doc")
})

test_that("ensure_extension and friends work", {
  expect_equal(ensure_extension("foo", ext = "csv"), "foo.csv")
  expect_equal(ensure_extension("foo", ext = ".csv"), "foo.csv")
  expect_equal(ensure_extension("foo.csv", ext = "csv"), "foo.csv")
  expect_equal(ensure_extension("foo.csv.bar", ext = "csv"), "foo.csv.bar.csv")
  expect_equal(ensure_extension("foo", ext = "R"), "foo.R")

  expect_equal(ensure_r("foo"), "foo.R")
  expect_equal(ensure_r("foo.R"), "foo.R")
  expect_equal(ensure_rmd("foo"), "foo.Rmd")
  expect_equal(ensure_rmd("foo.Rmd"), "foo.Rmd")
  expect_equal(ensure_rds("foo"), "foo.rds")
  expect_equal(ensure_rds("foo.rds"), "foo.rds")
  expect_equal(ensure_csv("foo"), "foo.csv")
  expect_equal(ensure_csv("foo.csv"), "foo.csv")

})
