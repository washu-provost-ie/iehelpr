test_that("glimpse2 works", {
  tst <- tibble::tibble(
    a = c("foo", "bar", "baz", "blah", "foo"),
    b = c(1, 10, 11, 4, 7),
    c = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    d = list(c("a", "b"), "c", c("xx", "zz"), 1:3, TRUE)
  )

  expect_equal(glimpse2(tst),
               tibble::tibble(
                 name = c("a", "b", "c", "d"),
                 type = c("<chr>", "<dbl>", "<lgl>", "<list>"),
                 levels = c("4: [\"bar\", \"baz\", \"blah\", \"foo\"]", "<NA>", "2: [FALSE, TRUE]", "<NA>"),
                 values = c("\"foo\", \"bar\", \"baz\", \"blah\"", "1, 10, 11, 4", "TRUE, TRUE, FALSE, FALSE", "c(\"a\", \"b\"), \"c\", c(\"xx\", \"zz\"), 1:3")
               ))

  expect_equal(glimpse2(tst, exclude_num_levels = FALSE),
               tibble::tibble(
                 name = c("a", "b", "c", "d"),
                 type = c("<chr>", "<dbl>", "<lgl>", "<list>"),
                 levels = c("4: [\"bar\", \"baz\", \"blah\", \"foo\"]", "5: [1, 4, 7, 10, 11]", "2: [FALSE, TRUE]", "<NA>"),
                 values = c("\"foo\", \"bar\", \"baz\", \"blah\"", "1, 10, 11, 4", "TRUE, TRUE, FALSE, FALSE", "c(\"a\", \"b\"), \"c\", c(\"xx\", \"zz\"), 1:3")
               ))

  expect_equal(glimpse2(tst, exclude_num_levels = FALSE, max_levels = 3),
               tibble::tibble(
                 name = c("a", "b", "c", "d"),
                 type = c("<chr>", "<dbl>", "<lgl>", "<list>"),
                 levels = c("<NA>", "<NA>", "2: [FALSE, TRUE]", "<NA>"),
                 values = c("\"foo\", \"bar\", \"baz\", \"blah\"", "1, 10, 11, 4", "TRUE, TRUE, FALSE, FALSE", "c(\"a\", \"b\"), \"c\", c(\"xx\", \"zz\"), 1:3")
               ))

  expect_equal(glimpse2(tst, n_values = 3),
               tibble::tibble(
                 name = c("a", "b", "c", "d"),
                 type = c("<chr>", "<dbl>", "<lgl>", "<list>"),
                 levels = c("4: [\"bar\", \"baz\", \"blah\", \"foo\"]", "<NA>", "2: [FALSE, TRUE]", "<NA>"),
                 values = c("\"foo\", \"bar\", \"baz\"", "1, 10, 11", "TRUE, TRUE, FALSE", "c(\"a\", \"b\"), \"c\", c(\"xx\", \"zz\")")
               ))
})
