test_that("recode_hook and replace_hook work", {
  tst <- c("foofy", "barbie", "bazzle", "barbie", "foofy")
  tst2 <- c("foofy", "barbie", "bazzle", "barbie", "foofie")
  from <- c("foo", "bar", "baz")
  to <- c("foofoo", "barbar", "bazbaz")
  f <- list("foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz")
  out <- c("foofoo", "barbar", "bazbaz", "barbar", "foofoo")

  expect_equal(recode_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz"), out)
  expect_equal(recode_hook(tst, !!!f), out)
  expect_equal(recode_hook(tst, from = from, to = to), out)
  expect_error(recode_hook(tst, !!!f, from = from, to = to), "both.*from.*and")

  expect_equal(recode_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz", "blah" ~ "x"), out) |>
    expect_message("No values match the pattern.*blah")

  expect_error(recode_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar", unmatched = "error"),
               "Location.*unmatched")
  expect_error(recode_hook(tst2, "foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz"),
               "More than one string.*matches")


  expect_equal(replace_hook(tst, c("foo", "bar") ~ "a", "baz" ~ "b"), c("a", "a", "b", "a", "a"))
  expect_equal(replace_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz"), out)
  expect_equal(replace_hook(tst, !!!f), out)
  expect_equal(replace_hook(tst, from = from, to = to), out)
  expect_error(replace_hook(tst, !!!f, from = from, to = to), "both.*from.*and")

  expect_equal(replace_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz", "blah" ~ "x"), out) |>
    expect_message("No values match the pattern.*blah")
  expect_error(replace_hook(tst2, "foo" ~ "foofoo", "bar" ~ "barbar", "baz" ~ "bazbaz"),
               "More than one string.*matches")

  expect_equal(replace_hook(tst, c("foo", "bar") ~ "a", "baz" ~ "b"), c("a", "a", "b", "a", "a"))

  # recode and replace should differ only when part of the input is not matched. Recode makes it NA, and
  # replace just retains the original values
  expect_equal(recode_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar"), c("foofoo", "barbar", NA, "barbar", "foofoo"))
  expect_equal(replace_hook(tst, "foo" ~ "foofoo", "bar" ~ "barbar"), c("foofoo", "barbar", "bazzle", "barbar", "foofoo"))
})

test_that("replace_matches works", {
  tst <- c("foofy", "barbie", "bazzle", "oof", "foofie", "rif", "barbie")

  expect_equal(replace_matches(tst, "foo" ~ "x", "bar" ~ "y"), c("x", "y", "bazzle", "oof", "x", "rif", "y"))
  expect_equal(replace_matches(tst, "f" ~ "x", "bar" ~ "y"), c("x", "y", "bazzle", "x", "x", "x", "y"))
  expect_equal(replace_matches(tst, "f$" ~ "x", "bar" ~ "y"), c("foofy", "y", "bazzle", "x", "foofie", "x", "y"))
  expect_error(replace_matches(tst, "foo" ~ "x", "bar" ~ "y", "e$" ~ "z"), "matched multiple patterns")
})

test_that("str_detect_multi works", {
  x <- c("foo", "bar", "baz", "blah")

  expect_equal(str_replace_multi(x, "ba." ~ "ee", "ah" ~ "iz"), c("foo", "ee", "ee", "bliz"))
  expect_equal(str_replace_multi(x, "oo" ~ "ee", "az" ~ "iz", "ee" ~ "NO"), c("fee", "bar", "biz", "blah"))
})

test_that("left_join2 and inner_join2 work", {
  x <- tibble::tibble(ID = c("a", "b", "c", "b", "a", "d"), a = 1:6)
  y <- tibble::tibble(ID = c("a", "b", "c"), label = c("lab A", "lab B", "lab C"))

  # the ID "d" remains but no data from y is joined in (label is `NA` for this row)
  expect_equal(left_join2(x, y, by = "ID", .after = ID),
               tibble::tibble(
                 ID = c("a", "b", "c", "b", "a", "d"),
                 label = c("lab A", "lab B", "lab C", "lab B", "lab A", NA),
                 a = 1:6
                )
  )

  # because y does not include the ID "d", this row is fully removed from the output
  expect_equal(inner_join2(x, y, by = "ID", .after = ID),
               tibble::tibble(
                 ID = c("a", "b", "c", "b", "a"),
                 label = c("lab A", "lab B", "lab C", "lab B", "lab A"),
                 a = 1:5
               )
  )
})
