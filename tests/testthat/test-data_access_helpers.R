test_that("extract_year and extract_term work", {
  expect_equal(extract_year(c("fall 2025a", "fall 2026b")), 2025:2026)
  expect_equal(extract_year(c("f25", "f26")),               2025:2026)
  expect_equal(extract_year(c("f2025", NA)),                c(2025, NA_integer_))

  expect_error(extract_year(c("f25", "f2026")),   "must contain.*all")
  expect_error(extract_year(c("f1980", "f2026")), "must start with.*20")
  expect_equal(extract_year(c("f1980", "f2026"), is_2000 = FALSE), c(1980, 2026))

  expect_equal(extract_term(c("fall 2025a", "spring 2026b")), c("Fall", "Spring"))
  expect_equal(extract_term(c("f25", "sp26", "su20")),        c("Fall", "Spring", "Summer"))
  expect_error(extract_term(c("f25", "sz26")),                "not interpretable as")
  expect_equal(extract_term(c("f25", NA)),                    c("Fall", NA_integer_))
})

test_that("sems_format functions work", {
  expect_equal(sems_format("f23", "sp25"),          c("Fall 2023", "Spring 2025"))
  expect_equal(sems_format(c("f23", "sp25")),       c("Fall 2023", "Spring 2025"))
  expect_equal(sems_format("f23", "sp25", "su25"),  c("Fall 2023", "Spring 2025", "Summer 2025"))

  expect_equal(sems_format("f23", "sp25", to_sis = TRUE), c("FL2023", "SP2025"))
  expect_equal(sems_format_sis("f23", "sp25"),            c("FL2023", "SP2025"))

  expect_error(sems_format(c("f25", "sz26")),             "not interpretable as")
})

test_that("sems_harmonize works", {
  expect_equal(sems_harmonize(c("FL2020", "SP2021", "SU2022")), c("Fall 2020", "Spring 2021", "Summer 2022"))
})

test_that("sems_expand works", {
  expect_equal(sems_expand("sp16", "f20:sp22", paste("f", 23:25)),
               c("Spring 2016", "Fall 2020", "Spring 2021", "Fall 2021", "Spring 2022", "Fall 2023", "Fall 2024", "Fall 2025"))

  expect_equal(sems_expand("Spring 16", "Fall 20:Spring 22", paste("Fall", 23:25)),
               c("Spring 2016", "Fall 2020", "Spring 2021", "Fall 2021", "Spring 2022", "Fall 2023", "Fall 2024", "Fall 2025"))

  expect_equal(sems_expand(sp16, f20:sp22, paste("f", 23:25)),
               c("Spring 2016", "Fall 2020", "Spring 2021", "Fall 2021", "Spring 2022", "Fall 2023", "Fall 2024", "Fall 2025"))

  expect_error(sems_expand(su16, f20:sp22, paste("f", 23:25)), "can't include.*summer")
  expect_error(sems_expand(sp16, f20:su22, paste("f", 23:25)), "can't include.*summer")
  expect_error(sems_expand(sp16, f20:sp22, paste("su", 23:25)), "can't include.*summer")

  expect_equal(sems_expand(su16, f20:sp22, paste("f", 23:25), include_summer = TRUE),
               c("Summer 2016", "Fall 2020", "Spring 2021", "Summer 2021", "Fall 2021", "Spring 2022", "Fall 2023", "Fall 2024", "Fall 2025"))
  expect_equal(sems_expand(sp16, f20:su22, paste("f", 23:25), include_summer = TRUE),
               c("Spring 2016", "Fall 2020", "Spring 2021", "Summer 2021", "Fall 2021", "Spring 2022", "Summer 2022", "Fall 2023", "Fall 2024", "Fall 2025"))
  expect_equal(sems_expand(sp16, f20:sp22, paste("su", 23:25), include_summer = TRUE),
               c("Spring 2016", "Fall 2020", "Spring 2021", "Summer 2021", "Fall 2021", "Spring 2022", "Summer 2023", "Summer 2024", "Summer 2025"))

  expect_equal(sems_expand(sp16, f20:sp22, paste("f", 23:25), to_sis = TRUE),
               c("SP2016", "FL2020", "SP2021", "FL2021", "SP2022", "FL2023", "FL2024", "FL2025"))
  expect_equal(sems_expand_sis(sp16, f20:sp22, paste("f", 23:25)),
               c("SP2016", "FL2020", "SP2021", "FL2021", "SP2022", "FL2023", "FL2024", "FL2025"))
  expect_equal(sems_expand_sis(sp16, f20:su22, paste("f", 23:25), include_summer = TRUE),
               c("SP2016", "FL2020", "SP2021", "SU2021", "FL2021", "SP2022", "SU2022", "FL2023", "FL2024", "FL2025"))
})

test_that("compute_AY_startYear, compute_student_year, and compute_student_sem work", {
  sems <- c("f24", "f26", "su20")
  entry_sems <- c("f20", "sp20", "f16")

  sems_na <- c(sems, NA)
  entry_sems_na <- c(NA, entry_sems)

  expect_equal(compute_AY_startYear(sems),                         c(2024, 2026, 2019))
  expect_equal(compute_AY(sems),                                   c("AY 2024-25", "AY 2026-27", "AY 2019-20"))
  expect_equal(compute_student_year(sems, entry_sem = entry_sems), c(5, 8, 4))
  expect_equal(compute_student_sem(sems, entry_sem = entry_sems),  c(9, 14, 8.5))

  expect_equal(compute_AY_startYear(sems_na),                            c(2024, 2026, 2019, NA_integer_))
  expect_equal(compute_AY(sems_na),                                      c("AY 2024-25", "AY 2026-27", "AY 2019-20", NA_character_))
  expect_equal(compute_student_year(sems_na, entry_sem = entry_sems_na), c(NA_integer_, 7, 1, NA_integer_))
  expect_equal(compute_student_sem(sems_na, entry_sem = entry_sems_na),  c(NA_integer_, 13, 1.5, NA_real_))
})

