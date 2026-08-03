test_that("sems_format functions work", {
  expect_equal(sems_format("f23", "sp25"),          c("Fall 2023", "Spring 2025"))
  expect_equal(sems_format(c("f23", "sp25")),       c("Fall 2023", "Spring 2025"))
  expect_equal(sems_format("f23", "sp25", "su25"),  c("Fall 2023", "Spring 2025", "Summer 2025"))

  expect_equal(sems_format("f23", "sp25", to_sis = TRUE), c("FL2023", "SP2025"))
  expect_equal(sems_format_sis("f23", "sp25"),            c("FL2023", "SP2025"))
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
