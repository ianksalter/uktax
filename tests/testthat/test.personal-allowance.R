testthat::context("Testing PersonalAllowance class")

testthat::test_that("PersonalAllowance$amount method produce expected values", {

  personal_allowance_2017 <-
    dplyr::filter(personal_allowances, year == 2017)$allowance[[1]]
  testthat::expect_equal(personal_allowance_2017$amount(100000), 11500)
  testthat::expect_equal(personal_allowance_2017$amount(100002), 11499)
  testthat::expect_equal(personal_allowance_2017$amount(100004), 11498)
  testthat::expect_equal(personal_allowance_2017$amount(123000), 0)

  personal_allowance_2016 <-
    dplyr::filter(personal_allowances, year == 2016)$allowance[[1]]
  testthat::expect_equal(personal_allowance_2016$amount(100000), 11000)
  testthat::expect_equal(personal_allowance_2016$amount(100002), 10999)
  testthat::expect_equal(personal_allowance_2016$amount(100004), 10998)
  testthat::expect_equal(personal_allowance_2016$amount(123000), 0)
})
