# Unit tests for the PersonalAllowance class

load_all()

library(testthat)

context("Testing PersonalAllowance class")

test_that("PersonalAllowance$amount method produce expected values", {

  personal_allowance_2017 <-
    filter(personal_allowances, year == 2017)$allowance[[1]]
  expect_equal(personal_allowance_2017$amount(100000), 11500)
  expect_equal(personal_allowance_2017$amount(100002), 11499)
  expect_equal(personal_allowance_2017$amount(100004), 11498)
  expect_equal(personal_allowance_2017$amount(123000), 0)

  personal_allowance_2016 <-
    filter(personal_allowances, year == 2016)$allowance[[1]]
  expect_equal(personal_allowance_2016$amount(100000), 11000)
  expect_equal(personal_allowance_2016$amount(100002), 10999)
  expect_equal(personal_allowance_2016$amount(100004), 10998)
  expect_equal(personal_allowance_2016$amount(123000), 0)
})
