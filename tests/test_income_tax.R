testthat::context("Testing IncomeTax class")

testthat::test_that("IncomeTax$amount method produce expected values", {

  income_tax_2017 <-
    dplyr::filter(income_tax, year == 2017)$tax[[1]]
  testthat::expect_equal(income_tax_2017$amount(5000), 0)
  testthat::expect_equal(income_tax_2017$amount(21500), 2000)
  # Test data from calculator here: http://iknowtax.com/2017/
  testthat::expect_equal(income_tax_2017$amount(24000),2500)
  testthat::expect_equal(income_tax_2017$amount(43000),6300)
  testthat::expect_equal(income_tax_2017$amount(55000),10700)
  testthat::expect_equal(income_tax_2017$amount(120000),40700)
  testthat::expect_equal(income_tax_2017$amount(200000),75800)
  testthat::expect_equal(income_tax_2017$amount(500000),210800)

  # TODO: Add test data from other tax years.

})
