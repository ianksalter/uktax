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
  # Test data from calculator here: http://iknowtax.com/2016/
  income_tax_2016 <-
    dplyr::filter(income_tax, year == 2016)$tax[[1]]
  testthat::expect_equal(income_tax_2016$amount(25000),2800)
  testthat::expect_equal(income_tax_2016$amount(200000),76100)
  # Test data from calculator here: http://iknowtax.com/2018/
  income_tax_2018 <-
    dplyr::filter(income_tax, year == 2018)$tax[[1]]
  testthat::expect_equal(income_tax_2018$amount(25000),2630)
  testthat::expect_equal(income_tax_2018$amount(200000),75600)
  # Test data from calculator here: http://iknowtax.com/2019/
  income_tax_2019 <-
    dplyr::filter(income_tax, year == 2019)$tax[[1]]
  testthat::expect_equal(income_tax_2019$amount(25000),2500)
  testthat::expect_equal(income_tax_2019$amount(200000),75000)
})
