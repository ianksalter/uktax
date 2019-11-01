context('Testing UK Tax and Benefit functions 2017 to 2018')

test_that('nationalInsurance function produces expected values',{
  # Note source for the following data is:
  # https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2015-to-201
  primaryThreshold <- 8164
  upperEarningsLimit <- 45000
  primaryRate <- 0.12
  higherEarningsRate <- 0.02
  employeeNIFor12000In2017 <-
    (12000-primaryThreshold)*primaryRate
  employeeNIFor60000In2017 <-
    (60000-upperEarningsLimit)*higherEarningsRate +
    (upperEarningsLimit-primaryThreshold)*primaryRate
  #Tests
  expect_equal(nationalInsurance2017$employeeAmount(12000),employeeNIFor12000In2017)
  expect_equal(nationalInsurance2017$employeeAmount(60000),employeeNIFor60000In2017)
  # Note source for the following data is:
  # https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2015-to-2016
  secondaryThreshold2015 <- 8164
  employerRate2015 <- 0.138
  employerNIFor12000In2015 <- (12000-secondaryThreshold2015)*employerRate2015
  employerNIFor60000In2015 <- (60000-secondaryThreshold2015)*employerRate2015
  #Tests
  expect_equal(nationalInsurance2015$employerAmount(12000),employerNIFor12000In2015)
  expect_equal(nationalInsurance2015$employerAmount(60000),employerNIFor60000In2015)
})
