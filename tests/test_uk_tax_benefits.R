# Unit tests for the functions in the file UKTaxAndBenefits2017to2018.r

source("src/main/r/UKTaxAndBenefits.r")

library(testthat)

context('Testing UK Tax and Benefit functions 2017 to 2018')

test_that('PersonalAllowance$amount method produce expected values',{
  
  expect_equal(personalAllowance2017$amount(100000),11500)
  expect_equal(personalAllowance2017$amount(100002),11499)
  expect_equal(personalAllowance2017$amount(100004),11498)
  expect_equal(personalAllowance2017$amount(123000),0)

  expect_equal(personalAllowance2015$amount(100000),10600)
  expect_equal(personalAllowance2015$amount(100002),10599)
  expect_equal(personalAllowance2015$amount(100004),10598)
  expect_equal(personalAllowance2015$amount(123000),0)
})

test_that('Tax$amount method produces expected values',{
  expect_equal(incomeTax2017$amount(5000),0)
  expect_equal(incomeTax2017$amount(21500),2000)
  # Test data from calculator here: http://iknowtax.com/2017/
  expect_equal(incomeTax2017$amount(24000),2500)
  expect_equal(incomeTax2017$amount(43000),6300)
  expect_equal(incomeTax2017$amount(55000),10700)
  expect_equal(incomeTax2017$amount(120000),40700)
  expect_equal(incomeTax2017$amount(200000),75800)
  expect_equal(incomeTax2017$amount(500000),210800)
  # Test data from calculator here: http://iknowtax.com/2015/
  expect_equal(incomeTax2015$amount(24000),2680)
  expect_equal(incomeTax2015$amount(43000),6603)
})

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

test_that('universalCredit function produces expected values',{
  # Universal Credit Constants for 2017/18 from:
  # https://revenuebenefits.org.uk/universal-credit/guidance/entitlement-to-uc/calculating-universal-credit
  amount <- 371.8*12
  workAllowance <- 0
  earnedTaper <- 0.63
  unearnedTaper <- 1 
  expect_equal(universalCredit2017$amount(0),amount)
  expect_equal(universalCredit2017$amount(1000),amount-(1000*earnedTaper))
  expect_equal(universalCredit2017$amount(amount/earnedTaper),0)
  expect_equal(universalCredit2017$amount(1000,1000),amount-1000*earnedTaper-1000*unearnedTaper)
  expect_equal(universalCredit2017$amount(0,amount),0)
  expect_equal(universalCredit2017$amount(amount,amount),0)
  # Universal Credit Constants for 2015/16 from:
  # http://researchbriefings.files.parliament.uk/documents/CBP-7446/CBP-7446.pdf
  workAllowance <- 1332
  earnedTaper <- 0.65
  expect_equal(universalCredit2015$amount(0),amount)
  expect_equal(universalCredit2015$amount(1000),amount)
  expect_equal(universalCredit2015$amount(workAllowance + amount/earnedTaper),0)
  expect_equal(universalCredit2015$amount(1000,1000),amount-1000)
  expect_equal(universalCredit2015$amount(0,amount),0)
  expect_equal(universalCredit2015$amount(amount,amount),0)
})


test_that('ukIncomeTaxDataFrame function has the correct shape',{
  taxAndBenefitsDf <- taxAndBenefits2017$dataFrame()
  expect_length(taxAndBenefitsDf$initialIncome,51)
  expect_length(taxAndBenefitsDf$personalAllowance,51)
  expect_length(taxAndBenefitsDf$incomeTax,51)
  expect_length(taxAndBenefitsDf$employeesNationalInsurance,51)
  expect_length(taxAndBenefitsDf$employersNationalInsurance,51)
  expect_length(taxAndBenefitsDf$universalCreditEarned,51)
  expect_length(taxAndBenefitsDf$universalCreditUnearned,51)
  expect_length(taxAndBenefitsDf$finalIncomeEarned,51)
  expect_length(taxAndBenefitsDf$finalIncomeUnearned,51)
})

test_that('ukIncomeDataFrame function produces correct values',{
  taxAndBenefitsDf <- taxAndBenefits2017$dataFrame()
  expect_equal(taxAndBenefitsDf$initialIncome[1],1)
  testPoint1 <- 5
  initialIncome1 <- taxAndBenefitsDf$initialIncome[testPoint1]
  testPoint2 <-35
  initialIncome2 <- taxAndBenefitsDf$initialIncome[testPoint2]
  expect_equal(taxAndBenefitsDf$personalAllowance[testPoint2],
               taxAndBenefits2017$incomeTax$personalAllowance$amount(initialIncome2))
  expect_equal(taxAndBenefitsDf$incomeTax[testPoint2],
               taxAndBenefits2017$incomeTax$amount(initialIncome2))
  expect_equal(taxAndBenefitsDf$employeesNationalInsurance[testPoint2],
               taxAndBenefits2017$nationalInsurance$employee$amount(initialIncome2))
  expect_equal(taxAndBenefitsDf$employersNationalInsurance[testPoint2],
               taxAndBenefits2017$nationalInsurance$employer$amount(initialIncome2))
  expect_equal(taxAndBenefitsDf$universalCreditEarned[testPoint1],
               taxAndBenefits2017$universalCredit$amount(initialIncome1))
  expect_equal(taxAndBenefitsDf$universalCreditUnearned[testPoint1],
               taxAndBenefits2017$universalCredit$amount(0,initialIncome1))
  expect_equal(taxAndBenefitsDf$finalIncomeEarned[testPoint1],
               initialIncome1 +
                 taxAndBenefits2017$universalCredit$amount(initialIncome1) -
                 taxAndBenefits2017$incomeTax$amount(initialIncome1) -
                 taxAndBenefits2017$nationalInsurance$employee$amount(initialIncome1)) 
  expect_equal(taxAndBenefitsDf$finalIncomeUnearned[testPoint1],
               initialIncome1 +
                 taxAndBenefits2017$universalCredit$amount(0,initialIncome1)-
                 taxAndBenefits2017$incomeTax$amount(initialIncome1))
})


