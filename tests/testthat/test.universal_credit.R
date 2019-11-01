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

