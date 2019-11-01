
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


