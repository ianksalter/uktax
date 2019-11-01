
# TODO: adjustedUkIncomeTaxDataFrame - gives gross income as inclusive of employers ni contribution.
# TODO: Add adjusted amount function
#' The Tax and Benefits  Class
#'
#' This class describes the UK Tax and Benefits systeem
#' Note this is a simplification of the actual UK tax system.
#' @field incomeTax The UK income tax structure
#' @field nationalInsurance The UK social security structure
#' @field universalCredit The UK asocial benifit structure
#' @keywords uk income tax, social security
#' @export
#' @examples
#' Tax and Benefits
TaxAndBenefits <- R6::R6Class("TaxAndBenefits",
                              public = list(
                                incomeTax = NULL,
                                nationalInsurance = NULL,
                                universalCredit = NULL,
                                initialize = function (incomeTax = NA, nationalInsurance = NA, universalCredit = NA){
                                  self$incomeTax <- incomeTax
                                  self$nationalInsurance <- nationalInsurance
                                  self$universalCredit <- universalCredit
                                },
                                #' The amount method
                                #'
                                #' This method determines the final amount a person will recieve after tax, national insurance
                                #' and universal credit.
                                #' @param earnedIncome The initial earned income of the person.
                                #' @param unearnedIncome The initial unearned income of the person.
                                #' amount()
                                amount = function(earnedIncome,unearnedIncome=0){
                                  earnedIncome + unearnedIncome +
                                    self$universalCredit$amount(earnedIncome,unearnedIncome) -
                                    self$incomeTax$amount(earnedIncome+unearnedIncome) -
                                    self$nationalInsurance$employee$amount(earnedIncome)
                                },
                                #' The Data Frame method
                                #'
                                #' This method returns a data frame with the following columns based upon
                                #' the tax and benefit structures:
                                #' 1) initialIncome
                                #' 2) personalAllowance
                                #' 3) incomeTax
                                #' 4) employeesNationalInsurance
                                #' 5) employersNationalInsurance
                                #' 6) universalCreditEarned - assumes all income is earned
                                #' 7) universalCreditUnearned - assumes all income is unearned
                                #' 9) finalIncomeEarned - assumes all income is earned
                                #' 10) finalIncomeUnearned - assumes all income is unearned
                                #' @param from The first point of initialIncome.
                                #' @param to The final point of the initial Income in the frame.
                                #' @param by The incriments by which initial income is increased in the frame.
                                #' dataFrame()
                                dataFrame = function(from=1,to=50001,by=1000){
                                  initialIncome <- seq(from,to,by)
                                  personalAllowance <- sapply(initialIncome,self$incomeTax$personalAllowance$amount)
                                  incomeTax <- sapply(initialIncome,self$incomeTax$amount)
                                  employeesNationalInsurance <-
                                    sapply(initialIncome,self$nationalInsurance$employee$amount)
                                  employersNationalInsurance <-
                                    sapply(initialIncome,self$nationalInsurance$employer$amount)
                                  universalCreditEarned <-
                                    sapply(initialIncome,self$universalCredit$amount)
                                  universalCreditUnearned <-
                                    sapply(initialIncome,function(income){self$universalCredit$amount(0,income)})
                                  finalIncomeEarned <-
                                    sapply(initialIncome,self$amount)
                                  finalIncomeUnearned  <-
                                    sapply(initialIncome,function(income){self$amount(0,income)})
                                  data.frame(initialIncome,
                                             personalAllowance,
                                             incomeTax,
                                             employeesNationalInsurance,
                                             employersNationalInsurance,
                                             universalCreditEarned,
                                             universalCreditUnearned,
                                             finalIncomeEarned,
                                             finalIncomeUnearned)
                                }
                                )
                              )

taxAndBenefits2017 <- TaxAndBenefits$new(
  incomeTax = incomeTax2017,
  nationalInsurance = nationalInsurance2017,
  universalCredit = universalCredit2017
)


taxAndBenefits2015 <- TaxAndBenefits$new(
  incomeTax = incomeTax2015,
  nationalInsurance = nationalInsurance2015,
  universalCredit = universalCredit2015
)
