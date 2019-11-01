#' The Universal Credit Class
#'
#' This class describes the Universal Credit available to a single UK citizen
#' over 25 with no children.
#' @field amount The amount of the credit.
#' @field workAllowance The amount that can be earned before credit is gradually withdrawn.
#' @field earnedTaper The rate at which credit is withdrawn for earnings above the workAllowance.
#' @field unearnedTaper The rate at which credit is withdrawn for unearned income.
#' @keywords uk income tax, personal allowance
#' @export
#' @examples
#' UniversalCredit
UniversalCredit <- R6::R6Class("UniversalCredit",
                               public = list(
                                 baseAmount = NULL,
                                 workAllowance = NULL,
                                 earnedTaper = NULL,
                                 unearnedTaper = NULL,
                                 initialize = function (baseAmount = NA, workAllowance = NA, earnedTaper = NA, unearnedTaper = NA){
                                   self$baseAmount <- baseAmount
                                   self$workAllowance <- workAllowance
                                   self$earnedTaper <- earnedTaper
                                   self$unearnedTaper <- unearnedTaper
                                 },
                                 #' The amount method
                                 #'
                                 #' This method determines how much universal credit is due dependant upon their income.
                                 #' @param earnedIncome The initial earned income of the person.
                                 #' @param unearnedIncome The initial unearned income of the person.
                                 #' @keywords uk universal credit
                                 #' amount()
                                 amount = function(earnedIncome,unearnedIncome=0){
                                   max(0,
                                       self$baseAmount -
                                         max(0,(earnedIncome - self$workAllowance))*self$earnedTaper -
                                         (unearnedIncome*self$unearnedTaper))
                                 }
                               )
)

# Data for 2017/18 from:
# https://revenuebenefits.org.uk/universal-credit/guidance/entitlement-to-uc/calculating-universal-credit
universalCredit2017 <- UniversalCredit$new(
  baseAmount = 371.8*12,
  workAllowance = 0,
  earnedTaper = 0.63,
  unearnedTaper = 1
)

# Data for for 2015/16 from:
# http://researchbriefings.files.parliament.uk/documents/CBP-7446/CBP-7446.pdf
universalCredit2015 <- UniversalCredit$new(
  baseAmount = 371.8*12,
  workAllowance = 1332,
  earnedTaper = 0.65,
  unearnedTaper = 1
)

