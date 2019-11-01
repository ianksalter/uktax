#' The Variable Rate Tax class
#'
#' A variable rate tax class captures the general idea of tax taken at different rates
#' for different levels of income.
#' UK National insurance is a variable rate tax for both employees and employers
#' however because of the withdrawal of the personal allowance after 100000
#' UK income tax is not though it could be modeled as a variable rate tax.
#' @field threshold The vector of tax thresholds, must be in ascending order.
#' @field rate The vector of tax rates, must be same size as thresholds.
#' @keywords variable tax rate
#' @export
#' @examples
#' Income Tax
VariableRateTax <- R6::R6Class("VariableRateTax",
                               public = list(
                                 threshold = NULL,
                                 rate = NULL,
                                 initialize = function (threshold = NA, rate = NA){
                                   self$threshold <- threshold
                                   self$rate <- rate
                                 },
                                 #' The Amount method
                                 #'
                                 #' This method determines the amount to be paid for the given income
                                 #' given the thresholds and the rates.
                                 #' @param income The initial income of the person.
                                 #' @keywords uk income tax
                                 #' amount()
                                 amount = function(income){
                                   noOfThresholds <- length(self$threshold)
                                   #Input variable preconditions
                                   if (noOfThresholds != length(self$rate))
                                     Stop("threshold and rate vectors must be the same size")
                                   if (noOfThresholds > 1 && self$threshold != sort(self$threshold))
                                     Stop("threshold must be in ascending order")
                                   level <- 1
                                   due <- 0
                                   if (noOfThresholds > 0){
                                     upperThreshold <- self$threshold[noOfThresholds]
                                     while (level < noOfThresholds){
                                       lowerThreshold <- self$threshold[level]
                                       upperThreshold <- self$threshold[level+1]
                                       if (income > lowerThreshold){
                                         due <- due + (min(upperThreshold,income)-lowerThreshold)*self$rate[level]
                                       }
                                       level <- level + 1
                                     }
                                     if (income > upperThreshold) {
                                       due <- due + (income-upperThreshold)*self$rate[level]
                                     }
                                   }
                                   due
                                 }
                               )
)
