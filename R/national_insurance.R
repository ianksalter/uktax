#' The National Insurance class
#'
#' The class captures the notion of UK national insurance
#' UK National insurance is a variable rate tax for both employees and employers
#' however because of the withdrawal of the personal allowance after 100000
#' UK income tax is not though it could be modeled as a variable rate tax.
#' @field employee The variable rate tax structure for employee contributions.
#' @field employer The variable rate tax structure for employer contributions.
#' @export
#' @examples
#' Income Tax
NationalInsurance <- R6::R6Class("NationalInsurance",
                                 public = list(
                                   employee = NULL,
                                   employer = NULL,
                                   #' The initialize method
                                   #'
                                   #' Constructs UK national insurance
                                   #' @param employeeThreshold A vector of thresholds for the employee
                                   #' @param employeeRate The rates applied between the corresponding threshold and the next threshold
                                   #' @param employerThreshold A vector of thresholds for the employee
                                   #' @param employerRate The rates applied between the corresponding threshold and the next threshold
                                   #' @keywords uk income tax
                                   #' initialize()
                                   initialize = function (
                                     employeeThreshold = NA,
                                     employeeRate = NA,
                                     employerThreshold = NA,
                                     employerRate = NA){
                                     self$employee <- VariableRateTax$new(employeeThreshold,employeeRate)
                                     self$employer <- VariableRateTax$new(employerThreshold,employerRate)
                                   },
                                   #' The Employee Amount method
                                   #'
                                   #' This method determines the amount to be paid by an employee with the given initial income.
                                   #' @param income The initial income of the person.
                                   #' @keywords uk income tax
                                   #' employeeAmount()
                                   employeeAmount = function(income){
                                     self$employee$amount(income)
                                   },
                                   #' The Employer Amount method
                                   #'
                                   #' This method determines the amount to be paid by an employer with the given initial income.
                                   #' @param income The initial income of the person.
                                   #' @keywords uk income tax
                                   #' employerAmount()
                                   employerAmount = function(income){
                                     self$employer$amount(income)
                                   }
                                 )
)

# Sourced from:
# https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2017-to-2018
nationalInsurance2017 <- NationalInsurance$new(
  employeeThreshold = c(8164,45000),
  employeeRate = c(0.12,0.02),
  employerThreshold = c(8112),
  employerRate = c(0.138)
)

# Note source for the following data is:
# https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2015-to-2016
nationalInsurance2015 <- NationalInsurance$new(
  employeeThreshold = c(8060,42385),
  employeeRate = c(0.12,0.02),
  employerThreshold = c(8164),
  employerRate = c(0.138)
)
