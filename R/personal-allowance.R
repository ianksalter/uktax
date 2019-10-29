library(R6)
library(tidyverse)
library(purrr)

#' The Personal Allowance Class
#'
#' This class describes the personal allowance that a person has in the UK tax system.
#' In the UK the personal allowance is how much you can earn tax free. However the allowance is
#' withdrawn at the given rate over the threshold.
#' Note this is a simplification of the actual UK tax system. For example
#' this class ignores the married couples allowance.
#' @field allowance The amount of the allowance.
#' @field threshold The income at which the allowance starts to be withdrawn.
#' @field rate The rate at which the allowance is withdrawn.
#' @method amount Determines how much personal alowance a person has dependant upon their income.
#' @section Methods:
#' \describe{
#' \item{\code{amount}}{Determines how much personal alowance a person has dependant upon their income.}
#' }
#' @keywords uk income tax, personal allowance
#' @export
#' @examples
#' personalAllowance2017 <- PersonalAllowance$new(
#'   base = 11500,
#'   threshold = 100000,
#'   withdrawalRate = 0.5
#' )
#' personalAllowance2017$amount(101000)
PersonalAllowance <-R6Class("PersonalAllowance",
                            public = list(
                              base = NULL,
                              threshold = NULL,
                              withdrawalRate = NULL,
                              initialize = function (base = NA, threshold = NA, withdrawalRate = NA){
                                self$base <- base
                                self$threshold <- threshold
                                self$withdrawalRate <- withdrawalRate
                              },
                              amount = function(income){
                                if (income < self$threshold)
                                  self$base
                                else
                                  max(0 , self$base - floor((income-self$threshold)*self$withdrawalRate))
                              }
                            )
)

# Data sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
personalAllowanceData <-
  tribble(
    ~year, ~base, ~threshold, ~withdrawalRate,
    #----/-----/------/---
    2016,11000,100000,0.5,
    2017,11500,100000,0.5,
    2018,11850,100000,0.5,
    2019,12500,100000,0.5
  )

filter(personalAllowanceData, year == 2016)$base #Note potentially useful

pmap(list(personalAllowanceData$base,
          personalAllowanceData$threshold,
          personalAllowanceData$withdrawalRate),
     PersonalAllowance$new)

personalAllowances <-
  tibble(
    year = personalAllowanceData$year,
    allowance = pmap(list(personalAllowanceData$base,
                           personalAllowanceData$threshold,
                           personalAllowanceData$withdrawalRate),
                      PersonalAllowance$new)
  )


# TODO: Delete stuff below once refactored.
# Sourced from:
# https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2017-to-2018
# personalAllowance2017 <- PersonalAllowance$new(
#   base = 11500,
#   threshold = 100000,
#   withdrawalRate = 0.5
# )

# Sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
# personalAllowance2015 <- PersonalAllowance$new(
#   base = 10600,
#   threshold = 100000,
#   withdrawalRate = 0.5
# )


