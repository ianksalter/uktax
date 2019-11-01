#' The Income Tax Class
#'
#' This class describes the tax payable by an individual. Income up to the
#' personal allowance is tax free. Then income is taxed at the rate in the rate
#' vector until the corresponding threshold. Note this class makes a number of
#' simplifications including ignoring Scotland for sake of simplicity
#' @field personalAllowance class used to calculate tax free personal alowance
#'   based upon income.
#' @field threshold The vector of tax thresholds, must be in ascending order.
#' @field rate The vector of tax rates, must be same size as thresholds.
#' @section Methods: \describe{ \item{\code{amount}}{Determines how much tax a
#'   person would pay dependant upon their income.}}
#' @export
#' @examples
#'
#' Income Tax
IncomeTax <-
  R6::R6Class("IncomeTax",
          public = list(
            personal_allowance = NULL,
            threshold = NULL,
            rate = NULL,
            # TODO: refactor class to create heavier constructor
            initialize = function(personal_allowance = NA,
                                  threshold = NA,
                                  rate = NA) {
              self$personal_allowance <- personal_allowance
              self$threshold <- threshold
              self$rate <- rate
            },
            amount = function(income) {
              no_of_thresholds <- length(self$threshold)
              #Input variable preconditions
              if (no_of_thresholds != length(self$rate))
                Stop("threshold and rate vectors must be the same size")
              if (no_of_thresholds > 1 &&
                  self$threshold != sort(self$threshold))
                Stop("threshold must be in ascending order")
              allowance <- self$personal_allowance$amount(income)
              level <- 1
              tax <- 0
              # TODO: Think about refactoring this code so it works in
              # functional mannar perhaps by doing much more work in
              # initialise to create astructure easy to calculate from
              # Maybe look at creating a data structure for, or using
              # a tibble for thresholds and rates.
              while (level < no_of_thresholds) {
                lower_threshold <- allowance + self$threshold[level]
                upper_threshold <- allowance + self$threshold[level + 1]
                if (income > lower_threshold) {
                  tax <- tax +
                    (min(upper_threshold, income) - lower_threshold) * self$rate[level]
                }
                level <- level + 1
              }
              if (income > upper_threshold) {
                tax <- tax +
                  (income - upper_threshold) * self$rate[level]
              }
              tax
            }
          )
)


# Data sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
income_tax_data <-
  tibble::tribble(
    ~year, ~threshold, ~rate,
    #----/------/-----------/----------------
    2016, c(0, 32000, 150000), c(0.2, 0.4, 0.45),
    2017, c(0, 33500, 150000), c(0.2, 0.4, 0.45),
    2018, c(0, 34500, 150000), c(0.2, 0.4, 0.45),
    2019, c(0, 37500, 150000), c(0.2, 0.4, 0.45)
  )

income_tax <-
  tibble::tibble(
    year = income_tax_data$year,
    tax = purrr::pmap(list(personal_allowances$allowance,
                           income_tax_data$threshold,
                           income_tax_data$rate),
                      IncomeTax$new)
)

# Sourced from:
# https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2017-to-2018
# incomeTax2017 <- IncomeTax$new(
#   personalAllowance = personalAllowance2017,
#   threshold = c(0, 33500, 150000),
#   rate = c(0.2, 0.4, 0.45)
# )

# Sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
# incomeTax2015 <- IncomeTax$new(
#   personalAllowance = personalAllowance2015,
#   threshold = c(0, 31785, 150000),
#   rate = c(0.2, 0.4, 0.45)
# )
