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


              thresholdBase <- allowance * append(self$threshold, 0, 0)
              thresholdLimit <- allowance * append(self$threshold, Inf)
              revisedRate <- append(self$rate,0,0)
              # ISSUE Think there is a bug here factor out into two testable functions
              # Income (or maybe taxBuckets) Buckets - to calculate the vector of tax buckets
              # Calculate Bucket - which calculates the tax payable for each bucket. Note this
              # may need to be a function that produces a function based upon an income.
              # Interesting but funcy.
              incomeBuckets <-
                purrr::pmap(
                  list(
                    thresholdBase,
                    thresholdLimit,
                    revisedRate
                  ),
                  function(base,
                           limit,
                           rate){
                    if(income > base){
                      (max(income,limit) - base) * rate
                    } else {0}
                  }
                )
              sum(incomeBuckets)
              # TODO: Delete code below but only after we have eliminated all loops
              # in similar code
              # level <- 1
              # tax <- 0
              # while (level < no_of_thresholds) {
              #   lower_threshold <- allowance + self$threshold[level]
              #   upper_threshold <- allowance + self$threshold[level + 1]
              #   if (income > lower_threshold) {
              #     tax <- tax +
              #       (min(upper_threshold, income) - lower_threshold) * self$rate[level]
              #   }
              #   level <- level + 1
              # }
              # if (income > upper_threshold) {
              #   tax <- tax +
              #     (income - upper_threshold) * self$rate[level]
              # }
              # tax
              # End delete here
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
