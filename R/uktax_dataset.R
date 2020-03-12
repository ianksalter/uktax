#' Collection of Inportant UK Tax System Data
#'
#' A data set containg tax rates and thresholds, national insurance rates and
#' thresholds, population, GDP and GINI data for the United Kingdom
#' @format A tibble with 4 rows and ???? variables: \describe{
#'   \item{year}{the UK financial year beginning with the value}
#'   \item{pa_amount}{amount of the personal allowance}
#'   \item{pa_threshold}{income at which the allowance starts to be withdraw}
#'   \item{pa_withdrawel_rate}{rate at which the allowance is withdrawn}
#'   \item{tax_threshold}{vector of tax thresholds in ascending order}
#'   \item{tax_rate}{vector of tax rates, vector is the same size as
#'   tax_threshold}
#'   \item{ni_employee_threshold}{vector of employee national insurence
#'   thresholds in ascending order}
#'   \item{ni_employee_rate}{vector of employee national insurance rates vector
#'   is the same size as
#'   ni_employee_threshold}
#'   \item{ni_employer_threshold}{vector of employer national insurance
#'   thresholds in ascending order}
#'   \item{ni_employer_rate}{vector of employer national insurance rates, vector
#'   must be same size as ni_employer_threshold}
#'   \item{uc_amount}{amount of the credit}
#'   \item{uc_work_allowance}{amount that can be earned before credit is
#'   gradually withdrawn}
#'   \item{uc_earned_taper}{rate at which credit is withdrawn for earnings above
#'   the workAllowance}
#'
#'   \item{uc_unearned_taper}{rate at which credit is withdrawn for unearned
#'   income}
#'   \item{population}{the number of people living in the United
#'   Kingdom}
#'   \item{working_population}{the number of people living and working
#'   in the United Kingdom}
#'   \item{gdp}{gross domestic product for the United
#'   Kingdom}
#'   \item{gini_original}{includes all sources of income from employment,
#'   private pensions, investments and other non-government sources}
#'   \item{gini_gross}{is based on original income plus cash benefits (such as
#'   the State Pension)}
#'   \item{gini_disposable}{gross income after the effects of direct taxation}

# Stuff still to introduce Tax Reciepts Government Spending

#' }
#' @source \url{https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past}
#' @source \url{}
#'
#' @source \url{https://www.ons.gov.uk/peoplepopulationandcommunity/personalandhouseholdfinances/incomeandwealth/bulletins/householdincomeinequalityfinancial/yearending2018/}
#'
#'
#'
#' GINI England and Wales
"uktax_dataset"
