# Data sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
personal_allowance_data <-
  tibble::tribble(
    ~year, ~pa_amount, ~pa_threshold, ~pa_withdrawel_rate,
    #----/------/-----------/----------------
    2016, 11000, 100000, 0.5,
    2017, 11500, 100000, 0.5,
    2018, 11850, 100000, 0.5,
    2019, 12500, 100000, 0.5
  )


# Data sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
income_tax_data <-
  tibble::tribble(
    ~year, ~tax_threshold, ~tax_rate,
    #----/------/-----------/----------------
    2016, c(0, 32000, 150000), c(0.2, 0.4, 0.45),
    2017, c(0, 33500, 150000), c(0.2, 0.4, 0.45),
    2018, c(0, 34500, 150000), c(0.2, 0.4, 0.45),
    2019, c(0, 37500, 150000), c(0.2, 0.4, 0.45)
  )

# Mote will need to map this function across the full list to get the actual structure
both <- dplyr::full_join(personal_allowance_data, income_tax_data, by = "year")
