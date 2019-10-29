# This file creates a set of functions representing the key components of the UK
# Tax and Benefit System in 2017/2018. It is not supposed to create
# a set of calculators covering every eventuality, but a set that can be used
# to illustrate discussions of the tax and benefit system.

# TODO Consider refactoring into a set of seperate classes?

library(R6)

#' The Personal Allowance Class
#'
#' This class describes the personal allowance that a person has in the UK tax system.
#' In the UK the personal allowance is how much you can earn tax free. However the allowance is
#' withdrawn at the give rate over the threshold
#' Note this is a simplification of the actual UK tax system.
#' @field allowance The amount of the allowance.
#' @field threshold The income at which the allowance starts to be withdrawn
#' @field rate The rate at which the allowance is withdrawn.
#' @keywords uk income tax, personal allowance
#' @export
#' @examples
#' Personal Allowance
PersonalAllowance <-R6Class("PersonalAllowance",
  public = list(
    base = NULL,
    threshold = NULL,
    withdrawalRate = NULL,
    initialize = function (base = NA, threshold = NA, withdrawalRate = NA){
      self$base <- base
      self$threshold <- threshold
      self$withdrawalRate <- withdrawalRate
    },    #' The amount method
    #'
    #' This method determines how much personal alowance a person has dependant upon their income.ed
    #' @param income The initial income of the person.
    #' @keywords uk income tax, personal allowance
    #' @export
    #' @examples
    #' amount()
    amount = function(income){
      if (income < self$threshold)
        self$base
      else
        max(0 , self$base - floor((income-self$threshold)*self$withdrawalRate))
    }
  )
)

# Sourced from:
# https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2017-to-2018
personalAllowance2017 <- PersonalAllowance$new(
  base = 11500,
  threshold = 100000,
  withdrawalRate = 0.5
)

# Sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
personalAllowance2015 <- PersonalAllowance$new(
  base = 10600,
  threshold = 100000,
  withdrawalRate = 0.5
)

#' The Income Tax Class
#'
#' This class describes the tax payable by an individual..
#' Income up to the personal allowance is tax free. Then income is taxed at
#' the rate in the rate vector until the corresponding threshold.
#' Note this class makes a number of simplifications including ignoring Scotland for sake of simplicity
#' @field personalAllowance class used to calculate tax free personal alowance based upon income.
#' @field threshold The vector of tax thresholds, must be in ascending order.
#' @field rate The vector of tax rates, must be same size as thresholds.
#' @keywords uk income tax
#' @export
#' @examples
#' Income Tax
IncomeTax <-R6Class("IncomeTax",
  public = list(
    personalAllowance = NULL,
    threshold = NULL,
    rate = NULL,
    initialize = function (personalAllowance = NA,threshold = NA, rate = NA){
      self$personalAllowance <- personalAllowance
      self$threshold <- threshold
      self$rate <- rate
    },
    #' The Amount method
    #'
    #' This method determines how much income tax a single person pays depending upon their income.city
    #' @param income The initial income of the person.
    #' @keywords uk income tax
    #' @export
    #' @examples
    #' amount()
    amount = function(income){
      noOfThresholds <- length(self$threshold)
      #Input variable preconditions
      if (noOfThresholds != length(self$rate))
        Stop("threshold and rate vectors must be the same size")
      if (noOfThresholds > 1 && self$threshold != sort(self$threshold))
        Stop("threshold must be in ascending order")
      allowance <- self$personalAllowance$amount(income)
      level <- 1
      tax <- 0
      while (level < noOfThresholds){
        lowerThreshold <- allowance + self$threshold[level]
        upperThreshold <- allowance + self$threshold[level+1]
        if (income > lowerThreshold){
          tax <- tax + (min(upperThreshold,income)-lowerThreshold)*self$rate[level]
        }
        level <- level + 1
      }
      if (income > upperThreshold) {
        tax <- tax + (income-upperThreshold)*self$rate[level]
      }
      tax
    }
  )
)

# Sourced from:
# https://www.gov.uk/guidance/rates-and-thresholds-for-employers-2017-to-2018
incomeTax2017 <- IncomeTax$new(
  personalAllowance = personalAllowance2017,
  threshold = c(0,33500,150000),
  rate = c(0.2,0.4,0.45)
)

# Sourced from:
# https://www.gov.uk/government/publications/rates-and-allowances-income-tax/income-tax-rates-and-allowances-current-and-past
incomeTax2015 <- IncomeTax$new(
  personalAllowance = personalAllowance2015,
  threshold = c(0,31785,150000),
  rate = c(0.2,0.4,0.45)
)

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
VariableRateTax <- R6Class("VariableRateTax",
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

#' The National Insurance class
#'
#' The class captures the notion of UK national insurance
#' UK National insurance is a variable rate tax for both employees and employers
#' however because of the withdrawal of the personal allowance after 100000
#' UK income tax is not though it could be modeled as a variable rate tax.
#' @field employee The variable rate tax structure for employee contributions.
#' @field employer The variable rate tax structure for employee contributions.
#' @keywords UK National Insurance Socail Security
#' @export
#' @examples
#' Income Tax
NationalInsurance <-R6Class("NationalInsurance",
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
UniversalCredit <-R6Class("UniversalCredit",
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
TaxAndBenefits <-R6Class("TaxAndBenefits",
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
