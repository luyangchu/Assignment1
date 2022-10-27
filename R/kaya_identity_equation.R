library(checkmate)
library(roxygen2)
#' create Kaya equation as a function
#'
#' more detailed description.
#'
#' @param pop Numeric
#' @param gdp Numeric
#' @param enInt Numeric
#' @param carnInt Numeric
#' @param output_type String
#'
#' @return Numeric
#'
#' @examples
#' kaya_equation.germany <- kaya_equation(82.4, 44, 5, 0.05)
#'
#' @export
#'
kaya_identity_equation <- function(pop,gdp,enInt,carbInt,output_type = "CO2" ){
  # all the variables are non negative
  # add checkmate to check input
  checkmate::assertNumber(pop, na.ok = FALSE, lower = 0)
  checkmate::assertNumber(gdp, na.ok = FALSE, lower = 0)
  checkmate::assertNumber(enInt, na.ok = FALSE, lower = 0)
  checkmate::assertNumber(carbInt, na.ok = FALSE, lower = 0)

  yearly_CO2 <-  pop * gdp * enInt * carbInt

  # two output types, CO2 and C, default CO2
  # connection:3.67 tonnes of CO2 equals 1 tonne of C
  if (output_type == "C") {
    yearly_C <- yearly_CO2 / 3.67
    return(yearly_C)
  }else {
    return(yearly_CO2)
  }}

