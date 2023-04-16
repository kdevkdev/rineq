#' Confidence intervals for `hci` objects
#' @importFrom stats qnorm
#' @param object An object of class `hci`
#' @param level Confidence interval level defaults to `0.95`
#' @param parm Unused
#' @param ... Unused
#' @return A confidence interval in a numeric vector of length 2
#' @export
#' @examples 
#' data(housing)
#' ci.bmi <- ci(ineqvar = housing$income, outcome = housing$bmi, method = "direct")
#' confint(ci.bmi)
#' 
confint.hci <-
function(object, parm = NULL, level = 0.95, ...) {
  if (!inherits(object, 'hci')) stop("Object is not of class hci")
  parm <- NULL
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  fac <- qnorm(a)
  ci <- object$concentration_index + fac * sqrt(object$variance) 
  return(ci)
}