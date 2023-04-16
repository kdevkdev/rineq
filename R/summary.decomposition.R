#' Prints and returns a summary for a `decomposition` object. 
#' @param object Result of a decomposition analysis, of class `decomposition`
#' @param digits Number of digits, defaults to R `digits` option
#' @param addcoefs Whether or not to add coefficients (defaults to `FALSE`)
#' @param ... Additional parameters, currently unused
#' @return  A data frame frame with columns for the absolute and relative contribution, elasticity, concentration index including confidence intervals, 
#' and whether correction was applied. If specified using `addcoefs`, the coefficients are included as the first column. 
#' @importFrom stats confint
#' @export
#' 
#' @examples
#' data(housing)
#' # Linear regression & decompose 
#' fit.lm <- lm(bmi ~ sex + tenure + place + age,data = housing)
#' contrib.lm <- contribution(fit.lm, housing$income)
#' 
#' # print
#' print(contrib.lm)
summary.decomposition <-function(object, digits = getOption('digits'), addcoefs = FALSE, ...) {

  ds <- digits
    
  if (!inherits(object,"decomposition")) stop("object is not of class decomposition")
  cat("Overall CI:", round(concentration_index(object$overall_ci),ds), "\n")
  cat("95% confidence interval:", round(confint(object$overall_ci),ds),"\n")
  if (object$outcome_corrected) cat("(based on a corrected value)\n")
  cat("\n")
  
  cat("Decomposition:\n")
  result <- data.frame(round(object$rel_contribution,ds))
  names(result) <- "Contribution (%)"
  
  if(addcoefs)
    results$Coefficient <- c(NA, object$betas)
  
  result$`Contribution (Abs)` <- round(object$ci_contribution,ds)
  result$Elasticity <- round(c(0,object$elasticities),ds)
  result$"Concentration Index" <- c(NA, round(object$partial_cis,ds))
  result$"lower 5%" <- c(NA, round(object$confints[1,],ds))
  result$"upper 5%" <- c(NA, round(object$confints[2,],ds))
  result$Corrected <- ""
  result$Corrected[object$corrected_coefficients] <- "yes"
  result$Corrected[!object$corrected_coefficients] <- "no"
  print(result)
  invisible(result)
}
