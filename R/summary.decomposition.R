#' Summary for decomposition 
#' @param object Result of a decomposition
#' @param digits Number of digits, defaults to R `digits` option
#' @param addcoefs Wether or not to add coefficients (defaulst to `FALSE`)
#' @param ... Additional parameters, currently unused
#' @importFrom stats confint
#' @export
summary.decomposition <-
function(object, digits = getOption('digits'), addcoefs = FALSE, ...) {

  ds <- digits
    
  if (!inherits(object,"decomposition")) stop("Object is not of class decomposition")
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
  return(result)
}
