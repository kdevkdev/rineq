#' @export
summary.decomposition <-
function(object, digits = getOption('digits'), ...) {

  ds <- digits
    
  if (!inherits(object,"decomposition")) stop("Object is not of class decomposition")
  cat("Overall CI:", round(concentration_index(object$overall_ci),ds), "\n")
  cat("95% confidence interval:", round(confint(object$overall_ci),ds),"\n")
  if (object$outcome_corrected) cat("(based on a corrected value)\n")
  cat("\n")
  
  cat("Decomposition:\n")
  result <- data.frame(round(object$rel_contribution,ds))
  names(result) <- "Contribution (%)"
  result$`Contribution (Abs)` = round(object$ci_contribution,ds)
  result$Elasticity <- round(object$ci_contribution/c(0,object$partial_cis),ds)
  result$"Concentration Index" <- c(NA, round(object$partial_cis,ds))
  result$"lower 5%" <- c(NA, round(object$confints[1,],ds))
  result$"upper 5%" <- c(NA, round(object$confints[2,],ds))
  result$Corrected <- ""
  result$Corrected[object$corrected_coefficients] <- "yes"
  result$Corrected[!object$corrected_coefficients] <- "no"
  return(result)
}
