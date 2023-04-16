#' Print function for `decomposition` objects.
#' @param x  Object of type `decomposition`
#' @param ... Currently unused
#' @return Invisibly returns `x` as the function is called for side effects. 
#' @export
#' 
#' @examples 
#' data(housing)
#' # Linear regression &  decompose 
#' fit.lm <- lm(bmi ~ sex + tenure + place + age,data = housing)
#' contrib.lm <- contribution(fit.lm, housing$income)
#' 
#' # print
#' print(contrib.lm)
print.decomposition <-function(x, ...) {
  if (!inherits(x,"decomposition")) stop("Object is not of class decomposition")
  cat("Overall CI:", concentration_index(x$overall_ci), "\n")
  if (x$outcome_corrected) cat("(based on a corrected value)\n")
  cat("\n")
  
  cat("Decomposition:\n")
  result <- data.frame(x$rel_contribution, x$ci_contribution)
  colnames(result) <- c("Contribution (%)", "Absolute contribution")
  result$Corrected <- ""
  result$Corrected[x$corrected_coefficients] <- "yes"
  result$Corrected[!x$corrected_coefficients] <- "no"
  print(result)
  
  # return X since its a base-style plot function (https://adv-r.hadley.nz/functions.html) called for side effects    
  invisible(x)
}

