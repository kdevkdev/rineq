#' Prints the a summary of the concentration index object `hci`
#' 
#' @param object  Object of type `hci`
#' @param ... Currently unused
#' @return No returns value. Directly prints to the standard output connection. 
#' @importFrom stats confint
#' @export
#' 
#' @examples
#' data(housing)
#' ci.bmi <- ci(ineqvar = housing$income, outcome = housing$bmi, method = "direct")
#' summary(ci.bmi)
#' 
summary.hci <- function(object, ...) {
    if (!inherits(object,'hci')) stop("Object is not of class hci")
    cat("Call:\n")
    print(object$call)
    cat("\nType of Concentration Index:\n", object$type,"\n")
    cat("\nHealth Concentration Index:\n", object$concentration_index,"\n\n")
    cat("Variance:\n", object$variance, "\n\n")
    cat("95% Confidence Interval:\n", confint(object), "\n")
}
