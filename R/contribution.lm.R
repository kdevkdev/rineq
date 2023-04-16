#' @importFrom stats weights model.matrix coefficients
#' @export
contribution.lm <-
function(object, ranker, correction = TRUE, type = "CI", intercept = "exclude") {
  

    # The ranking variable (wealth, income,...) should be given explicitly.
    # Throw an error if this is not a numeric one
    if (!inherits(ranker, "numeric")) stop("Not a numeric ranking variable")
    
    intercept <- match.arg(intercept, choices = c("exclude", "include"))
      
    # extract the outcome of the lm object - assume its the first column
    outcome <- object$model[,1]
    
    # extract the model matrix & coefficients
    # with or without intercept
    if(intercept == "exclude"){
      mm <- model.matrix(object)[, -1, drop= F]
      betas <- coefficients(object)[-1]
    }
    else{
      mm <- model.matrix(object)
      betas <- coefficients(object)
    }
      
    # extract the weights of the lm object
    if (!is.null(weights(object))) {
      wt <- weights(object) } else {
        wt <- rep(1, nrow(mm))
      }   
    
    # call the underlying decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction, citype = type)
    return(results)
}

