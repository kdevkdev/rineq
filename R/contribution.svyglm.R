#' @importFrom stats model.matrix coefficients
#' @export
contribution.svyglm <-
function(object, ranker, correction = TRUE, type = "CI", intercept = "exclude") {
    # The ranking variable (wealth, income,...) should be given explicitely.
    # Throw an error if this is not a numeric one
    if(!inherits(ranker,"numeric")) stop("Not a numeric ranking variable")
  
    intercept = match.arg(intercept, c("exclude", "include"))
    
    # extract the outcome of the svyglm object
    outcome <- predict(object, newdata = object$model[,-1], vcov = FALSE)
    
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
    
    # extract the weights of the svyglm object
    wt <- 1 / object$survey.design$prob
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction, citype = type)
    return(results)
}
