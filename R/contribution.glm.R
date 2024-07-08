#' @importFrom stats predict model.matrix coefficients
#' @export
contribution.glm <-
function(object, ranker, correction = TRUE, type = "CI", intercept = "exclude") {
    
    intercept <- match.arg(intercept,choices = c("exclude", "include"))
  
    # extract the outcome of the glm object
    outcome <- predict(object)

    # extract the weights of the glm object
    wt <- object$prior.weight

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
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction, citype = type)
    return(results)
}
