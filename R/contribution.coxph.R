#' @importFrom stats model.matrix weights coefficients
#' @export
contribution.coxph <-
function(object, ranker, correction = TRUE, type = "CI", intercept = "exclude") {
    
    # extract the outcome of the coxph object
    outcome <- object$linear.predictor
    
    intercept <- match.arg(intercept, choices = c("exclude", "include"))
    
    # extract the weights of the coxph object -> in case of NULL (none provided) generate unit weights
    wt <- weights(object)
    
    if(is.null(wt))
      wt <- rep(1, length(outcome))
    
    # extract the model matrix & coefficients
    # with or without intercept
    if(intercept == "exclude"){
      # extract the model matrix of the coxph object
      mm <- model.matrix(object)[,names(object$coefficients)][, -1, drop=F]
      betas <- coefficients(object)[-1]
    }
    else{
      # extract the model matrix of the coxph object
      mm <- model.matrix(object)[,names(object$coefficients)]
      betas <- coefficients(object)
    }  
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction, citype = type)
    return(results)
}

