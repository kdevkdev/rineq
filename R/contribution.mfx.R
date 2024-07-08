#' @importFrom stats weights model.matrix
#' @export
contribution.probitmfx <-
  function(object, ranker, correction = TRUE, type = "CI", intercept ="exclude") {
    
    intercept <- match.arg(intercept, choices = c("exclude", "include"))
    
    # extract y outcome (fit is a glam object, so its not a fitted y but the y used for glm())
    outcome = object$fit$y
    
    # extract the model matrix & coefficients
    # with or without intercept
    if(intercept == "exclude"){
      mm <- model.matrix(object$fit)[, -1, drop= F]
      betas <- object$mfxest[,1]
    }
    else{
      mm <- model.matrix(object$fit)
      betas <-  c("(Intercept)" = NA, object$mfxest[,1]) # no intercept available here
    }
    
    # extract the weights of the glm object
    wt <- object$fit$prior.weight
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction, citype = type)
    return(results)
}
#' @method contribution logitmfx
#' @export
contribution.logitmfx <- contribution.probitmfx