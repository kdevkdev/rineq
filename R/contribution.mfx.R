#' @method contribution probitmfx
#' @importFrom stats weights model.matrix
#' @export
contribution.probitmfx <-
  function(object, ranker, correction = TRUE, type = "CI") {
    # The ranking variable (wealth, income,...) should be given explicitely.
    # Throw an error if this is not a numeric one
    if (!inherits(ranker, "numeric")) stop("Not a numeric ranking variable")
    
    
    # extract youtcome
    outcome = object$fit$y
    

    # extract the model matrix of the svyglm object
    mm <- model.matrix(object$fit)
    
    # extract the weights of the glm object
    wt <- object$fit$prior.weight
    
    # retrieve the marginal effects
    betas <- object$mfxest[,1]
    
    # call the backend decomposition function
    results <- decomposition(outcome, betas, mm, ranker, wt, correction, citype = type)
    return(results)
}
#' @method contribution logitmfx
#' @export
contribution.logitmfx <- contribution.probitmfx