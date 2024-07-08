#' @title Decomposition analysis
#' 
#' @description Used by the wrapper [contribution()] but can be used manually. Calculates the decomposition for a given regression model. 
#' @details NOTE: Only models with data with ordinary indexes are supported (starting from 1, sequentially increasing by increments of 1). For the case were rows with `NA` are automatically omitted by the model function, the used indices are guessed based on the row names of the model matrix and then used for accessing the `ranker` variable. However, this may lead to issues if the row names do not correspond to ordinary integer indexes. 
#' 
#' @param outcome Outcome variable
#' @param betas  Beta coefficients from regression. 
#' @param mm Model matrix from regression
#' @param ranker Ranking variable
#' @param wt Weights
#' @param correction Apply sign correction? 
#' @param citype Character, CI type to be calculated, defaults to `CI`. Use `CIw` for binary outcomes. 
#'
#' @return S3 object of class decomposition
#' @importFrom stats confint
#' 
#' @examples
#' fit.lm = lm(mtcars$mpg ~ mtcars$cyl)
#' decomp = decomposition(mtcars$mpg, coefficients(fit.lm), fit.lm$model, 
#'                        mtcars$hp, wt = rep(1, nrow(mtcars)), correction = FALSE) 
#' summary(decomp)
#' 
#' @export
decomposition <- function(outcome, betas, mm, ranker, wt, correction, citype = "CI") {    
  
  
    # The ranking variable (wealth, income,...) should be given explicitly
    # Throw an error if it is not numeric or integer
    stopifnot("Ranking variable is not numeric" = inherits(ranker, "numeric") || inherits(ranker, "integer"))
  
  
    # define an index vector for the rows that are actually used in the model, rownames are strings that should be converted to integer
    rows <- as.numeric(rownames(mm))
    
    if(any(is.na(ranker[rows])) || length(rows) != length(ranker)){
      stop("Rownames of modelmatrix are invalid ranker indeces, rerun the model with data with ordinary integer indeces as row names") 
    } 
              

    # correct the sign for the partial outcomes when requested
    corrected <- rep(FALSE, ncol(mm))
    outcome_corrected <- FALSE
    if (correction) {
        temp <- apply(mm, 2, correct_sign)
        mm <- sapply(temp, corrected_value)
        corrected <- sapply(temp, is_corrected)
        temp <- correct_sign(outcome)
        outcome <- corrected_value(temp)
        outcome_corrected <- is_corrected(temp)
	}
    
    # calculate the partial CI for every variable in the model matrix
    # weights are given by the weights of the model
    # use only the observations of the ranking variable that are actually used in the model
    # the intermediate cis returns a list with objects of class hci
    # indices stores the values of the concentration index only

    cis <- apply(mm, 2, ci, ineqvar = ranker[rows], weights = wt, type = citype)
    indices <- sapply(cis, concentration_index)
    confints <- sapply(cis, confint)
    
    # calculate the weighted average for every variable in the model matrix
    averages <- apply(mm, 2, weighted.mean, w = wt)
    
    # calculate the weighted mean of the outcome
    mu <- weighted.mean(outcome, w = wt)
    
    # calculate the sum off all partial contributions
    elasticities <- betas * averages / mu
    contributions <- elasticities * indices
    sumOfContributions <- sum(contributions)
    
    # calculate the residual CI: first, calculate the overall CI
    # between the overall wealth variable (using only the observations used in the model)
    # and the health outcome; use the sampling weights used in the model
    # then subtract the sum of all partial contributions
    CIoverall <- ci(ranker[rows], as.numeric(outcome), wt, type = citype)
    CIresid <- concentration_index(CIoverall) - sumOfContributions

    # add the residual CI to the contributions vector and name it
    contributions <- c(CIresid, contributions)
    names(contributions)[1] <- "residual"
    
    # calculate the % contribution
    pctcontrib <- contributions/sum(contributions) * 100
    
    # return the result
    results <-
	list(betas = betas,
	     partial_cis = indices,
		 confints = confints,
		 averages = averages, 
		 rel_contribution = pctcontrib,
		 ci_contribution = contributions, 
		 elasticities = elasticities,
		 overall_ci = CIoverall,
		 corrected_coefficients = corrected, 
		 outcome_corrected = outcome_corrected,
		 rows = rows)
    class(results) <- "decomposition"
    
    return(results)   
}
