#' @title Corrects negative values in the health variable
#'
#' @description The Relative Concentration Index is not bonded between [-1,1] if the health variable contains both negative and positive values. This function corrects for this either by imputing a value of 0 for all negative values or by subtracting the minimum value.
#'
#' @usage correct_sign(x, shift = TRUE)
#' 
#' @param x A numeric vector, typically representing health.
#' @param shift If \code{FALSE} (the default), 0 is imputed for all negative values in \code{x}. If \code{TRUE} the minimum value of \code{x} is subtracted from it.
#'
#' @return Returns a list with 2 components:
#' \itemize{
#' \item{corrected}{corrected version of \code{x}}
#' \item{modified}{logical, \code{TRUE} when any of the elements of \code{x} have been changed}
#' }
#' These components can be extracted with the functions \code{correctedValue} and \code{isCorrected}.
#' 
#' @author Peter Konings
#' 
#' @export
#'
#' @examples
#' data(housing)
#'
#' # standardize & normalize bmi, will introduce negative values
#' housing$zbmi <- (housing$bmi - mean(housing$bmi))/ sd(housing$bmi)
#'
#' housing$zbmi.shifted <- corrected_value(correct_sign(housing$zbmi, shift = TRUE))
#' housing$zbmi.imputed <- corrected_value(correct_sign(housing$zbmi, shift = FALSE))
#' 
#' ## compare the effect of both methods
#' plot(density(housing$zbmi, na.rm = TRUE))
#' points(density(housing$zbmi.shifted, na.rm = TRUE), col = 'blue')
#' points(density(housing$zbmi.imputed, na.rm = TRUE), col = 'green')
#' 
correct_sign <-
function(x, shift = TRUE) {
    # we have to check for two things here:
    # 1/ sign(0) = 0, but this does not pose a problem, so leave them out of the test
    # 2/ NA should remain NA; since sign(NA) = NA we leave them out of the test
    # if the signs of the remaining vector are all the same, there will be only 1 unique value
    # if not, there will be two unique values.
	modified <- FALSE
	if (length(unique(sign(x[(sign(x) != 0) & (!is.na(x))]))) > 1)
	{
		if(isTRUE(shift))
		{
			# subtract the minimum, taking care about possible NAs in the vector...
			myOffset <- min(x, na.rm = TRUE)
			x <- x - myOffset
		} else {
			x[x < 0] <- 0
		}
		# throw a warning that the variable has been changed
		modified <- TRUE
	}
    # return the original vector if everything is ok, otherwise the modified one.
    return(list(correctedx = x, modified = modified))
}

