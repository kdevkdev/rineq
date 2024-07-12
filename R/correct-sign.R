#' @title Corrects negative values in the health variable
#'
#' @description The Relative Concentration Index is not bonded between \eqn{[-1,1]} if the health variable contains both negative and positive values. This function corrects for this either by imputing a value of 0 for all negative values or by subtracting the minimum value.
#'
#' @usage correct_sign(x, shift = TRUE)
#' 
#' @param x A numeric vector, typically representing health.
#' @param shift If `FALSE` (the default), 0 is imputed for all negative values in `x`. If `TRUE` the minimum value of `x` is subtracted from it.
#'
#' @return `correct_sign()` returns a list with 2 components:
#' \describe{
#' \item{corrected}{corrected version of `x`}
#' \item{modified}{logical, `TRUE` when any of the elements of `x` have been changed}}
#' `corrected_value()` returns the corrected value if passed the result of `correct_sign().
#' 
#' `is_corrected()` returns `TRUE` if a modifications was made if passed the result of `correct_sign()`, `FALSE` otherwise.
#' 
#' These components can be extracted with the functions `corrected_value` and `is_corrected`.
#' 
#' @author Peter Konings
#' 
#' @export
#' @examples
#' data("housing")
#'
#' # standardize & normalize bmi, will introduce negative values
#' housing$bmi.std <- (housing$bmi - mean(housing$bmi))/ sd(housing$bmi)
#'
#' housing$bmi.std.shifted <- corrected_value(correct_sign(housing$bmi.std, shift = TRUE))
#' housing$bmi.std.imputed <- corrected_value(correct_sign(housing$bmi.std, shift = FALSE))
#' 
#' ## compare the effect of both methods
#' plot(density(housing$bmi.std, na.rm = TRUE))
#' points(density(housing$bmi.std.shifted, na.rm = TRUE), col = 'blue')
#' points(density(housing$bmi.std.imputed, na.rm = TRUE), col = 'green')
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
#' @describeIn correct_sign Return the corrected value
#' @export
corrected_value <-  function(x) {
    return(x$correctedx)
}
#' @describeIn correct_sign Check if the sign was corrected
#' @export
is_corrected <-  function(x) {
    return(x$modified)
}

