#' Calculates the weighted variance
#'
#' @param x numeric vector
#' @param wt weights 
#' @param na.rm If `TRUE`, indices where `x` is NA will be removed
#'
#' @return A numeric vector containing weighted variance of the elements of `x`
#' 
#' @examples
#' x <- sample(1:10, size = 10, replace = TRUE)
#' x.weight <- seq(0, 1, length.out = 10)
#' var_wt(x, wt = x.weight)
#' @export 
var_wt<- function(x, wt, na.rm = FALSE) {
  if(na.rm) 
  {
	i <-  !is.na(x)
    x <- x[i]
    wt <- wt[i]
  }
  s <- sum(wt)
  return((sum(wt*x^2) * s - sum(wt*x)^2) / (s^2 - sum(wt^2)))
}