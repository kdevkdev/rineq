#' Calculates the weighted rank
#'
#' @param x numeric vector
#' @param wt weights 
#'
#' @author Peter Konings
#' @references Kakwani \emph{et al}., 1997.
#' @return A numeric vector containing weighted fractional ranks of the elements of \code{x}.
#' 
#' @export 
#' @examples
#' x <- sample(1:10, size = 10, replace = TRUE)
#' x.weight <- seq(0, 1, length.out = 10)
#' rank_wt(x, wt = x.weight)
rank_wt <- function(x, wt) {
  n <- length(x)
  r <- vector(length = n)
  
  ## weighted fractional rank, see van Doorslaer & Koolman (2004)
  ## since population weights can be >1, rescale so that sum(wt) = n
  myOrder <- order(x)
  
  wt[myOrder] <- wt[myOrder] / sum(wt)
  
  ## calculate the fractional rank and return it in the original order of the variable
  ## in the first term, we use a modified vector of weights starting with 0 and the last value chopped off
  ## see Lerman & Yitzhaki eq. (2)
  ## a loop is avoided by using the cumulative sum
  r[myOrder] <-
    (c(0, cumsum(wt[myOrder[-length(myOrder)]])) + wt[myOrder]/2)
  
  ## return object
  return(r)
}