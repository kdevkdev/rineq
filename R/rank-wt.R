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
rank_wt <- function(x, wt){
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
#' Generalized weighted ranking function
#' @param x  numeric vector
#' @param wt weights
#'
#' @description In the case of ties, the ordinary `rank_wt()` function uses the order in the original data. 
#' This is the same approach as in the stata code provided by O’Donnell et al. (2008) in the original World Bank publication, 
#' but depends on the arbitrary initial order in the data 
#' The Stat conindex code however uses uses the generalized weighted rank implementation published by van Ourti (2004). For Stata compatibility use `rank_gwt()`
#' 
#' @details The formula notation in van Ourti (2004) seems to rely on absolute an absolute deduction of 1 unit of monetary income value. This only works
#' in the integer case. Instead, this this implementation uses the next lowest `x` value, respectively the next lowest rank, to calculate the 
#' proportion of the inequality variable up to the respective value
#'
#' @references van Ourti, T., 2004.Measuring horizontal inequity in Belgian health care using a Gaussian random e¡ects two part count data model. Health Economics, 13: 705–724.
#' @return A numeric vector containing weighted fractional ranks of the elements of \code{x}.
#' @export
#'
#' @examples
#' x <- sample(1:10, size = 10, replace = TRUE)
#' x.weight <- seq(0, 1, length.out = 10)
#' rank_gwt(x, wt = x.weight)
rank_gwt <- function(x, wt){  

  n = length(x)
  rank_order <- order(x)

  # order and rescale weights
  wt <- wt[rank_order]
  x  <- x[rank_order]
  
  # three times order/rank is not optimal, but the subsequent ranks should encounter a sorted vector
  rank_rank_min   <- rank(x, ties.method = "min") # position of minimum value for ties
  rank_rank_max   <- rank(x, ties.method = "max") # position of minimum value for ties
  
  cs <- cumsum(wt) 
  q <- cs[rank_rank_max] # calculate proportion with at least x_i (using weighted proportions)
  

  # shifted version - proportion for at least  x_l, where x_l is the next lowest value for position i
  q_s <- c(0, cs)[rank_rank_min]
  
  # calculate normalized generalized rank and return
  rg <- (q_s + 0.5*(q-q_s))/sum(wt)
  
  # return in original order: to sort back we need to take order again
  rg[order(rank_order)]
}
