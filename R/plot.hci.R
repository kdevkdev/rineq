#' @importFrom graphics polygon
#' @export
plot.hci <-
function(x, ...) {
    if (!any(class(x) == 'hci')) stop("Object is not of class hci")
    myOrder <- order(x$fractional_rank)
    xCoord <- x$fractional_rank[myOrder]
    y <- x$outcome[myOrder]
    cumdist <- cumsum(y) / sum(y)
    
    plot(c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", xlab = "fractional rank", ylab = "cumulative distribution" , 
        main = "Health Concentration Curve", ...)
    polygon(xCoord, cumdist, col = "light gray", ...)
}

