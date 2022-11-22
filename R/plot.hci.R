#' @importFrom graphics polygon
#' @export
plot.hci <-
function(x, ...) {
    if (!any(class(x) == 'hci')) stop("Object is not of class hci")
    #myOrder <- order(x$fractional_rank)
    #xCoord <- x$fractional_rank[myOrder]
    #y <- x$outcome[myOrder]
    #cumdist <- cumsum(y) / sum(y)
  
    o = order(x$ineqvar)
    xval = c(0,cumsum(x$ineqvar[o]) / sum(x$ineqvar))
    yval = c(0,cumsum(x$outcome[o]) / sum(x$outcome))
    
    #plot(c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", xlab = "Cumulative share inequality variable", ylab = "Cumulative share of outcome" , 
    #    main = "Concentration Curve", ...)
    #polygon(xCoord, cumdist, col = "light gray", ...)
    plot(c(0,100), c(0,100), type = "l", col = "black", ..., xlab = "Cumulative % inequality variable", ylab = "Cumulative % outcome")
    lines(xval*100, yval*100, col = "gray", lty = "dashed")   
  
  }

