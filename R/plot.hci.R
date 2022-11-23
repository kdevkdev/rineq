#' @importFrom graphics lines
#' @export
plot.hci <-
function(x, ...) {
    if (!inherits(x,'hci')) stop("Object is not of class hci")
  
    o = order(x$ineqvar)
    xval = c(0,cumsum(x$ineqvar[o]) / sum(x$ineqvar))
    yval = c(0,cumsum(x$outcome[o]) / sum(x$outcome))
    
    plot(c(0,100), c(0,100), type = "l", col = "black", ..., xlab = "Cumulative % inequality variable", ylab = "Cumulative % outcome")
    lines(xval*100, yval*100, col = "gray", lty = "dashed")   
  
  }

