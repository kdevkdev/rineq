#' Plots a barplot of the contribution percentages in a `decomposition` object. Sets custom plot margins and uses the graphical parameters `xlim`, `horiz`, `las` and `xlab` which therefore cannot be customized 
#' 
#' @importFrom graphics barplot 
#' @importFrom graphics par 
#' @importFrom graphics strwidth
#' @param decreasing Whether to sort contributions decreasing or not
#' @param x Object returned from decomposition function
#' @param horiz If the barplots should be printed horizontally or vertically
#' @param ...  Graphical parameter passed on to `base::barplot()`
#' @return Invisibly returns `x` as the function is called for side effects (plotting). 
#' @export
#' 
#' @examples
#' data(housing)
#' # Linear regression & decompose 
#' fit.lm <- lm(bmi ~ sex + tenure + place + age,data = housing)
#' contrib.lm <- contribution(fit.lm, housing$income)
#' 
#' # plot horizontally, in increasing order
#' plot(contrib.lm, decreasing = FALSE, horiz = TRUE)
plot.decomposition <-
  function(x, decreasing = TRUE, horiz = FALSE, ...) {
    if(!inherits(x,"decomposition")) stop("Object is not of class decomposition")
    
    # safeguard against preemtive exist to restore the user's value
    old_par = par(list("cex.axis", "mai"))
    on.exit(par(old_par))
    
    # somewow base limits fail to be nice, calculate our own (horiz steers if bars are horizontal)
    lims = range(pretty(x$rel_contribution))
    
    
    
    # set soem pars
    par(cex.axis = 1)
    
    # needed tick label width
    tick_width <- max(strwidth(names(x$rel_contribution), units  = "inches"))
    
    if( horiz == F ){
      par(mai = c(tick_width+0.45,0.9,0.82,0.42))
      barplot(sort(x$rel_contribution, decreasing = decreasing),ylim=lims, horiz = FALSE, las = 2,ylab = "Proportion",...)
    }
    else{
      par(mai = c(0.9,tick_width+0.45,0.82,0.42))
      barplot(sort(x$rel_contribution, decreasing = decreasing),xlim=lims, horiz = TRUE, las = 2, xlab = "Proportion",...)
    }
    
    # return X since its a base-style plot function (https://adv-r.hadley.nz/functions.html)
    invisible(x)
  }