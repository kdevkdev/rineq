#' @importFrom graphics barplot 
#' @export
plot.decomposition <-
function(x, decreasing = TRUE, horiz = F, ...) {
  if (class(x) != "decomposition") stop("Object is not of class decomposition")
  
  # somewow base limits fail to be nice, calculate our own (horiz steers if bars are horizontal)
  lims = range(pretty(x$rel_contribution))
    
  if( horiz == F )
    barplot(sort(x$rel_contribution, decreasing = decreasing),ylim=lims, horiz = F, ...)
  else
    barplot(sort(x$rel_contribution, decreasing = decreasing),xlim=lims, horiz = T, ...)
}

