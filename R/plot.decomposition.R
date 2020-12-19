#' @importFrom graphics barplot 
#' @export
plot.decomposition <-
function(x, decreasing = TRUE, ...) {
  if (class(x) != "decomposition") stop("Object is not of class decomposition")
  barplot(sort(x$rel_contribution, decreasing = decreasing),...)
}

