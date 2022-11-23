#' @export
print.hci <-
function(x, ...) {
  if (!inherits(x,"hci")) stop("Object is not of class hci")
  print(x$concentration_index)
}

