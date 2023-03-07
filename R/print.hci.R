#' Prints an `hci` object.
#' @param x  Object of type `hci`
#' @param ... Currently unused
#' @return Invisibly returns `x` as the function is called for side effects. 
#' @export
print.hci <-
function(x, ...) {
  if (!inherits(x,"hci")) stop("Object is not of class hci")
  print(x$concentration_index)
  
  # return X since its a base-style plot function (https://adv-r.hadley.nz/functions.html) called for side effects    
  invisible(x)
}

