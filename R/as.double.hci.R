as.double.hci <-
  function(x, ...) {
    if (!inherits(x,'hci')) stop("Object is not of class hci")

    x$concentration_index
  }
