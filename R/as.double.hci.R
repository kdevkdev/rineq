as.double.hci <-
  function(x, ...) {
    if (!any(class(x) == 'hci')) stop("Object is not of class hci")

    x$concentration_index
  }
