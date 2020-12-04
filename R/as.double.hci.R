as.double.hci <-
  function(object, ...) {
    if (!any(class(object) == 'hci')) stop("Object is not of class hci")

    object$concentration_index
  }
