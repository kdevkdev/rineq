variance_concentration_index <-
function(object) {
    if (!inherits(object,'hci')) stop("Object is not of class hci")
    return(object$variance)
}
