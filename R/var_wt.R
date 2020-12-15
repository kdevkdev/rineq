# weighted variance
var_wt<- function(x, w, na.rm = FALSE) {
  if(na.rm) 
  {
	i <-  !is.na(x)
    x <- x[i]
    w <- w[i]
  }
  s <- sum(w)
  return((sum(w*x^2) * s - sum(w*x)^2) / (s^2 - sum(w^2)))
}