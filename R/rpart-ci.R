#' Calculates the weighted rank
#'
#' @param x numeric vector
#' @param wt weights 
#'
#' @author Peter Konings
#' @references Kakwani \emph{et al}., 1997.
#' @return A numeric vector containing weighted fractional ranks of the elements of \code{x}.
#' 
#' @export 
#' @examples
#' x <- sample(1:10, size = 10, replace = TRUE)
#' x.weight <- seq(0, 1, length.out = 10)
#' rank_wt(x, wt = x.weight)
rank_wt <- function(x, wt) {
  n <- length(x)
  r <- vector(length = n)

  ## weighted fractional rank, see van Doorslaer & Koolman (2004)
  ## since population weights can be >1, rescale so that sum(wt) = n
  myOrder <- order(x)

  wt[myOrder] <- wt[myOrder] / sum(wt)

  ## calculate the fractional rank and return it in the original order of the variable
  ## in the first term, we use a modified vector of weights starting with 0 and the last value chopped off
  ## see Lerman & Yitzhaki eq. (2)
  ## a loop is avoided by using the cumulative sum
  r[myOrder] <-
    (c(0, cumsum(wt[myOrder[-length(myOrder)]])) + wt[myOrder]/2)

  ## return object
  return(r)
}


## Concentration Index ----------------------------------------------------#
CI <- function(y, wt) {
  # this aims to have a vector of goodness of length (n-1) as required. 
  # If not specified, CI cannot be calculated for a single-subject node
  # and the vector of goodness is n-2
  if (length(y) == 2) {
    ci <- 0

  } else {
    y[, 1] <- rank_wt(y[, 1], wt)
    ci <- abs(2 / weighted.mean(y[,2],wt) *
              cov.wt(as.matrix(data.frame(y[, c(1, 2)])), wt = wt)$cov[1,2])
  }	

  ## return object
  return(ci)
}

## Generalized Concentration Index ----------------------------------------#
## see Clarke and al 2002
CIg  <- 
function(y, wt) {
  if (length(y) == 2) {
    cig <- 0

  } else {
      y[, 1] <- rank_wt(y[, 1], wt)
      cig <- abs(2 * cov.wt(as.matrix(data.frame(y[, c(1, 2)])), wt = wt)$cov[1,2])
  }

  return(cig)
}


## Concentration Index with Erreygers Correction --------------------------#
## see Erreygers (2009)
CIc <- function(y, wt) {         
  if (length(y) == 2) {
    cic <- 0

  } else {
    y[, 1] <- rank_wt(y[, 1], wt)
    cic <- 4 * (abs(2 * cov.wt(as.matrix(data.frame(y[, c(1, 2)])), wt = wt)$cov[1,2])) / (max(y[, 2], na.rm = TRUE) - min(y[, 2], na.rm = TRUE))
  }

  return(cic)
}



## The 'evaluation' function ----------------------------------------------#
## Called once per node
##  Produce a label (1 or more elements long) for labeling each node,
##  and a deviance.  The latter is
##	- of length 1
##       - equal to 0 if the node is "pure" in some sense (unsplittable)
##       - does not need to be a deviance: any measure that gets larger
##            as the node is less acceptable is fine.
##       - the measure underlies cost-complexity pruning, however
eval_ci  <-  function(y, wt, parms) {
  CI_fun <-
  switch(parms,
         "1" = CI,
         "2" = CIg,
         "3" = CIc)

  wmean <- weighted.mean(y[, 2], wt) 
  ci <- CI_fun(y, wt)
  list(label = wmean, deviance = ci)
}


## The 'split' function ---------------------------------------------------#
## Called once per split variable per node
## If continuous=T
##   The actual x variable is ordered
##   y is supplied in the sort order of x, with no missings,
##   return two vectors of length (n-1):
##      goodness = goodness of the split, larger numbers are better.
##                 0 = couldn't find any worthwhile split
##        the ith value of goodness evaluates splitting obs 1:i vs (i+1):n
##      direction= -1 = send "y< cutpoint" to the left side of the tree
##                  1 = send "y< cutpoint" to the right
##         this is not a big deal, but making larger "mean y's" move towards
##         the right of the tree, as we do here, seems to make it easier to
##         read
## If continuous=F, x is a set of integers defining the groups for an
##   unordered predictor.  In this case:
##       direction = a vector of length m= "# groups".  It asserts that the
##           best split can be found by lining the groups up in this order
##           and going from left to right, so that only m-1 splits need to
##           be evaluated rather than 2^(m-1)
##       goodness = m-1 values, as before.
##
## The reason for returning a vector of goodness is that the C routine
##   enforces the "minbucket" constraint. It selects the best return value
##   that is not too close to an edge.
split_ci <- function(y, wt, x, parms, continuous) {

  CI_fun <-
  switch(parms,
         "1" = CI,
         "2" = CIg,
         "3" = CIc)

  if (continuous) {
    n <- nrow(y) 
    cir <- double(n-1)        
    cil <- double(n-1)
    wmeanr <- double(n-1)
    wmeanl <- double(n-1)
    goodness <- double(n-1) 
    for (i in 2:n) {
      indx <- i:n
      cir[i] <- CI_fun(y[indx,1:2],wt[indx])
      cil[i] <- CI_fun(y[-indx,1:2],wt[-indx])
      wmeanr[i] <- sum(wt[indx] * x[indx])/sum(wt[indx])
      wmeanl[i] <- sum(wt[-indx] * x[-indx])/sum(wt[-indx])
    }
    cir <- cir[-1]
    cil <- cil[-1]
    wmeanr <- wmeanr[-1]
    wmeanl <- wmeanl[-1]
    ci <- CI_fun(y[,1:2],wt)
    goodness <- ci-((cil*(1:(n-1))/n)+(cir*((n-1):1)/n)) 
    goodness[goodness<0] <- 0
    list(goodness= goodness, direction=sign(wmeanl-wmeanr))

  } else {
    # Categorical X variable
    n <- nrow(y) 
    ux <- sort(unique(x))
    m <- length(ux)	
    cir <- double(m-1)        
    cil <- double(m-1)
    nr <- double(m-1)        
    nl <- double(m-1)
    goodness <- double(m-1) 

    for(i in 2:m) {
      indx <- i:m
      cir[i] <- CI_fun(y[x %in% ux[indx],1:2],wt[x %in% ux[indx]])
      cil[i] <- CI_fun(y[x %in% ux[-indx],1:2],wt[x %in% ux[-indx]])
      nl[i] <- length(y[x %in% ux[indx],2])
      nr[i] <- length(y[x %in% ux[-indx],2])
    }

    cir <- cir[-1]
    cil <- cil[-1]
    nl <- nl[-1]
    nr <- nr[-1]
    ci <- CI_fun(y[,1:2],wt)
    goodness <- ci-((cil*nl/n)+(cir*nr/n)) 
    goodness[goodness<0] <-  0 
    list(goodness = goodness, direction = ux)
  }
}
	

## The 'init' function ---------------------------------------------------#
##   fix up y to deal with offsets
##   return a dummy parms list
##   numresp is the number of values produced by the eval routine's "label"
##   numy is the number of columns for y
##   summary is a function used to print one line in summary.rpart
init_ci <- function(y, offset, parms = c("CI", "CIg", "CIc"), wt) {
  if (!is.matrix(y)) stop("y must be a matrix")
  if (!ncol(y) == 2) stop("y should have 2 columns")

  parms <- match.arg(parms)
  parms <- which(parms == c("CI", "CIg", "CIc"))

  sfun <-
  function(yval, dev, wt, ylevel, digits) {
   paste("Deviance (concentration index)=",format(signif(dev,2)),
			"; Mean=",signif(yval,2))
  }

  text <-
  function(yval,dev, wt, ylevel, digits, n, use.n) {
		if(!use.n) {paste("Mean = ",signif(yval,2))}
		else{paste("\n","\n","\n","\n","\n","N = ",n,"\n",
				"Mean = ",signif(yval,2),"\n", 
				"C = ", signif(dev,2))
   }
  }

  list(y = y, parms = parms,
       numresp = 1, numy = 2, summary = sfun, text = text)
}


## wrapper function: rpart + concentration index --------------------------#

#' Recursive Partitioning and Regression Trees using Concentration Index
#' @title Recursive Partitioning and Regression Trees Using the Concentration Index 
#'
#' @usage rpart_ci(formula, data, weights, type = c("CI", "CIg", "CIc"), 
#'   subset, na.action = na.rpart, model = FALSE, x = FALSE, 
#'   y = TRUE, control, cost, ...) 
#'
#' @param formula A \code{\link{formula}}, with y as a two columns object (the first one must be the wealth variable and the second one the health outcome variable) and no interaction terms. If it's a data frame that is taken as the model frame, see \code{\link{model.frame}}.
#' @param data An optional data frame in which to interpret the variables named in the formula.
#' @param weights Optional case weights
#' @param type One of \code{"CI"}, \code{"CIg"}, \code{"CIc"}. If type is missing, "CI" is chosen as the default action. \code{"CI"} corresponds to the Concentration Index, \code{"CIg"} to Generalized Concentration Index (see Clarke and al, 2002) and \code{"CIc"} to Corrected Concentration Index (see Erreygers, 2009).
#' @param subset Optional expression saying that only a subset of the rows of the data should be used in the fit.
#' @param na.action The default action deletes all observations for which y is missing, but keeps those in which one or more predictors are missing.
#' @param model If logical: keep a copy of the model frame in the result? If the input value for model is a model frame (likely from an earlier call to the rpart function), then this frame is used rather than constructing new data.
#' @param x Keep a copy of the x matrix in the result.
#' @param y keep a copy of the dependent variable in the result. If missing and model is supplied this defaults to FALSE.
#' @param control A list of options that control details of the \code{rpart} algorithm.  See \code{\link{rpart.control}}.
#' @param cost A vector of non-negative costs, one for each variable in the model. Defaults to one for all variables. These are scalings to be applied when considering splits, so the improvement on splitting on a variable is divided by its cost in deciding which split to choose.
#' @param ... Arguments to \code{rpart.control} may also be specified in the call to \code{rpart_cil}. They are checked against the list of valid arguments.
#'
#' @return An object of class \code{rpart}. See \code{\link{rpart.object}}.
#'
#' @references Breiman L., Friedman J. H., Olshen R. A., and Stone, C. J. (1984) \emph{Classification and Regression Trees.} Wadsworth.
#' @author Saveria Willimes and Brecht Devleesschauwer, adapting from the \code{\link{rpart}} package by Terry M.Therneau and Beth Atkinson.
#' 
#' 
#' @seealso \code{\link{imp}}, \code{\link{rpart.control}}, \code{\link{rpart.object}}, \code{\link{summary.rpart}}, \code{\link{print.rpart}}
#' 
#' @import stats
#' @import rlang
#' @import rpart
#' @export 
#' @examples
#' data(nigeria)
#' 
#' tree <-
#'   rpart_ci(
#'     cbind(wealth, zscore1) ~
#'       quintile + ed + rural + region + male + bord + agechild + agemother,
#'     data = nigeria,
#'   weights = nigeria$weight,
#'     type = "CI")
#' 
#' plot(tree, xpd = NA)
#' text(tree, use.n = TRUE)
#' 
rpart_ci <- function(formula, data, weights, type = c("CI", "CIg", "CIc"),
  subset, na.action = na.rpart, model = FALSE, x = FALSE, y = TRUE,
  control, cost, ...) {

  ## check 'type'
  type <- match.arg(type)

  ## define 'method'
  method_ci <- list(eval = eval_ci, split = split_ci, init = init_ci)

  ## fetch list of arguments
  args <- as.list(match.call())[-1]

  ## add 'concentration index' methods
  args$method <- method_ci

  ## add 'type' to 'parms'
  args$parms <- type
  args$type <- NULL

  ## call 'rpart()'
  out <- do.call("rpart", args)

  ## return rpart output
  return(out)
}


