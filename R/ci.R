#' Calculates different type of concentration indexes
#'
#' This function calculates the relative concentration index (Kakwani et al.), the generalized concentration index (Clarke et al., 2002), the Wagstaff index for bounded variables (Owen et al. 2016), and the concentration index with Erreygers' correction (Erreygers et al., 2009).  It returns an object of class `hci` for which confidence intervals, summaries and plots are defined.
#'
#' @param ineqvar Used for ranking, usually relates to the socioeconomic position, for example income. 
#' @param outcome The variable in which the inequality should be measures, for example health. 
#' @param weights Optional, used to weigh the observations. Defaults to equal weights for all observations. 
#' @param type Character, the type of concentration index to be calculated: relative concentration index (`CI`, default), generalized concentration index (`CIg`), concentration index with Erreygers Correction `CIc`, or Wagstaff concentration index suitable for bounded and binary outcomes `CIw`
#' @param method Character, defines the calculation method. One of:  
#'   * `linreg_delta`: Based on linear regression without transforming the left hand side variable. Computes correct standard errors that take into account the sampling variability of the estimate of the mean (O’Donnell et al. 2008, Owen et al. 2016)    
#'   * `linreg_convenience`): Based on simpler regression with transformed left hand side variable. Standard errors do not take into account the sampling variability of the estimate of the mean(O’Donnell et al. 2008, Owen et al. 2016)
#'   * `cov_convenience`: Based on covariance. Equivalent to `linreg_convenience` (O’Donnell et al. 2008, Owen et al. 2016)
#'   * `direct`: Using direct formula, standard errors do no take weighting appropriately into account  (O’Donnell et al. 2008, Kakwani et al. 1997) 
#' @param df_correction If `TRUE` (default), calculates the concentration index based on the population variance (derived from the sample variance).
#' @param robust_se Uses robust standard errors if `TRUE`. Only available for the `linreg_*` type methods. Requires the `sandwich` package.
#' @param rse_type  Character, type argument for the `vcovHC()`. `HC3`' is suggested as default, set to `HC1` for Stata compatibility. See `?sandwich::vcovHC()` for options. 
#' @param rank_function Function to calculate the weighted rank of `ineqvar`. Takes two arguments: the variable that holds the rank order information, and the weights for the ranks. `rineq` currently provides two, `rank_wt` (default, corresponds to code provided in the World Bank report by O’Donnell et al.,2008) and `rank_gwt` (generalized handling of ties as also used by the Stata Conindex command, initially published by van Ourti, (2004)). 
#' 
#' @return An  S3 object of class `hci`. Contains:
#' 
#'   * `concentration_index` The concentration index
#'   * `type` The type 
#'   * `method` The method used for calculation
#'   * `variance` The variance,used for calculation of confidence intervals
#'   * `fractional_rank` Computed fractional rank `NA`  
#'   * `outcome` Outcome after removing `NA` 
#'   * `call` Call signature
#'   * `n` Number of observations after removing `NA`
#'   * `robust_se` Were robust standard errors calculated?
#'   * `rse_type` Type of robust standard errors. 
#'   * `df_correction` Do the degrees of freedom correspond to a sample?
#' 
#' @references Clarke, P. M., Gerdtham, U. G., Johannesson, M., Bingefors, K., & Smith, L. (2002). On the measurement of relative and absolute income-related health inequality. Social Science & Medicine, 55(11), 1923-1928
#' @references Erreygers, G. (2009). Correcting the concentration index. Journal of health economics, 28(2), 504-515
#' @references Kakwani, N., Wagstaff, A., & Van Doorslaer, E. (1997). Socioeconomic inequalities in health: measurement, computation, and statistical inference. Journal of econometrics, 77(1), 87-103.
#' @references O'Donnel, O., O'Neill S., Van Ourti T., & Walsh B. (2016). Conindex: Estimation of Concentration Indices. The Stata Journal, 16(1): 112-138.
#' @references O’Donnell, O., Van Doorslaer, E. , Wagstaff, A., Lindelow, M., 2008. Analyzing Health Equity Using Household Survey Data: A Guide to Techniques and Their Implementation, World Bank Publications. The World Bank.
#' @references van Ourti, T., 2004. Measuring horizontal inequity in Belgian health care using a Gaussian random effects two part count data model. Health Economics, 13: 705–724.
#' @importFrom stats na.omit weighted.mean lm coef cov.wt
#' @export
#' @examples
#' # Direct
#' data(housing)
#' ci.bmi <- ci(ineqvar = housing$income, outcome = housing$bmi, method = "direct")
#' summary(ci.bmi)
#' 
#' # retrieve value
#' ci.bmi$concentration_index
#' 
#' # obtain confidence intervals
#' confint(ci.bmi, level = 0.95) 
#' plot(ci.bmi)
#' 
#' # Wagstaff type with binary outcome and robust standard errors 
#' # that should correspond to Stata (depends on 'sandwich'):
#' ci.bmi.b <- ci(housing$income, housing$high.bmi, type = "CIw", robust_se = TRUE, 
#'    rse_type = "HC1")
#' 
ci <- function(ineqvar, outcome, weights = NULL, type = c("CI", "CIg", "CIc", "CIw"), 
               method = c("linreg_delta", "linreg_convenience", "cov_convenience", "direct"), 
               df_correction = TRUE, robust_se = FALSE, rse_type = "HC3", rank_function = rineq::rank_wt) {
  argname_outcome <-deparse(substitute(outcome))
  argname_ineqvar <-deparse(substitute(ineqvar))
  
  
	

  if (robust_se && (!requireNamespace("sandwich", quietly = T))) {
    stop("Package \"sandwich\" required for robust standard errors, run: install.packages('sandwich') ", call. = F)
  }
  # check type argument
  type <- match.arg(type)
  method <- match.arg(method)
  
  if(robust_se && !(method %in% c("linreg_delta", "linreg_convenience"))) stop("Robust standard errors only available for linreg_* methods", call.=F)
  
  
  # if no wt argument is given (default), set all sampling weights to 1.
  # sum(weights) = 1, see Lerman & Yitzhaki (1989), par. 2
  if (is.null(weights)) weights <- rep(1, length(ineqvar))
  	
  # replicate weight n times if its not a vector
  if (length(weights) == 1) weights <- rep(weights, length(ineqvar))

  # pack in dataframe 
  cdf <- data.frame(ineqvar = ineqvar, outcome = outcome, weights = weights)
	
  # ineqvar can be incomplete, but weights and outcome should be complete for calcs:
  # cdf <- cdf[ !is.na(cdf$weights) & !is.na(cdf$outcome),]
  cdf <- na.omit(cdf)
  n <- nrow(cdf)
	
	
  cdf$weights_norm = cdf$weights / sum(cdf$weights)
  
  # cdf$rank = rank_wt(cdf$ineqvar, wt = rep(1,n))

  # calculate weighted average of the health variable
  mean_outcome <- weighted.mean(cdf$outcome, w = cdf$weights)

  # calculate weighed rank of the wealth variable
  cdf$weighted_rank <- rank_function(x = cdf$ineqvar, wt = cdf$weights)

  # correction factor for sample vs population degree of freedoms 
  if(df_correction) cf <- (n-1)/n else cf = 1

  # calculate average rank
  meanw_rank  <- mean(cdf$weighted_rank)

  # variance of weighted rank
  var_ranku = var_wt(cdf$weighted_rank, wt = cdf$weights, na.rm = T)
	
  ###### actual core calulations ###############
  if(method == "linreg_delta")
  {
  	# follow 8.12 in world bank doc to also get  standard error
    cdf$wo = cdf$weights*cdf$outcome
    
  	mod_lm = lm(outcome ~ weighted_rank, data = cdf, weights = cdf$weights)
  	
  	# calculate the index using the delta method
  
  	#for illustration purposes define the g() function. 
  	# grad is are the partial derivatives with respect to a and b
  	g = function(a,b, const_var) { 2*const_var * b / (a+b/2) } 
  
  	a = coef(mod_lm)[["(Intercept)"]]
  	b = coef(mod_lm)[["weighted_rank"]]
  	
  	
  	dterm = 8* var_ranku / (2*a+b)^2
  	grad = c(-b*dterm, a*dterm)  
  
  	concentration_index = g(a,b, var_ranku)*cf
  	
  	if(robust_se) vcov = sandwich::vcovHC(mod_lm, type=rse_type)
  	else vcov = vcov(mod_lm)
  	
  	varC = (t(grad) %*% vcov  %*% grad)[1,1]
  	
	}else if(method == "linreg_convenience"){
	  
	  lhsu = 2*var_ranku*(cdf$outcome/mean_outcome)
	  
	  dm = data.frame(y = lhsu, x = cdf$weighted_rank)
	  mod_lm = lm(y ~ x, data = dm, weights = cdf$weights) # dont forget weighting
	  concentration_index = coef(mod_lm)[["x"]]*cf
	  
	  if(robust_se) vcov = sandwich::vcovHC(mod_lm, type=rse_type)
	  else vcov = vcov(mod_lm)
	  
	  varC = vcov[2,2]
	  
	}else if(method == "cov_convenience"){
	 
	  cv = cov.wt(cbind(cdf$outcome, cdf$weighted_rank), cdf$weights)
	  

	  concentration_index = (2/mean_outcome)*cv$cov[1,2] *cf
	  
	  varC = cv$cov[2,2]
	}else if(method == "direct"){
	  
	  # order df
	  o <- order(cdf$weighted_rank)
	  ordered_df <- cdf[o,]


	  concentration_index <- 2 * sum(cdf$weights_norm * (cdf$outcome - mean_outcome) * (cdf$weighted_rank - meanw_rank) / mean_outcome)
	  
	  

	  # calculate variance of the ci for weighted microcase data
	  # see Kakwani et al. (eq 14) with qi = qt, R = ft and y = x (orig)
	  # or worldbank document 8.9
	  qi <- cumsum(ordered_df$outcome) / sum(ordered_df$outcome)
	  qilag <- c(0, qi[-n])
	  ai <- ordered_df$outcome / mean_outcome * (2 * ordered_df$weighted_rank - 1 - concentration_index) + 
	    2 - qilag - qi
	  varC <- (sum(ai^2) / n - (1 + concentration_index)^2) / n
	  
	}
	else
	{
      stop("unkown method")  
	}

		
###### calculate the different types of concentration index #############
	
  if (is.null(type)) 
  {
    
   concentration_index <- concentration_index
   type = "CI"	
             
  } else if (type == "CI"){
    
    # relative
    concentration_index <- concentration_index
            
  } else if (type == "CIg"){
    
    # generalized
    concentration_index <- concentration_index *  mean_outcome
    
    varC = varC * mean_outcome^2 # treat as constant
            
  } else if (type == "CIc"){

    # Erreygers correction
    f = 4 * mean_outcome / (max(outcome, na.rm = TRUE) - min(outcome, na.rm = TRUE))
    concentration_index <- concentration_index * f
    varC = varC* f^2 # treat as constant

  } else if(type == "CIw"){
    
    # Wagstaff
    concentration_index <- concentration_index/(1-mean_outcome)
    varC = varC * (1/(1-mean_outcome))^2 # treat as constant
  }

	# return an object of class hci
	ci <-
	list(concentration_index = concentration_index,
	     type = type,
	     method = method,
		 variance = varC, 
		 fractional_rank = cdf$weighted_rank,
		 outcome = cdf$outcome,
		 ineqvar = cdf$ineqvar,
		 call =  deparse(match.call()),
		 argname_outcome = argname_ineqvar,
		 argname_ineqvar = argname_outcome,
		 n = n, 
		 robust_se = robust_se, 
		 rse_type = rse_type,
		 df_correction = df_correction)
	class(ci) <- "hci"
	return(ci)
}
