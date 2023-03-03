#' @title Function to decompose the Relative Concentration Index into its components
#' @details These functions decompose the Relative Concentration Index into its components using a (generalized) linear model, optionally using a survey design, or a Cox Proportional Hazards model. Print, summary and plot methods have been defined for the results.
#' @details If \code{correction} is \code{TRUE} negative values of components or outcome are corrected using \code{\link{correct_sign}} with option \code{shift = FALSE}.
#' @details For non-linear models the decomposition needs to rely on a linear approximizations of the effects. There are different approaches. One is to work on the scale of the \code{glm} coefficients and calculate the concentration index based on the predicted outcome. (Konings et al., 2010, Speybroeck et al., 2010). Another approach is to use marginal effects as beta coefficients and the original outcome (O'Donnel et al. 2008). 
#' @details This function supports both. For \code{glm}, \code{coxpy}, and \code{svyglm} models, the first approach is used. The second approach is implemented for model objects of type \code{probitmfx} and \code{logitmfx} from the 'mfx' package. See examples. 
#' @details Use the \code{decomposition} function directly to manually specify coefficients, outcomes, and model matrices for arbitrary models.  
#' @usage contribution(object, ranker, correction = TRUE, type = "CI")
#'
#' 
#' @return An object of class \code{decomposition} containing the following components:
#' \itemize{
#'   \item{betas}{a numeric vector containing regression coefficients}
#'   \item{partial_cis}{a numeric vector containing partial RCIs}
#'   \item{confints}{a numeric vector contaning 95\% confience intervals for the partial concentration indices}
#'   \item{averages}{}
#'   \item{ci_contribution}{}
#'   \item{overall_ci}{}
#'   \item{corrected_coefficients}{}
#'   \item{outcome_corrected}{}
#'   \item{rows}{}
#' }
#' 
#' @param object The model result object. class \code{coxph}, \code{glm}, \code{lm} or \code{svyglm}, \code{probitmfx}, \code{logitmfx}; the outcome should be the health variable and the predictors the components. For \code{summary()}: an object of class \code{decomposition}.
#' @param ranker Ranking variable with the same length as the outcome. 
#' @param correction A logical indicating whether the global and partial RCIs should be corrected for negative values using imputation.
#' @param type Concentration index type that the decomposition should be applied to. Defaults to \code{CI}. Use \code{CIw} for binary outcomes. 
#'
#'
#' @references Konings, P., Harper, S., Lynch, J., Hosseinpoor, A.R., Berkvens, D., Lorant, V., Geckova, A., Speybroeck, N., 2010. Analysis of socioeconomic health inequalities using the concentration index. Int J Public Health 55, 71–74. https://doi.org/10.1007/s00038-009-0078-y
#' @references Speybroeck, N., Konings, P., Lynch, J., Harper, S., Berkvens, D., Lorant, V., Geckova, A., Hosseinpoor, A.R., 2010. Decomposing socioeconomic health inequalities. Int J Public Health 55, 347–351. https://doi.org/10.1007/s00038-009-0105-z
#' @references O’Donnell, O., Doorslaer, E. van, Wagstaff, A., Lindelow, M., 2008. Analyzing Health Equity Using Household Survey Data: A Guide to Techniques and Their Implementation, World Bank Publications. The World Bank.
#'
#' @author Peter Konings
#'
#' @section Warning:
#' \code{ranker} should be chosen with care. Ideally, it is a variable from the same dataframe as the other variables. If not, redefine the row names in the model. 
#' 
#' @examples
#' data(housing)
#'
#' ## Linear regression direct decomposition
#' fit.lm <- lm(bmi ~ sex + tenure + place + age,data = housing)
#'        
#'
#' ## decompose relative concentration index
#' contrib.lm <- contribution(fit.lm, housing$income) 
#' summary(contrib.lm)
#' plot(contrib.lm, decreasing = FALSE, horiz = TRUE)
#'     
#'     
#' ## GLM: Decomposition based on predicted outcome
#' fit.logit <-glm(high.bmi ~ sex + tenure + place + age, data = housing)
#' 
#' contrib.logit <- contribution(fit.logit, housing$income) 
#' summary(contrib.logit)
#' plot(contrib.logit, decreasing = FALSE,horiz = TRUE)
#' 
#' 
#' ## GLM probit: Decomposition based on predicted outcome
#' fit.probit <-glm(high.bmi ~ sex + tenure + place + age, data = housing, 
#'                 family = binomial(link = probit))
#' 
#' # binary, set type to 'CIw'
#' contrib.probit <- contribution(fit.probit, housing$income, type = "CIw") 
#' summary(contrib.probit)
#' plot(contrib.probit, decreasing = FALSE,horiz = TRUE)
#' 
#' \dontrun{
#' 
#'    ## Marginal effects probit using package 'mfx': Decomposition based on predicted outcome
#'    library(mfx)
#'    fit.mfx <-probitmfx(high.bmi ~ sex + tenure + place + age, data = housing)
#'    
#'    contrib.mfx <- contribution(fit.mfx, housing$income, type = "CIw") 
#'    summary(contrib.mfx, type="CIw")
#'    plot(contrib.mfx, decreasing = FALSE, horiz = TRUE)
#' 
#' }
#' 
#' @export
contribution <-
function(object, ranker, correction = TRUE, type = "CI") { UseMethod("contribution") }
