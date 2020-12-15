#' @title Function to decompose the Relative Concentration Index into its components
#' @details These functions decompose the Relative Concentration Index into its components using a (generalized) linear model, optionally using a survey design, or a Cox Proportional Hazards model. Print, summary and plot methods have been defined for the results.
#' @details If \code{correction} is \code{TRUE} negative values of components or outcome are corrected using \code{\link{correct_sign}} with option \code{shift = FALSE}.
#' @usage contribution(object, ranker, correction = TRUE)
#'
#' 
#' @return An object of class \code{decomposition} containing the following components
#' \itemize{
#'   \item{betas}{a numeric vector containing regression coefficients}
#'   \item{partial_cis}{a numeric vector containing partial RCIs}
#'   \item{confints}{a numeric vector contaning 95\% confience intervals for the partial concentration indices}
#' }
#' 
#' @param object The model result object. class \code{coxph}, \code{glm}, \code{lm} or \code{svyglm}; the outcome should be the health variable and the predictors the components. For \code{summary()}: an object of class \code{decomposition}.
#' @param ranker Ranking variable with the same length as the outcome. 
#' @param correction A logical indicating whether the global and partial RCIs should be corrected for negative values using imputation.
#' @return
#'
#' @references Konings \emph{et al.}, 2009
#' @references Speybroeck \emph{et al.}, 2009
#'
#' @author Peter Konings
#'
#' @section Warning:
#' \code{ranker} should be chosen with care. Ideally, it is a variable from the same dataframe as the other variables. If not, redefine the row names in the model. 
#' @examples
#' data(nigeria)
#'
#' ## fit multivariable model
#' fit <-
#'    glm(zscore1 ~
#'            quintile + ed + rural + region + male + bord + agechild + agemother,
#'        data = nigeria,
#'        weights = nigeria$weight)
#' summary(fit)
#'
#' ## decompose relative concentration index
#' contrib <- contribution(fit, nigeria$wealth) 
#' summary(contrib)
#' par(mar = c(4, 8, 1, 1))
#' plot(contrib, decreasing = FALSE,
#'     horiz = TRUE, las = 1, xlab = "Contribution (%)")
#' @export
contribution <-
function(object, ranker, correction = TRUE) { UseMethod("contribution") }
