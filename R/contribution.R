#' @title Function to decompose the Relative Concentration Index into its components
#' 
#' @description Currently compatible with `lm`, `glm` logit and probit, `svyglm`, `coxph` and `mfx` marginal effects probit.
#' 
#' @details These functions decompose the Relative Concentration Index into its components using a (generalized) linear model, optionally using a survey design, or a Cox Proportional Hazards model. Print, summary and plot methods have been defined for the results.
#' @details If `correction` is `TRUE` negative values of components or outcome are corrected using [correct_sign()] with option `shift = FALSE`.
#' @details For non-linear models the decomposition needs to rely on a linear approximations of the effects. There are different approaches. One is to work on the scale of the `glm` coefficients and calculate the concentration index based on the predicted outcome. (Konings et al., 2010, Speybroeck et al., 2010). Another approach is to use marginal effects as beta coefficients and the original outcome (O'Donnel et al. 2008). 
#' @details This function supports both. For `glm`, `coxph`, and `svyglm` models, the first approach is used. The second approach is implemented for model objects of type `probitmfx` and `logitmfx` from the 'mfx' package. See examples. 
#' @details Per default, the intercept in models is excluded, but this can be changed by setting the the `intercept` argument to `include`, but this may conceptually make less sense and is more appropriate if the model does not contain an intercept. 
#' @details Use [decomposition()] function directly to manually specify coefficients, outcomes, and model matrices for arbitrary models. 
#' @details NOTE: Be careful with automatically omitted rows in models. Only models with data with ordinary indexes are supported (starting from 1, sequentially increasing by increments of 1). For the case were rows with `NA` are automatically omitted by the model function, the used indices are guessed based on the row names of the model matrix and then used for accessing the `ranker` variable. However, this may lead to issues if the row names do not correspond to ordinary integer indexes. For example, if a model such as lm uses the default `na.omit` action and removes rows, the data in the model might not be consistent with the `ranker` vector anymore.   
#' @usage contribution(object, ranker, correction = TRUE, type = "CI", intercept = "exclude")
#'
#' 
#' @return An object of class `decomposition` containing the following components:
#' 
#' * `betas` A numeric vector containing regression coefficients
#' * `partial_cis` A numeric vector containing partial confidence intervals
#' * `confints` A numeric vector containing 95\% confidence intervals for the partial concentration indices
#' * `averages` Weighted averages of every variable in the model
#' * `ci_contribution` Confidence intervals for contributions
#' * `overall_ci` Confidence intervals for the concentration index
#' * `corrected_coefficients` Corrected coefficients using  [correct_sign()] if, requested `FALSE` otherwise
#' * `outcome_corrected` Corrected outcome  [correct_sign()] if requested, `FALSE` otherwise
#' * `rows` Rownames of used rows in the model
#' 
#' 
#' @param object The model result object. class `coxph`, `glm`, `lm` or `svyglm`, `probitmfx`, `logitmfx`; the outcome should be the health variable and the predictors the components. 
#' @param ranker Ranking variable with the same length as the outcome. 
#' @param correction A logical indicating whether the global and partial confidence should be corrected for negative values using imputation.
#' @param type Character, concentration index type that the decomposition should be applied to. Defaults to `CI`. Use `CIw` for binary outcomes. 
#' @param intercept Character, one of `exclude` or `include`, defaults to `exclude`. If `exclude`, the intercept coefficient will not included in the decomposition analysis, if set to `include`, it will be included. 
#'
#' @references Konings, P., Harper, S., Lynch, J., Hosseinpoor, A.R., Berkvens, D., Lorant, V., Geckova, A., Speybroeck, N., 2010. Analysis of socioeconomic health inequalities using the concentration index. Int J Public Health 55, 71–74. https://doi.org/10.1007/s00038-009-0078-y
#' @references Speybroeck, N., Konings, P., Lynch, J., Harper, S., Berkvens, D., Lorant, V., Geckova, A., Hosseinpoor, A.R., 2010. Decomposing socioeconomic health inequalities. Int J Public Health 55, 347–351. https://doi.org/10.1007/s00038-009-0105-z
#' @references O’Donnell, O., Doorslaer, E. van, Wagstaff, A., Lindelow, M., 2008. Analyzing Health Equity Using Household Survey Data: A Guide to Techniques and Their Implementation, World Bank Publications. The World Bank.
#'
#' @author Peter Konings
#'
#' @section Warning:
#' `ranker` should be chosen with care. Ideally, it is a variable from the same dataframe as the other variables. If not, redefine the row names in the model. 
#' 
#' @examples
#' data(housing)
#'
#' ## Linear regression direct decomposition
#' fit.lm <- lm(bmi ~ sex + tenure + place + age,data = housing)
#'        
#' # decompose relative concentration index
#' contrib.lm <- contribution(fit.lm, housing$income) 
#' summary(contrib.lm)
#' plot(contrib.lm, decreasing = FALSE, horiz = TRUE)
#'
#'     
#' # GLM: Decomposition based on predicted outcome
#' fit.logit <-glm(high.bmi ~ sex + tenure + place + age, data = housing)
#' 
#' contrib.logit <- contribution(fit.logit, housing$income) 
#' summary(contrib.logit)
#' plot(contrib.logit, decreasing = FALSE,horiz = TRUE)
#' 
#' 
#' # GLM probit: Decomposition based on predicted outcome
#' fit.probit <-glm(high.bmi ~ sex + tenure + place + age, data = housing, 
#'                 family = binomial(link = probit))
#' 
#' # binary, set type to 'CIw'
#' contrib.probit <- contribution(fit.probit, housing$income, type = "CIw") 
#' summary(contrib.probit)
#' plot(contrib.probit, decreasing = FALSE,horiz = TRUE)
#' 
#' 
#' # Marginal effects probit using package 'mfx': Decomposition based on predicted outcome
#' fit.mfx <-mfx::probitmfx(high.bmi ~ sex + tenure + place + age, data = housing)
#' 
#' contrib.mfx <- contribution(fit.mfx, housing$income, type = "CIw") 
#' summary(contrib.mfx, type="CIw")
#' plot(contrib.mfx, decreasing = FALSE, horiz = TRUE)
#' 
#' 
#' # package survey svy 
#' des = survey::svydesign(~1, data= housing, weights = rep(1, NROW(housing)))
#' fit.svy = survey::svyglm(bmi ~ tenure+height+weight, design = des)
#' contrib.svy = contribution(fit.svy, housing$income)
#' 
#' 
#' # adapted from the `coxph` example in survival package 
#' testcph <- data.frame(time = c(4,3,1,1,2,2,3), 
#'               status = c(1,1,1,0,1,1,0), 
#'               x      = c(0,2,1,1,1,0,0), 
#'               sex    = c(0,0,0,0,1,1,1),
#'               income = c(100,50, 20, 20, 50, 60,100))
#'               
#' # Fit a stratified model 
#' fit.coxph = survival::coxph(Surv(time, status) ~ x + strata(sex), testcph) 
#' contrib.coxph = contribution(fit.coxph, testcph$income) 
#' 
#' 
#' @export
contribution <-
function(object, ranker, correction = TRUE, type = "CI", intercept = "exclude") { UseMethod("contribution") }
