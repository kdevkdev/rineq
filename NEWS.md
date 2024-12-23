# rineq 0.2.4
* Safeguard against invalid indexes when accessing the ranking variable in the decomposition analysis by checking the length and for `NA` at indexes used by the model matrix.  
* Add additional rank function `rank_gwt` that implements the generalized version of the weighted fractional as suggested in van Ourti 2004, and using it for the concentration index leads to results compatible with stata's Conindex. It can be enabled with a new parameter in `ci()`;, `rank_function = rank_gwt()` that for compatibility has default value `rank_wt()`.
* Fix cox proportional haxard example to not attempt to incorrectly discard the intercept (coxph models do not have intercepts). Also fix the `survival` formula syntax to not use explicit `::` calls. 


# rineq 0.2.3
* Further fix issues for CRAN release, related to spelling, policies, and  documentation
    * documented return value for all functions, even if called for side effects
    * FALSE/TRUE instead of T/F
    * safeguarding against altering user options in plot and not restoring them
* Examples for all packages supported decomposition model types
* Improve handling of whether intercept should be included or not for `contribution()`


# rineq 0.2.2
* Get ready for release on CRAN. Improve spelling, documentation
* Add imaginary example `housing` data set to power the examples. The previous example data set `Nigeria` was removed because of uncertainty about compatible licensing. No appropriate microdata set with both continuous inequality and health outcome variable with an appropriate license could be found, hence the artificial data.


# rineq 0.2.1
* improve plotting. Calculate margins to avoid cutting-off labels for decomposition, more informative axis labels.

# rineq 0.2
* Forked from (https://github.com/brechtdv/rineq), which in turn was forked from `decomp`
* Add support for binary type concentration index
* Add support for robust standard errors, requires the `sandwich` package
* Add support for different methods of calculating the concentration index and briefly outline their pros and cons. See `method` argument of `ci()`
* Add support for marginal effects probit (`mfx` package).
* Convert documentation to the `roxygen2` format.
