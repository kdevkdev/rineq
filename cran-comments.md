# Checks

No NOTES, WARNIGNS, or ERROS besides: 

- New package NOTE
- Rhub complaining about some miktext directory left, but that seems to be an rhub bug.


Initial submisiion to CRAN. 


# Resubmission

After 2. submission (March 4):

* Changes in response to comments:
    * changed `T` to `TRUE` and `F` to FALSE in `ci` function declaration (defaults for `df_correction`, `robust_se`)
    * changed `F` to `FALSE `in `plot.decomposition` function declaration (default for `horiz`)
    * Removed 'Functions to' from start of DESCRIPTION, so that it directlystarts with 'Relative, generalized, and Erreygers
    corrected concentration index... '
    * Add documentation on return value for `plot.decomposition`, `plot.hci`, `print.hci`, `summary.hci`, `summary.decomposition`, and `print.decomposition`
    * Remove `\dontrun` from examples as its unlikely that it will take more than 5 seconds to run 
    * Use `on.exit()` to reset `par()` parameter to initual values

* Other changes:
    * changed filenames: var_vt.R to var-vt.R and rank_wt.R to rank-wt.R for concsiteny
    * Added the 'cph' role to all authors in Authors@R
    * Added 'Roxygen: list(markdown = TRUE)'to DESCRIPTION
    * Removed AUTHRO and MAINTAINER fields from DESCRIPTION
    * Bumped version to 0.2.3
    * Invisible returns of the first argument for `plot.decomposition`, `plot.hci`, `print.hci`, and `print.decomposition` (According to recommendations at https://adv-r.hadley.nz/functions.html)
    * Change title in DESCRIPTION to be more succint and informative ('Concentration Index and Decomposition for Health Inequalities')
    * Slight updates in documentation for `plot.decomposition`, `plot.hci`, `print.hci`, `summary.hci`, `summary.decomposition`, and `print.decomposition`
    * invisible return for `summary.decomposition` and explicit printing.
    * Add implicit return of NULL for `summary.hci`.
    * Add example and more documentation for `var_wt`.
    * Remove variance_concentration_index.R
    * Remove is-corrected.R and and corrected-value.R and integrate their contents ino correct-sign.R
    * change argument name of   w in `var_wt()` to wt for consistency with `rank_wt()`
    * convert to roxygen2 .rmd format
    * convert README.md to markdown




After 1. submission (March 4):

* Changes in response to comments:
    * Fixed unquoted software names, thank you for pointing this out




# Background

New version of decomp (https://rdrr.io/rforge/decomp/man/contribution.html) that was previously on cran but was archived I think. Features methods used to analyse (health) inequalities. Mainly based on work by Adam Wagstaff (worked for the World Bank, https://www.worldbank.org/en/topic/health/publication/analyzing-health-equity-using-household-survey-data). Valuable for public health and social epidemiology for analysing topics related to inequalities.
