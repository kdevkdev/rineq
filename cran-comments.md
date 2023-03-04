# Checking
Initial submit to CRAN. I ran R CMD check , and all checks and building in devtools to iron out issues. None remaining as far as i can tell (except rhub complaining about some miktext directory left, but that seems to be an rhub bug.

Also my first time to submit on CRAN, apologies in advance for oversights and mistakes.

* One thing i am unsure about where to put the 'mfx' and 'sandwich' package depencencies.
* Fixed unquoted software names, thank you for pointing this out.

# Background

New version of decomp (https://rdrr.io/rforge/decomp/man/contribution.html) that was previously on cran but was archived I think. Features methods used to analyse (health) inequalities. Mainly based on work by Adam Wagstaff (worked for the World Bank, https://www.worldbank.org/en/topic/health/publication/analyzing-health-equity-using-household-survey-data). Valuable for public health and social epidemiology for analysing topics related to inequalities.
