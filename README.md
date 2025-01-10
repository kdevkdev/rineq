## rineq


_Statistical Analysis of Health Inequalities_

The `rineq` package provides functions to  calculate the relative, generalized, and Erreygers corrected concentration index; plot Lorenz curves; and decompose health inequalities using (generalized) linear models, and survival models.

#### Available functions

Concentration index - main functions

| Function           | Description                                                                         |
|--------------------|-------------------------------------------------------------------------------------|
| `ci`               | Calculate the relative, generalized, or Erreygers corrected concentration index     |
| `contribution`     | Decompose the Relative Concentration Index into its components                      |


#### Install

Install the version on CRAN:
```r
install.packages("rineq")
```

or download and install the latest development version from GitHub using `devtools`:
```r
devtools::install_github("kdevkdev/rineq")
```

![R CMD check](https://github.com/kdevkdev/rineq/actions/workflows/R-CMD-check.yaml/badge.svg)
