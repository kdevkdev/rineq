## rineq


_Statistical Analysis of Health Inequalities_

The `rineq` package provides functions to  calculate the relative, generalized, and Erreygers corrected concentration index; plot Lorenz curves; and decompose health inequalities using (generalized) linear models, and survival models.

#### Available functions

Concentration index
| Function           | Description                                                                         |
|--------------------|-------------------------------------------------------------------------------------|
| `ci`               | Calculate the relative, generalized, or Erreygers corrected concentration index     |
| `correct_sign`     | Correct negative values in the health variable                                      |
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
