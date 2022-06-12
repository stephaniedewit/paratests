
<!-- README.md is generated from README.Rmd. Please edit that file -->

# paratests

<!-- badges: start -->
<!-- badges: end -->

The goal of paratests is to make performing (non-)parametric tests in R
easier and more reproducible. Today this package contains four
functions:

-   **anova()**: print the p value of an analysis of variance summary
-   **cor_scatterplot()**: scatterplot of variables and their
    correlation test coefficient and p value
-   **mean_barchart()**: barchart of a variable mean and standard
    deviation per group
-   **sw_tidy()**: print the p value of a Shapiro Wilk normality test

Every function has to be performed on a tidy dataset.

## Installation

Paratests depends on the following packages, make sure these are
installed and loaded with `library()` first:

``` r
# install.packages("usethis")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("magrittr")
# install.packages("palmerpenguins")
# install.packages("stats")
```

You can then install and use the the most recent version of paratests
from this GitHub with:

``` r
devtools::install_github("stephaniedewit/paratests")
library(paratests)
```

NOTE: {paratests} anova() function masks the {stats} anova() function.
To use the latter use `stats::anova()`.

## Examples

For a description of each function use `?anova()`, `?cor_scatterplot()`,
`?mean_barchart()` and `?sw_tidy`. For a vignette with a short analysis
combining the four functions use `browseVignettes("paratests")`.

To try for yourself, two example datasets can be loaded into the
Environment with `data(PlantGrowth_edit)` and `data(potato)`.
