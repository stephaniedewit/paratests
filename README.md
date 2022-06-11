
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

You can install the development version of paratests from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("stephaniedewit/paratests")
```

## Examples

For an example of each function use `?anova()`, `?cor_scatterplot()`,
`?mean_barchart()` and `?sw_tidy`. For a vignette with a short analysis
combining the four functions use `browseVignettes("paratests")`.
