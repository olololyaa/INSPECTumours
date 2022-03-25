
![Maturity level-0](https://img.shields.io/badge/Maturity%20Level-ML--0-red)

<!-- badges: start -->
[![R-CMD-check](https://github.com/AstraZeneca/INSPECTumours/workflows/R-CMD-check/badge.svg)](https://github.com/AstraZeneca/INSPECTumours/actions)
<!-- badges: end -->


IN-vivo reSPonsE Classification of Tumours (INSPECTumours)
=================================================================================

The package was built under R version 4.1.1

# Project information 

This is a shiny tool to classify and analyse pre-clinical tumour data automatically. 

# Software requirements:
Depends: 
    R (>= 3.5.0)
Imports: 
    brms,
    dplyr,
    DT,
    ggeffects,
    ggplot2,
    knitr,
    lme4,
    modelr,
    pander,
    plotly,
    purrr,
    readxl,
    rlang,
    rmarkdown,
    shiny,
    shinyalert,
    shinyFeedback,
    shinyjs,
    shinytoastr,
    shinyvalidate,
    tidybayes,
    tippy,
    tidyr,
    vroom,
    waiter

# How to use 

In order to use this package, please follow the instruction below. 

## Install from github:

```r
if (!requireNamespace("devtools", quietly = TRUE))

    install.packages("devtools")

devtools::install_github("AstraZeneca/INSPECTumours")

library(INSPECTumours)

run_app()
```

## Run App Locally

```r
devtools::load_all()
run_app()
```
## Run Tests

```r
devtools::test()
```
## Run github check for package 

```r
usethis::use_github_action_check_standard()
```



