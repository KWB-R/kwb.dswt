[![R-CMD-check](https://github.com/KWB-R/kwb.dswt/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.dswt/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.dswt/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.dswt/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.dswt/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.dswt)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.dswt)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.dswt)](https://kwb-r.r-universe.dev/)

# kwb.dswt

This package contains functions to be used in KWB project 
[DSWT](https://www.kompetenz-wasser.de/en/forschung/projekte/dswt).

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.dswt' from GitHub
remotes::install_github("KWB-R/kwb.dswt")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.dswt](https://kwb-r.github.io/kwb.dswt)

Development: [https://kwb-r.github.io/kwb.dswt/dev](https://kwb-r.github.io/kwb.dswt/dev)
