
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KLINK: Kinship analysis with linked markers

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/KLINK)](https://CRAN.R-project.org/package=KLINK)
[![](https://cranlogs.r-pkg.org/badges/grand-total/KLINK?color=yellow)](https://cran.r-project.org/package=KLINK)
[![](https://cranlogs.r-pkg.org/badges/last-month/KLINK?color=yellow)](https://cran.r-project.org/package=KLINK)
<!-- badges: end -->

KLINK is an R package and Shiny app for LR calculations in forensic
kinship testing, with support for pairwise linked STR markers. It is
built on [pedsuite](https://magnusdv.github.io/pedsuite/), with a
dedicated [KLINK
homepage](https://magnusdv.github.io/pedsuite/articles/web_only/klink.html).

For details, see the paper [*KLINK: A program for kinship testing with
pairwise linked STR
markers*](https://doi.org/10.1016/j.fsigen.2026.103578) (Vigeland and
Gilfillan, 2026, *Forensic Science International: Genetics*).

**Try the online app:** <https://magnusdv.shinyapps.io/klink/>

## Running KLINK locally

If you are working with sensitive data, you might want to run KLINK
locally/offline. To set this up, first install the KLINK package in R:

``` r
install.packages("KLINK")
```

Then you may start KLINK as follows:

``` r
KLINK::launchApp()
```

<img src="man/figures/screenshot120.png" alt="" width="100%" />
