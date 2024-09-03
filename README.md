
<!-- README.md is generated from README.Rmd. Please edit that file -->

# KLINK: Kinship analysis with linked markers

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/KLINK)](https://CRAN.R-project.org/package=KLINK)
[![](https://cranlogs.r-pkg.org/badges/grand-total/KLINK?color=yellow)](https://cran.r-project.org/package=KLINK)
[![](https://cranlogs.r-pkg.org/badges/last-month/KLINK?color=yellow)](https://cran.r-project.org/package=KLINK)
<!-- badges: end -->

KLINK is an R package and a Shiny application for LR calculations in
forensic kinship testing. For more information, see the [KLINK
website](https://magnusdv.github.io/pedsuite/articles/web_only/klink.html).

Or try the live app here: <https://magnusdv.shinyapps.io/klink/>

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

<img src="man/figures/screenshot-klink.png" width="100%" />
