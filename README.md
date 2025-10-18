Package ***ci***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.0.1-brightgreen.svg)](https://github.com/mokymai/ci)
[![R-CMD-check](https://github.com/mokymai/ci/workflows/R-CMD-check/badge.svg)](https://github.com/mokymai/ci/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2025--10--18-yellowgreen.svg)](/commits/master)
<!-- badges: end -->

Convenience functions for calculating confidence intervals (CI).

Report bugs, issues, and desired improvements at
<https://github.com/mokymai/ci/issues>. Please specify your operating
system.

# Installation

Installation from a CRAN:

``` r
install.packages("ci")
```

## Development version

Installation from a CRAN-like repository:

``` r
repos <- c("https://mokymai.github.io/download/", getOption("repos"))
install.packages("ci", repos = repos)
```

<details>

<summary>

Install from GitHub
</summary>

Install from GitHub:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("mokymai/ci", dependencies = TRUE)
```

Note! To install from GitHub on Windows, you need the RTools.

</details>

# Examples

``` r
ci::ci_binom(x = 20, n = 101)
```

Or:

``` r
library(ci)
ci_binom(x = 20, n = 101)
```

``` r
library(tidyverse)
library(ci)

data(npk, package = "datasets")
head(npk)

npk |>
  group_by(N, P, K) |> 
  ci_mean_t(yield)
```
