Package ***ci***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.0.1-brightgreen.svg)](https://github.com/GegznaV/ci)
[![R-CMD-check](https://github.com/GegznaV/ci/workflows/R-CMD-check/badge.svg)](https://github.com/GegznaV/ci/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2025--10--18-yellowgreen.svg)](/commits/master)
[![Codecov test
coverage](https://codecov.io/gh/GegznaV/ci/graph/badge.svg)](https://app.codecov.io/gh/GegznaV/ci)
<!-- badges: end -->

Package `ci` is an educational package providing intuitive functions for
calculating confidence intervals (CI) for various statistical
parameters. Designed primarily for teaching and learning about
statistical inference (particularly confidence intervals). Offers
user-friendly wrappers around established methods for proportions,
means, and bootstrap-based intervals. Integrates seamlessly with
Tidyverse workflows, making it ideal for classroom demonstrations and
student exercises.

Report bugs, issues, and desired improvements at
<https://github.com/GegznaV/ci/issues>.

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
remotes::install_github("GegznaV/ci", dependencies = TRUE)
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
