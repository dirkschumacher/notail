
<!-- README.md is generated from README.Rmd. Please edit that file -->

# notail

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/dirkschumacher/notail/workflows/R-CMD-check/badge.svg)](https://github.com/dirkschumacher/notail/actions)
<!-- badges: end -->

The goal of `notail` is to experiment with tail-call elimination on the
byte-code level. It is purely for fun and educational purposes only and
is the result of an evening hack.

It also uses internal R API functions, so it will never fully pass
`R CMD check`.

## Installation

You can install the development version of `notail` like so:

``` r
remotes::install_github("dirkschumacher/notail")
```

## tailcall_eliminate

`tailcall_eliminate` takes a function. If that functions has a `Recall`
call as the last operation, it will attempt to replace the recursion by
a `GOTO` statement in the byte-code representation. Beware, this can
have unintended effects, especially if the calls have side-effects.

## Example

``` r
library(notail)
sum_n <- function(n, accumulator = 0) {
  if (n == 0) {
    return(accumulator)
  }
  Recall(n - 1, accumulator + n)
}
sum_n_2 <- tailcall_eliminate(sum_n)
try(sum_n(1000))
#> Error : evaluation nested too deeply: infinite recursion / options(expressions=)?
sum_n_2(1000)
#> [1] 500500
sum(1:1000)
#> [1] 500500
```
