

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Welcome to LitReach

LitReach is a Shiny app that allows authors to assess the global reach
and usage of their published work. The app provides a range of
visualisations that allow authors to explore the scope of their work in
a variety of ways, from citations in types of literature to global
reach. The app also includes a data tidying tool that formats their
citation data for use by the app.

LitReach Copyright (C) 2025 Ben Rowland This program comes with
ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to
redistribute it.

## Installation

The latest version can be installed from GitHub:

``` r
remotes::install_github("BRowland-git/LitReach")
```

All of the dependences are found on CRAN except for
[wordcloud2](https://github.com/Lchiffon/wordcloud2) which will need to
be installed from GitHub:

``` r
devtools::install_github("lchiffon/wordcloud2")
```

## Getting started

To run the app once the package is installed, use the following
commands:

``` r
library(LitReach)
LitReach()
```

From there follow the instructions that appear on the first page of the
app.


