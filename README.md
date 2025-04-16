

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Welcome to LitReach

LitReach is a Shiny app that allows authors to assess the global reach
and usage of their published work. The app provides a range of
visualisations that allow authors to explore the scope of their work in
a variety of ways, from citations in types of literature to global
reach. The app also includes a data tidying tool that formats the
citation data for use by the app.

LitReach Copyright (C) 2025 Ben Rowland 
This program comes with ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to
redistribute it.

## Installation

All of the dependencies are found on CRAN except for
[wordcloud2](https://github.com/Lchiffon/wordcloud2) which will need to
be installed from GitHub:

``` r
devtools::install_github("lchiffon/wordcloud2")
```

The latest version can be installed from GitHub:

``` r
remotes::install_github("BRowland-git/LitReach")
```

## Getting started

To run the app once the package is installed, use the following
commands:

``` r
library(LitReach)
LitReach()
```

From there, follow the instructions that appear on the first page of the
app.

## Example

We have included exemplar data from the citation searched for "methods for determining disease burden and calibrating national surveillance data in the United Kingdom the second study of infectious intestinal disease in the community iid2 study" by Sahra O'Brien 2010. The data can be found in the "example data" folder in the repository, and once the software has been installed, follow the instructions using the exemplar data to view the output. 

## Data Requirments

The data for LitReach is required to be in .csv format. More information is provided on the "Instructions and Data Upload" tab of the software.

## Contributions and Issues

Please see the [guide]() for code contributions and suggestions.
