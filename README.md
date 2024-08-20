
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GLOSSA - Global Species Spatiotemporal Analysis <a href="https://jmestret.github.io/glossa/"><img src="inst/app/www/logo_glossa.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/glossa)](https://CRAN.R-project.org/package=glossa)
[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

------------------------------------------------------------------------

GLOSSA is an open source application designed to model species
distributions and identify suitable habitats. Written in R, GLOSSA uses
the Shiny and bs4Dash libraries to provide an interactive and
user-friendly interface. By supplying presence/absence or presence-only
data (pseudo-absences will be generated) along with environmental
variables, GLOSSA fits models using advanced machine learning
techniques. With this app you can easily visualize and predict past,
present, and future scenarios.

<figure>
<img
src="https://github.com/jmestret/glossa/blob/main/inst/app/www/img/glossa_short_flowchart.png"
alt="short_workflow" />
<figcaption aria-hidden="true">short_workflow</figcaption>
</figure>

## Getting started

You can install and run GLOSSA using the following R code:

``` r
install.packages("glossa")

library(glossa)
run_glossa()
```

To install the development version of GLOSSA from GitHub, use:

``` r
if (!require("devtools")) 
  install.packages("devtools")

devtools::install_github("jmestret/glossa")
```

Please refer to our [website](https://jmestret.github.io/glossa/) for
how to use GLOSSA:

- [Overview](https://jmestret.github.io/glossa/)

- [Installation and quick guide](https://jmestret.github.io/glossa/)

- [Documentation](https://jmestret.github.io/glossa/)

- [Tutorials](https://jmestret.github.io/glossa/)

## How to cite GLOSSA

The GLOSSA paper is currently in progress. Meanwhile, you can reference
the GitHub repository as follows:

Mestre-Tomás, J., Fuster-Alonso, A. & Coll, M. (2024). GLOSSA: A
user-friendly R shiny app for machine learning analysis of marine
species distribution. R package version 0.1.0,
<https://github.com/jmestret/glossa>
