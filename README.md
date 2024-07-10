# GLOSSA - Global Species Spatiotemporal Analysis <img src="inst/app/www/logo_glossa.png" align="right" alt="" width="120" />

[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Stars](https://img.shields.io/github/stars/jmestret/glossa.svg)](https://github.com/jmestret/glossa/stargazers)

***

GLOSSA is an open source application designed to model species distributions and identify suitable habitats. Written in R, GLOSSA uses the Shiny and bs4Dash libraries to provide an interactive and user-friendly interface. By supplying presence/absence or presence-only data (pseudo-absences will be generated) along with environmental variables, GLOSSA fits models using advanced machine learning techniques. With this app you can easily visualize and predict past, present, and future scenarios.

![short_workflow](https://github.com/jmestret/glossa/blob/main/inst/app/www/img/glossa_short_flowchart.png)

## Getting started

We are currently in the process of uploading `glossa`to CRAN. In the meantime, you can install the development version from GitHub and run GLOSSA using the following R code:

```{r}
if (!require("devtools")) 
  install.packages("devtools")

devtools::install_github("jmestret/glossa")
library(glossa)
run_glossa()
```

## GUI

### Home tab

![home_tab](https://github.com/jmestret/glossa/blob/main/inst/app/www/img/glossa_gui_home.png)

### New analysis tab

![new_analysis_tab](https://github.com/jmestret/glossa/blob/main/inst/app/www/img/glossa_gui_new_analysis.png)

### Reports tab

![reports_tab](https://github.com/jmestret/glossa/blob/main/inst/app/www/img/glossa_gui_reports.png)

### Export tab

![export_tab](https://github.com/jmestret/glossa/blob/main/inst/app/www/img/glossa_gui_export.png)

