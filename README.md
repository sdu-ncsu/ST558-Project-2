README
================
Todd Idol
10/16/2020

-   [Description](#description)
-   [Project Repo](#project-repo)
-   [Weekly Analysis](#weekly-analysis)
-   [Packages](#packages)
    -   [Tidyverse](#tidyverse)
    -   [Rmarkdown](#rmarkdown)
    -   [Caret](#caret)
    -   [GBM](#gbm)
    -   [Data Table](#data-table)
-   [Render Code](#render-code)

Description
===========

This repo includes files (Rmd, md, images…) for a Bike Rental Analysis
project built in RStudio. The project analyzes 2 years of bike rental
data and employees a regression tree model and boosted regression tree
model to predict bike rentals for each day of the week based on a set of
categorical & quantitative variables (e.g. hour of the day, weather
conditions, temperature, humidity…)

Project Repo
============

Find the project repo [here](https://github.com/tkidol/ST558-Project-2).

Weekly Analysis
===============

-   The analysis for [Sunday is available here](SundayAnalysis.md).

-   The analysis for [Monday is available here](MondayAnalysis.md).

-   The analysis for [Tuesday is available here](TuesdayAnalysis.md).

-   The analysis for [Wednesday is available here](Wedesdaynalysis.md).

-   The analysis for [Thursday is available here](ThursdayAnalysis.md).

-   The analysis for [Friday is available here](FridayAnalysis.md).

-   The analysis for [Saturday is available here](SaturdayAnalysis.md).

Packages
========

Tidyverse
---------

The workhorse of this project for 3 main core packages: DPLYR -
manipulated all of my data (selects, joins, filter, new variables… ),
Tibble for rendering Data Frames more effectively, & () ggplot2 for
discrete & continuous data plots

Rmarkdown
---------

Key to the project for the Rmd file itself including the knitr::
functions for consumable object output (kable) & README.md github doc
output through render()

Caret
-----

Creates standardized train/test data, create controls and tuning
parameters and execute the regression/boosted regression tree analysis

GBM
---

Used by caret package to execute the generalized boosted regression
model for prediction

Data Table
----------

Used for setattr function to change row names in prediction output

Render Code
===========

Link to file
[here](https://github.com/tkidol/ST558-Project-2/blob/main/P2_Render.Rmd)

\` library(dplyr) library(rmarkdown)

charDay &lt;- c(“Sunday”, “Monday”, “Tuesday”, “Wednesday”, “Thursday”,
“Friday”, “Saturday”)

numDay &lt;- c(0, 1, 2, 3, 4, 5, 6)

output\_file &lt;- paste0(charDay,“Analysis.md”)

params &lt;- lapply(numDay, FUN = function(x) (list(day = x)))

reports &lt;- tibble(output\_file, params)

apply(reports, MARGIN = 1, FUN = function(x) { render(“P2\_TKIdol.Rmd”,
output\_file = x\[\[1\]\], params = x\[\[2\]\]) })

\`
