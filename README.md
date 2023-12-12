xpt-lib: Existential Risk Persuasion Tournament replication package
================

<!-- badges: start -->

<!-- badges: end -->

This repository contains all the code used to produce the analysis in the
[XPT report](https://forecastingresearch.org/s/XPT.pdf) from the Forecasting
Research Institute ([FRI](https://forecastingresearch.org/)).

## Installation

To install the xpt package from this GitHub repository, follow these steps:

1. Clone the private repository to your local machine.

2. Install the xpt package using `devtools::install()`:

```R

setwd("/path/to/xpt-lib")
devtools::install()
```

Replace /path/to/xpt with the actual path to your cloned xpt package directory.

Should you want to make changes to `xpt-lib` locally, just run
`devtools::document()`, then `install()` again, and you're golden.
`devtools::document` updates the NAMESPACE file (important).

Once you've installed `xpt`, you can load it using `library(xpt)`.

## Files

- `main.R` runs all the code used in the XPT report.
- Data (`sources` directory)
    - expertsG1_anon.csv: user IDs for the experts and their specialty (or specialties).
    - supers_anon.csv: user IDs for [Superforecasters](https://goodjudgment.com/services/custom-superforecasts/)
    - questionMetadata.csv: metadata for the forecasting questions (resolution year, question type, answer units, etc)
    - forecasts_anon.csv: forecasts from experts and Superforecasters
    - The `public-survey` directory contains the responses from public surveys, which were put out in three parts
    - survey_column_matches.csv maps the questions from the public survey (column names in `public_supplement` files) to the corresponding questions in the main tournament

## Notes

Note that the platform we used to host the XPT occasionally had a small amount of measurement error in tracking when forecasts were submitted. For instance, on questions where we asked the user to submit not just their median forecasts but forecasts of several quantiles (the 5th, 25th, 75th, and 95th percentiles), the platform occasionally registered these forecasts as submitted at slightly different times, a few seconds apart. Our code addresses these errors, but in a handful of cases this caused us to exclude valid forecasts from our analysis.

We do not provide specific timestamps (only calendar dates) in this data release to ensure forecaster anonymity. So, to replicate the results we released in the report as closely as possible, we’ve replaced timestamp-related variables with the variables forecastid, stage, and timestampid, and altered our code to use these proxy variables in place of regular timestamps. 'forecastId' marks sets of forecasts (eg. multiple quantiles) that our original code treated as a single forecast, 'stage' identifies the stage of the tournament that the forecast was submitted during, and 'timestampId' replaces all timestamps registered by the platform with an ordinal number.

There are a few minor discrepancies between the results we produced in the original report and those you will find if you rerun our programs using this data release. These discrepancies are the result of how different statistical programs store information about very large numbers. Feel free to contact us if you have any questions about discrepancies between the report and this data release.

## Questions

Please feel free to contact molly@forecastingresearch.org with any questions
about the tournament or the analysis herein, or if you wish to contribute.
