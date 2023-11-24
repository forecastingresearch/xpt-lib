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

## Usage

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

## Questions

Please feel free to contact molly@forecastingresearch.org with any questions
about the tournament or the analysis herein.
