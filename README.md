xpt-lib: Existential Risk Persuasion Tournament replication package
================

<!-- badges: start -->

<!-- badges: end -->

This repository contains all the code used to produce the analysis in the
[XPT report](https://forecastingresearch.org/s/XPT.pdf) by the Forecasting
Research Institute.

## Installation

To install the xpt package from this GitHub repository, follow these steps:

1. Clone the private repository to your local machine.

2. Install the xpt package using devtools::install():

```R

setwd("/path/to/xpt")
devtools::install()
```

Replace /path/to/xpt with the actual path to your cloned xpt package directory.

Should you want to make changes to `xpt-lib` locally, just run
`devtools::document()`, then `install()` again, and you're golden.
`devtools::document` updates the NAMESPACE file (important).

## Usage

Once you've installed `xpt`, you can load it using `library(xpt)`.
`main.R` runs all the code used in the XPT report.

[TODO: explain the sources files and give an example of how to get the median/gmod/mean by group on one question??]

## Questions

Please feel free to contact molly@forecastingresearch.org with any questions
about the tournament or the analysis herein.
