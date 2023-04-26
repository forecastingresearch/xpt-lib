## Installation

To install the xpt package from a private GitHub repository, follow these steps:

1. Clone the private repository to your local machine.

2. Install the xpt package using devtools::install():

```R

setwd("/path/to/xpt")
devtools::install()
```

Replace /path/to/xpt with the actual path to your cloned xpt package directory.

If you make changes to `xpt-lib`, just run `install()` again and you're golden.

## Usage

Once you've installed it that way, you can load it from anywhere with `library(xpt)`.
Then you can use any of the functions that have `@export` in their docstrings.
I designated those functions kind of haphazardly and probably missed some.

## Running tests

You can run tests with `devtools` or `testthat`. E.g.:

```R

devtools::test()  # runs them all
testthat::test_dir("tests/testthat/")  # equivalent, in theory
testthat::test_file("tests/testthat/test-agg.R")  # only the tests in test-agg.R
```
