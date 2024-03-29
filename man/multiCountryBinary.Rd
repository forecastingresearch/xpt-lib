% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{multiCountryBinary}
\alias{multiCountryBinary}
\title{Stats and graphs for Multi-year Country Binary Questions}
\usage{
multiCountryBinary(
  metaTable,
  data,
  public_supplement1,
  public_supplement2,
  public_supplement3,
  survey_column_matches
)
}
\arguments{
\item{metaTable}{\itemize{
\item metadata for all the multi-year country binary question sets
}}

\item{data}{\itemize{
\item data for all users on all multi-year country binary question sets
}}
}
\description{
For yes/no country questions, like country-by-country nuclear
use by 2030, this function will build out the folder structure and populate
it with stats and graphs.
}
