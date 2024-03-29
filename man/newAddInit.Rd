% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{newAddInit}
\alias{newAddInit}
\title{Initialize empty dataframe to hold the summary stats.}
\usage{
newAddInit()
}
\value{
Dataframe (wide) with one row per (question, stage) pair, gives aggregates
with columns like:
specialty - nuclear, biorisk, etc
pct* - everyone
g1* - group 1 (highest-paid experts & ALL of the supers)
g2* - group 2 (lower tier, experts only)
domainExperts - ONLY the g1 experts from the domain of the question
nonDomainExperts - experts from not the domain of the question, and supers
*_exc - filter out extreme outliers (based on standard deviation; different from trimmed mean)
}
\description{
Initialize empty dataframe to hold the summary stats.
}
