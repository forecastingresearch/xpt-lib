% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convergence.R
\name{convergenceAdd}
\alias{convergenceAdd}
\title{Within-team Convergence}
\usage{
convergenceAdd(phase1, phase2, phase3, phase4, convergenceTable)
}
\arguments{
\item{phase1}{End of stage/phase 1, where was everyone}

\item{phase2}{ditto}
}
\description{
How much did SD shrink over time (within-team) from stage to stage?

So, (roughly) each of the variables in the convergence functions are structured as follows:
t###_init#_stage#
where
t### refers to team
init# refers to the stage in which the forecaster first submitted a
forecast (i.e. init1 means we’re subsetting just to forecasters who
submitted a forecast during stage 1; init2 is specifically excluding those
who submitted during stage 1, and just focusing on people who first
submitted a forecast on that question in stage 2)
stage# refers to the end-of-stage snapshot we’re collecting data from
}
\note{
This code has not been scrutinized

This is NOT what we refer to as "convergence" in the XPT report.
}
