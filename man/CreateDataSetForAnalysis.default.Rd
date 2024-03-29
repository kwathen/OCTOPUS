% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateDataSetForAnalysis.R
\name{CreateDataSetForAnalysis.default}
\alias{CreateDataSetForAnalysis.default}
\title{CreateDataSetForAnalysis.default}
\usage{
\method{CreateDataSetForAnalysis}{default}(cPats, dCurrentTime, vISAStatus)
}
\arguments{
\item{cPats}{The object of patient data to create the data for analysis from.}

\item{dCurrentTime}{The current time in the trial.   In each simulated trial this will be the time at which
the analysis is created.}

\item{vISAStatus}{A vector with one element for each ISA.  If the status of the ISA != 0 then a dataset
is created, if the status = 0 for an ISA the dataset will not be created.}
}
\value{
{An object with class = "TrialData" that has a list with an element for each ISA data.
Before creating the dataset the ProcessData to create the version of the data needed, eg change from baseline,
and also copies covariates that are present.}
}
\description{
{This function creates a dataset for analysis based on the current time (dCurrentTime)
and returns an object with class = "TrialData" that has a list with an element for each ISA data.
Before creating the dataset the ProcessData to create the version of the data needed, eg change from baseline,
and also coppies covariates that are present.}
}
\seealso{
{ \href{https://github.com/kwathen/OCTOPUS/blob/master/R/CreateDataSetForAnalysis.R}{View Code on GitHub} }
}
