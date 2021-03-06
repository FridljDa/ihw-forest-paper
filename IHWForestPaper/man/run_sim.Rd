% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper.R
\name{run_sim}
\alias{run_sim}
\title{Apply multiple testing methods to the simulation with the continuous covariate.}
\usage{
run_sim(
  Ps,
  Xs,
  Hs,
  seed,
  alpha = 0.1,
  m = 10000,
  lfdr_only = FALSE,
  forest_par
)
}
\arguments{
\item{seed}{Integer; used for printing which simulation it running (does not set an actual RNG seed)}

\item{alpha}{Numeric (default: 0.1), nominal significance level at which to apply methods}

\item{m}{Number of hypotheses (default: m=10000)}

\item{lfdr_only}{Bool (default:FALSE), whether to run all methods (if FALSE) or only lfdr based methods (if TRUE)}
}
\value{
Data frame with FDP and Power of different methods on this simulation.
}
\description{
Apply multiple testing methods to the simulation with the continuous covariate.
}
