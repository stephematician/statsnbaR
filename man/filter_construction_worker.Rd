% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter_worker.R
\name{filter_construction_worker}
\alias{filter_construction_worker}
\title{Worker function to generate filters for an endpoint data query.}
\usage{
filter_construction_worker(endpoint, ...)
}
\arguments{
\item{endpoint}{List containing stats.nba.com API endpoint specification as
given by statsnbaR YAML data.}

\item{...}{List of key-value pairs to use as a filters.}
}
\value{
A list of key-value pairs for passing  as the filter argument to the
  query functions
}
\description{
All the filter functions are wrappers for this worker, which merges the
default values from and endpoint with the arguments specified.
}
\details{
Returns a named list of key-value pairs which are given either by the
arguments to the function, or by default values specified by statnbaR
internal YAML data. Ensures that all the filters specified have a value,
but performs no checks on the values themselves.
}
\author{
Stephen Wade
}
\seealso{
\code{\link{filter_bio}}  \code{\link{filter_per_player}}
  \code{\link{filter_per_player_clutch}}
}
\keyword{internal}

