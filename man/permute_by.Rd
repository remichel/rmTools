% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/permute_by.R
\name{permute_by}
\alias{permute_by}
\title{Permutes a DSA dataset.}
\usage{
permute_by(data, nperm = 1000, seednum = 1)
}
\arguments{
\item{data}{DSA dataset. Needs to be in longformat.}
}
\description{
\code{permute_by}
In case of unequal SOA values in different datasets, this function merges the slightly different SOA values.
}
\examples{
# Group all different SOA values in data$soa to 20 SOAs with a desired spacing of 42ms
data = permute_by()
}
\author{
Rene Michel
}
