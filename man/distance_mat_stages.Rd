% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probdist.R
\name{distance_mat_stages}
\alias{distance_mat_stages}
\title{Compute the distance matrix}
\usage{
distance_mat_stages(x, distance = probdist.kl)
}
\arguments{
\item{x}{list of conditional probabilities for each stage.}

\item{distance}{the distance function e.g. \code{\link{probdist.kl}}.}
}
\value{
The matrix with the distances between stages.
}
\description{
Compute the matrix of distances between probabilities,
e.g the transition probabilities for a given variable in a
staged event tree.
}
\keyword{internal}
