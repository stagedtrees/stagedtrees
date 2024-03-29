% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_from.R
\name{sample_from}
\alias{sample_from}
\title{Sample from a staged event tree}
\usage{
sample_from(object, size = 1, seed = NULL)
}
\arguments{
\item{object}{an object of class \code{sevt} with fitted probabilities.}

\item{size}{number of observations to sample.}

\item{seed}{an object specifying if and how the random number generator
should be initialized (‘seeded’). Either NULL or an integer
that will be used in a call to set.seed.}
}
\value{
A data frame containing \code{size} observations from the
variables in \code{object}.
}
\description{
Generate a random sample from the distribution encoded
in a staged event tree object.
}
\details{
It samples \code{size} observations according to
the transition probabilities (\code{object$prob}) in the model.
}
\examples{
model <- stages_fbhc(full(PhDArticles, lambda = 1))
sample_from(model, 10)
}
