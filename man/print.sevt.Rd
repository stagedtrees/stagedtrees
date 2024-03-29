% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/print.R
\name{print.sevt}
\alias{print.sevt}
\title{Print a staged event tree}
\usage{
\method{print}{sevt}(x, ..., max = 5)
}
\arguments{
\item{x}{an object of class \code{sevt}.}

\item{...}{additional parameters (compatibility).}

\item{max}{integer, limit on the numebr of variables to print.}
}
\value{
An invisible copy of \code{x}.
}
\description{
Print a staged event tree
}
\details{
The order of the variables in the staged tree
is printed (from root). In addition the number of levels of each
variable is shown in square brackets.
If available the log-likelihood of the model is printed.
}
\examples{
DD <- generate_xor_dataset(5, 100)
model <- full(DD, lambda = 1)
print(model)
}
