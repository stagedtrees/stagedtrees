% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sevt_add.R
\name{sevt_add}
\alias{sevt_add}
\title{Add a variable to a staged event tree}
\usage{
sevt_add(object, var, data, join_unobserved = TRUE, useNA = "ifany")
}
\arguments{
\item{object}{an object of class \code{sevt}.}

\item{var}{character, the name of the new variable to be added.}

\item{data}{either a \code{data.frame} or a \code{table} containing
the data from the variables in \code{object} plus \code{var}.}

\item{join_unobserved}{logical, passed to \code{\link{full}}.}

\item{useNA}{whether to include NA values in the tables.
Argument passed to \code{\link{table}}.}
}
\value{
An object of class \code{sevt} representing a
staged event tree model with \code{var} added as last variable.
}
\description{
Return an updated staged event tree with one additional
variable at the end of the tree.
}
\details{
This function update a staged event tree object with
an additional variable. The stages structure of the
new variable is initialized as in the saturated model.
}
\examples{
model <- full(Titanic, order = c("Age", "Class"))
print(model)
model <- sevt_add(model, "Survived", Titanic)
print(model)
}
