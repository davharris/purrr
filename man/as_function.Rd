% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{as_function}
\alias{as_function}
\title{Convert an object into a function.}
\usage{
as_function(.f)
}
\arguments{
\item{.f}{A function, formula, or atomic vector.

  If a \strong{function}, it is used as is.

  If a \strong{formula}, e.g. \code{~ .x + 2}, it is converted to a
  function with two arguments, \code{.x} or \code{.} and \code{.y}. This
  allows you to create very compact anonymous functions with up to
  two inputs.

  If \strong{character} or \strong{integer vector}, e.g. \code{"y"}, it
  is converted to an extractor function, \code{function(x) x[["y"]]}. To
  index deeply into a nested list, use multiple values; \code{c("x", "y")}
  is equivalent to \code{z[["x"]][["y"]]}.}
}
\description{
\code{as_function} is the powerhouse behind the varied function
specifications that purrr functions allow.
}
\examples{
as_function(~ . + 1)
as_function(1)
as_function(c("a", "b", "c"))
}

