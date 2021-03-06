% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regex2fixed.R
\name{select_types}
\alias{select_types}
\title{select types without performing slow regex search}
\usage{
select_types(regex, types_search, exact, index)
}
\arguments{
\item{regex}{a list of regular expressions}

\item{types_search}{lowercased types when \code{case_insensitive=TRUE}}

\item{exact}{set TRUE for \code{valuetype = fixed}}

\item{index}{index object created by \code{index_types()}}
}
\description{
This is an internal function for \code{regex2id()} that select types using an
index of types by reular expressions.
}
\keyword{internal}
