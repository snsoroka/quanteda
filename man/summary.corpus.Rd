% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/corpus-methods-base.R
\name{summary.corpus}
\alias{summary.corpus}
\title{summarize a corpus}
\usage{
\method{summary}{corpus}(object, n = 100, showmeta = FALSE,
  tolower = FALSE, ...)
}
\arguments{
\item{object}{corpus to be summarized}

\item{n}{maximum number of texts to describe, default=100}

\item{showmeta}{set to \code{TRUE} to include document-level
meta-data}

\item{tolower}{convert texts to lower case before counting types}

\item{...}{additional arguments passed through to \code{\link{tokenize}}}
}
\description{
Displays information about a corpus, including
includes attributes and metadata such as date of number of texts, creation 
and source.
}
\examples{
summary(data_corpus_inaugural)
summary(data_corpus_inaugural, n = 10)
mycorpus <- corpus(data_char_ukimmig2010, 
                   docvars = data.frame(party=names(data_char_ukimmig2010))) 
summary(mycorpus, showmeta=TRUE)  # show the meta-data
mysummary <- summary(mycorpus)    # (quietly) assign the results
mysummary$Types / mysummary$Tokens             # crude type-token ratio
}
\keyword{corpus}
\keyword{internal}
