\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,kRp.tagged,missing-method}
\title{Plot method for objects of class kRp.tagged}
\arguments{
  \item{x}{An object of class \code{kRp.tagged}}

  \item{what}{Character string, valid options are:
  \describe{ \item{\code{"wclass"}:}{Barplot of
  distribution of word classes}
  \item{\code{"letters"}:}{Line plot of distribution of
  word length in letters} }}
}
\description{
  Plot method for S4 objects of class
  \code{\link[koRpus]{kRp.tagged-class}}, plots the
  frequencies of tagged word classes.
}
\examples{
\dontrun{
tagged.results <- treetag("~/my.data/sample_text.txt", treetagger="manual", lang="en",
   TT.options=list(path="~/bin/treetagger", preset="en"))
plot(tagged.results)
}
}
\seealso{
  \code{\link[koRpus]{kRp.tagged-class}}
}
\keyword{methods}
\keyword{plot}

