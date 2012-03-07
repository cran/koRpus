\name{summary}
\alias{summary}
\alias{summary,kRp.lang-method}
\alias{summary,kRp.readability-method}
\alias{summary,kRp.tagged-method}
\alias{summary,kRp.TTR-method}
\alias{summary,kRp.txt.freq-method}
\alias{summary,-methods}
\title{Summary method for objects of class kRp.lang}
\arguments{
  \item{object}{An object of class \code{kRp.lang}}

  \item{object}{An object of class \code{kRp.readability}}

  \item{flat}{Logical, if TRUE only a named vector of main
  results is returned}

  \item{object}{An object of class \code{kRp.tagged}}

  \item{object}{An object of class \code{kRp.TTR}}

  \item{object}{An object of class \code{kRp.txt.freq}}
}
\description{
  Summary method for S4 objects of class
  \code{\link[koRpus]{kRp.lang-class}}

  Summary method for S4 objects of class
  \code{\link[koRpus]{kRp.readability-class}}

  Summary method for S4 objects of class
  \code{\link[koRpus]{kRp.tagged-class}}

  Summary method for S4 objects of class
  \code{\link[koRpus]{kRp.TTR-class}}

  Summary method for S4 objects of class
  \code{\link[koRpus]{kRp.txt.freq-class}}
}
\examples{
\dontrun{
summary(guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/"))
}
\dontrun{
summary(flesch(tagged.txt))
}
\dontrun{
tagged.results <- treetag("~/my.data/sample_text.txt", treetagger="manual", lang="en",
   TT.options=list(path="~/bin/treetagger", preset="en"))
summary(tagged.results)
}
\dontrun{
summary(lex.div(tagged.txt))
}
\dontrun{
summary(kRp.freq.analysis(tagged.txt))
}
}
\author{
  m.eik michalke \email{meik.michalke@hhu.de}

  m.eik michalke \email{meik.michalke@hhu.de}

  m.eik michalke \email{meik.michalke@hhu.de}

  m.eik michalke \email{meik.michalke@hhu.de}
}
\seealso{
  \code{\link[koRpus]{kRp.lang-class}}

  \code{\link[koRpus]{kRp.readability-class}}

  \code{\link[koRpus]{kRp.tagged-class}}

  \code{\link[koRpus]{kRp.TTR-class}}

  \code{\link[koRpus]{kRp.txt.freq-class}}
}
\keyword{methods}

