\name{show}
\alias{show}
\alias{show,kRp.lang-method}
\alias{show,kRp.readability-method}
\alias{show,kRp.TTR-method}
\alias{show,-methods}
\title{Show methods for objects of class kRp.lang}
\arguments{
  \item{object}{An object of class \code{kRp.lang}}

  \item{object}{An object of class \code{kRp.readability}}

  \item{object}{An object of class \code{kRp.TTR}}
}
\description{
  Show methods for S4 objects of class
  \code{\link[koRpus]{kRp.lang-class}}

  Show methods for S4 objects of class
  \code{\link[koRpus]{kRp.readability-class}}

  Show methods for S4 objects of class
  \code{\link[koRpus]{kRp.TTR-class}}
}
\examples{
\dontrun{
  guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/")
}
\dontrun{
flesch(tagged.txt)
}
\dontrun{
MTLD(tagged.txt)
}
}
\author{
  m.eik michalke \email{meik.michalke@hhu.de}

  m.eik michalke \email{meik.michalke@hhu.de}

  m.eik michalke \email{meik.michalke@hhu.de}
}
\seealso{
  \code{\link[koRpus]{kRp.lang-class}}

  \code{\link[koRpus]{kRp.readability-class}}

  \code{\link[koRpus]{kRp.TTR-class}}
}
\keyword{methods}

