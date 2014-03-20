\docType{methods}
\name{show,kRp.corp.freq-method}
\alias{show}
\alias{show,-methods}
\alias{show,kRp.TTR-method}
\alias{show,kRp.corp.freq-method}
\alias{show,kRp.lang-method}
\alias{show,kRp.readability-method}
\title{Show methods for koRpus objects}
\usage{
\S4method{show}{kRp.corp.freq}(object)

show(object)

\S4method{show}{kRp.lang}(object)

\S4method{show}{kRp.readability}(object)

\S4method{show}{kRp.TTR}(object)
}
\arguments{
  \item{object}{An object of class \code{kRp.lang},
  \code{kRp.readability}, \code{kRp.corp.freq} or
  \code{kRp.TTR}.}
}
\description{
Show methods for S4 objects of classes
\code{\link[koRpus]{kRp.lang-class}},
\code{\link[koRpus]{kRp.readability-class}},
\code{\link[koRpus]{kRp.corp.freq-class}} or
\code{\link[koRpus]{kRp.TTR-class}}.
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
\seealso{
\code{\link[koRpus]{kRp.lang-class}},
\code{\link[koRpus]{kRp.readability-class}},
\code{\link[koRpus]{kRp.corp.freq-class}},
\code{\link[koRpus]{kRp.TTR-class}}
}
\keyword{methods}

