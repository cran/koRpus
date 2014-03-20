\name{read.corp.custom}
\alias{read.corp.custom}
\title{Import custom corpus data}
\usage{
read.corp.custom(corpus, format = "file", fileEncoding = "UTF-8",
  quiet = TRUE, caseSens = TRUE, log.base = 10, ...)
}
\arguments{
  \item{corpus}{Either the path to directory with txt files
  to read and analyze, or a vector object already holding
  the text corpus.  Can also be an already tokenized and
  tagged text object which inherits class \code{kRp.tagged}
  (then the column \code{"token"} of the \code{"TT.res"}
  slot is used).}

  \item{format}{Either "file" or "obj", depending on
  whether you want to scan files or analyze the given
  object.}

  \item{fileEncoding}{A character string naming the
  encoding of the corpus files.}

  \item{quiet}{Logical. If \code{FALSE}, short status
  messages will be shown.}

  \item{caseSens}{Logical. If \code{FALSE}, all tokens will
  be matched in their lower case form.}

  \item{log.base}{A numeric value defining the base of the
  logarithm used for inverse document frequency (idf). See
  \code{\link[base:log]{log}} for details.}

  \item{...}{Additional options to be passed through to the
  \code{tokenize} function.}
}
\value{
An object of class
\code{\link[koRpus]{kRp.corp.freq-class}}.
}
\description{
Read data from a custom corpus into a valid object of class
\code{\link[koRpus]{kRp.corp.freq-class}}.
}
\details{
The function should enable you to perform a basic text
corpus frequency analysis. That is, not just to import
analysis results like LCC files, but to import the corpus
material itself. The resulting object is of class
\code{\link[koRpus]{kRp.corp.freq-class}}, so it can be
used for frequency analysis by other functions of this
package.
}
\examples{
\dontrun{
ru.corp <- read.corp.custom("~/mydata/corpora/russian_corpus/")
}
}
\seealso{
\code{\link[koRpus]{kRp.corp.freq-class}}
}
\keyword{corpora}

