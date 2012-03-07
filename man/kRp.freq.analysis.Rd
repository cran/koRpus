\name{kRp.freq.analysis}
\alias{kRp.freq.analysis}
\title{Analyze word frequencies}
\usage{
  kRp.freq.analysis(txt.file, corp.freq = NULL,
    desc.stat = TRUE, force.lang = NULL,
    tagger = "kRp.env", corp.rm.class = "nonpunct",
    corp.rm.tag = c(), ...)
}
\arguments{
  \item{txt.file}{Either an object of class
  \code{\link[koRpus]{kRp.tagged-class}},
  \code{\link[koRpus]{kRp.txt.freq-class}},
  \code{\link[koRpus]{kRp.analysis-class}} or
  \code{\link[koRpus]{kRp.txt.trans-class}}, or a character
  vector which must be a valid path to a file containing
  the text to be analyzed.}

  \item{corp.freq}{An object of class
  \code{\link[koRpus]{kRp.corp.freq-class}}.}

  \item{desc.stat}{Logical, whether a descriptive
  statistical analysis should be performed.}

  \item{force.lang}{A character string defining the
  language to be assumed for the text, by force.}

  \item{tagger}{A character string defining the
  tokenizer/tagger command you want to use for basic text
  analysis. Can be omitted if \code{txt.file} is already of
  class \code{kRp.tagged-class}. Defaults to
  \code{"kRp.env"} to get the settings by
  \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to
  \code{"tokenize"} to use
  \code{\link[koRpus:tokenize]{tokenize}}.}

  \item{corp.rm.class}{A character vector with word classes
  which should be ignored for frequency analysis. The
  default value \code{"nonpunct"} has special meaning and
  will cause the result of \code{kRp.POS.tags(lang,
  c("punct","sentc"), list.classes=TRUE)} to be used.}

  \item{corp.rm.tag}{A character vector with POS tags which
  should be ignored for frequency analysis.}

  \item{...}{Additional options to be passed through to the
  function defined with \code{tagger}.}
}
\value{
  An object of class
  \code{\link[koRpus]{kRp.txt.freq-class}}.
}
\description{
  The function \code{kRp.freq.analysis} analyzes texts
  regarding frequencies of tokens, word classes etc.
}
\details{
  The easiest way to see what kinds of analyses are done is
  probably to look at the slot description of
  \code{\link[koRpus]{kRp.txt.freq-class}}.

  By default, if the text has yet to be tagged, the
  language definition is queried by calling
  \code{get.kRp.env(lang=TRUE)} internally. Or, if
  \code{txt.file} has already been tagged, by default the
  language definition of that tagged object is read and
  used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to
  any other valid value, if you want to forcibly overwrite
  this default behaviour, and only then. See
  \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all
  supported languages.
}
\examples{
\dontrun{
kRp.freq.analysis("/some/text.txt", corp.freq=my.LCC.data)
}
}
\author{
  m.eik michalke \email{meik.michalke@hhu.de}
}
\seealso{
  \code{\link[koRpus:get.kRp.env]{get.kRp.env}},
  \code{\link[koRpus]{kRp.tagged-class}},
  \code{\link[koRpus]{kRp.corp.freq-class}}
}
\keyword{misc}

