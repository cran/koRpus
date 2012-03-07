\name{kRp.txt.freq-class}
\alias{kRp.txt.freq-class}
\alias{kRp.txt.freq,-class
-class}
\alias{kRp.txt.freq,-class}
\title{S4 class kRp.txt.freq}
\description{Class kRp.txt.freq}
\details{This class is used for objects that are returned by \code{\link[koRpus:kRp.freq.analysis]{kRp.freq.analysis}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string, naming the language that is assumed for the analized text in this object.}\item{\code{TT.res}:}{(\code{\link{data.frame}}) A data.frame with a version of the fully tagged text (like \code{TT.res} in class \code{koRpus.tagged}, plus frequency data).}\item{\code{desc}:}{(\code{\link{list}}) A list with detailed descriptive statistics on the analyzed text.}\item{\code{freq.analysis}:}{(\code{\link{list}}) A list with information on the word frequencies of the analyzed text.}}}

