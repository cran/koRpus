\name{kRp.hyphen-class}
\alias{kRp.hyphen-class}
\alias{kRp.hyphen,-class
-class}
\alias{kRp.hyphen,-class}
\title{S4 class kRp.hyphen}
\description{Class kRp.hyphen}
\details{This class is used for objects that are returned by \code{\link[koRpus:hyphen]{hyphen}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string,
      naming the language that is assumed for the analized text in this object}\item{\code{desc}:}{(\code{\link{list}}) Descriptive statistics of the analyzed text.}\item{\code{hyphen}:}{(\code{\link{data.frame}}) A data.frame with two columns:
\describe{
\item{\code{syll}:}{Number of recognized syllables}
\item{\code{word}:}{The hyphenated word}
}}}}

