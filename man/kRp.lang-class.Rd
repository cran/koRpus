\name{kRp.lang-class}
\alias{kRp.lang-class}
\alias{kRp.lang,-class
-class}
\alias{kRp.lang,-class}
\title{S4 class kRp.lang}
\description{Class kRp.lang}
\details{This class is used for objects that are returned by \code{\link[koRpus:guess.lang]{guess.lang}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string,
      naming the language (by a short identifier) that was estimated for the analized text in this object.}\item{\code{lang.name}:}{(\code{\link{character}}) A character string,
      full name of the estimated language.}\item{\code{txt}:}{(\code{\link{character}}) A character string containing the analized part of the text.}\item{\code{txt.full}:}{(\code{\link{character}}) A character string containing the full text.}\item{\code{udhr}:}{(\code{\link{data.frame}}) A data.frame with full analysis results for each language tried.}}}

