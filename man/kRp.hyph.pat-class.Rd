\name{kRp.hyph.pat-class}
\alias{kRp.hyph.pat-class}
\alias{kRp.hyph.pat,-class
-class}
\alias{kRp.hyph.pat,-class}
\title{S4 class kRp.hyph.pat}
\description{Class kRp.hyph.pat}
\details{This class is used for objects that are returned by \code{\link[koRpus:read.hyph.pat]{read.hyph.pat}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string,
      naming the language that is assumed for the patterns in this object}\item{\code{pattern}:}{(\code{\link{matrix}}) A matrix with three colums:
\describe{
\item{\code{orig}:}{The unchanged patgen patterns.}
\item{\code{char}:}{Only the characters used for matching.}
\item{\code{nums}:}{The hyphenation number code for the pattern.}
}}}}

