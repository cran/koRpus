\name{kRp.txt.trans-class}
\alias{kRp.txt.trans-class}
\alias{kRp.txt.trans,-class
-class}
\alias{kRp.txt.trans,-class}
\title{S4 class kRp.txt.trans}
\description{Class kRp.txt.trans}
\details{This class is used for objects that are returned by \code{\link[koRpus:kRp.text.transform]{kRp.text.transform}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string,
      naming the language that is assumed for the analized text in this object.}\item{\code{TT.res}:}{(\code{\link{data.frame}}) A data.frame with the fully tagged and transformed text (like \code{TT.res} in class \code{koRpus.tagged},
      plus
the new columns \code{token.old} and \code{equal}).}\item{\code{diff}:}{(\code{\link{list}}) A list with atomic vectors,
      describing the amount of diffences between both text variants (percentage):
\describe{
\item{\code{all.tokens}:}{Percentage of all tokens, including punctuation,
      that were altered.}
\item{\code{words}:}{Percentage of altered words only.}
\item{\code{all.chars}:}{Percentage of all characters, including punctuation,
      that were altered.}
\item{\code{letters}:}{Percentage of altered letters in words only.}
}}}}

