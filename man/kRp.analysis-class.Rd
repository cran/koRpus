\name{kRp.analysis-class}
\alias{kRp.analysis-class}
\alias{kRp.analysis,-class
-class}
\alias{kRp.analysis,-class}
\title{S4 class kRp.analysis}
\description{Class kRp.analysis}
\details{This class is used for objects that are returned by \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string, naming the language that is assumed for the analized text in this object}\item{\code{TT.res}:}{(\code{\link{data.frame}}) A commented verion of the fully tagged text. Depending on input data, this is
identical to the slot \code{TT.res} of function \code{treetag} or \code{kRp.freq.analysis}.}\item{\code{desc}:}{(\code{\link{list}}) Descriptive statistics}\item{\code{lex.div}:}{(\code{\link[koRpus]{kRp.TTR-class}}) Information on lexical diversity}\item{\code{freq.analysis}:}{(\code{\link{list}}) Information on the word frequencies of the analyzed text.}}}

