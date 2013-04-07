\name{kRp.tagged-class}
\alias{kRp.tagged-class}
\alias{kRp.tagged,-class
-class}
\alias{kRp.tagged,-class}
\title{S4 class kRp.tagged}
\description{Class kRp.tagged}
\details{This class is used for objects that are returned by \code{\link[koRpus:treetag]{treetag}}.}
\note{There is also \code{as()} methods to transform objects from other koRpus classes into kRp.tagged.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string, naming the language that is assumed for the tokenized text in this object.}\item{\code{desc}:}{(\code{\link{list}}) Descriptive statistics of the tagged text.}\item{\code{TT.res}:}{(\code{\link{data.frame}}) Results of the called tokenizer and POS tagger. The data.frame has eight columns:
\describe{
\item{\code{token}:}{The tokenized text}
\item{\code{tag}:}{POS tags for each token}
\item{\code{lemma}:}{Lemma for each token}
\item{\code{lttr}:}{Number of letters}
\item{\code{wclass}:}{Word class}
\item{\code{desc}:}{A short description of the POS tag}
\item{\code{stop}:}{Logical, \code{TRUE} if token is a stopword}
\item{\code{stem}:}{Stemmed token}
}}}}

