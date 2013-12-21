\name{kRp.corp.freq-class}
\alias{kRp.corp.freq-class}
\alias{kRp.corp.freq,-class
-class}
\alias{kRp.corp.freq,-class}
\title{S4 class kRp.corp.freq}
\description{Class kRp.corp.freq}
\details{This class is used for objects that are returned by \code{\link[koRpus:read.corp.LCC]{read.corp.LCC}} and \code{\link[koRpus:read.corp.celex]{read.corp.celex}}.

The slot \code{meta} simply contains all information from the "meta.txt" of the LCC[1] data and remains empty for data from a Celex[2] DB.}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\references{[1] \url{http://corpora.informatik.uni-leipzig.de/download.html}
[2] \url{http://celex.mpi.nl}}
\section{Slots}{\describe{\item{\code{meta}:}{(\code{\link{data.frame}}) Metadata on the corpora (dee details).}\item{\code{words}:}{(\code{\link{data.frame}}) Absolute word frequencies. It has the following columns:
\describe{
\item{\code{num}:}{Some word ID from the DB, integer}
\item{\code{word}:}{The word itself}
\item{\code{freq}:}{The frequency of that word in the corpus DB}
\item{\code{pct}:}{Percentage of appearance in DB}
\item{\code{pmio}:}{Appearance per million words in DB}
\item{\code{log10}:}{Base 10 logarithm of word frequency}
\item{\code{rank.avg}:}{Rank in corpus data, \code{\link{rank}} ties method "average"}
\item{\code{rank.min}:}{Rank in corpus data, \code{\link{rank}} ties method "min"}
\item{\code{rank.rel.avg}:}{Relative rank, i.e. percentile of \code{"rank.avg"}}
\item{\code{rank.rel.min}:}{Relative rank, i.e. percentile of \code{"rank.min"}}
} The slot might have additional columns,
      depending on the input material.}\item{\code{desc}:}{(\code{\link{data.frame}}) Descriptive information. It contains six numbers from the \code{meta} information,
      for convenient accessibility:
\describe{
\item{\code{tokens}:}{Number of running word forms}
\item{\code{types}:}{Number of distinct word forms}
\item{\code{words.p.sntc}:}{Average sentence length in words}
\item{\code{chars.p.sntc}:}{Average sentence length in characters}
\item{\code{chars.p.wform}:}{Average word form length}
\item{\code{chars.p.word}:}{Average running word length}
} The slot might have additional columns, depending on the input material.}}}

