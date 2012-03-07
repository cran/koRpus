## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.corp.freq
#
# This class is used for objects that are returned by \code{\link[koRpus:read.corp.LCC]{read.corp.LCC}} and \code{\link[koRpus:read.corp.celex]{read.corp.celex}}.
#
# The slot \code{meta} simply contains all information from the "meta.txt" of the LCC[1] data and remains empty for data from a Celex[2] DB.
#
# @slot meta Metadata on the corpora (dee details).
# @slot words Absolute word frequencies. It has the following columns:
#	 \describe{
#		\item{\code{num}:}{Some word ID from the DB, integer}
#		\item{\code{word}:}{The word itself}
#		\item{\code{freq}:}{The frequency of that word in the corpus DB}
#		\item{\code{pct}:}{Percentage of appearance in DB}
#		\item{\code{pmio}:}{Appearance per million words in DB}
#		\item{\code{log10}:}{Base 10 logarithm of word frequency}
#		\item{\code{rank.avg}:}{Rank in corpus data, \code{\link{rank}} ties method "average"}
#		\item{\code{rank.min}:}{Rank in corpus data, \code{\link{rank}} ties method "min"}
#		\item{\code{rank.rel.avg}:}{Relative rank, i.e. percentile of \code{"rank.avg"}}
#		\item{\code{rank.rel.min}:}{Relative rank, i.e. percentile of \code{"rank.min"}}
#	}
# @slot desc Descriptive information. It contains six numbers from the \code{meta} information, for convenient accessibility:
#	 \describe{
#		\item{\code{tokens}:}{Number of running word forms}
#		\item{\code{types}:}{Number of distinct word forms}
#		\item{\code{words.p.sntc}:}{Average sentence length in words}
#		\item{\code{chars.p.sntc}:}{Average sentence length in characters}
#		\item{\code{chars.p.wform}:}{Average word form length}
#		\item{\code{chars.p.word}:}{Average running word length}
# }
# @name kRp.corp.freq,-class
# @aliases kRp.corp.freq,-class kRp.corp.freq-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
# @references
# [1] \url{http://corpora.informatik.uni-leipzig.de/download.html}
# [2] \url{http://celex.mpi.nl}
#' @exportClass kRp.corp.freq
# @rdname kRp.corp.freq-class

setClass("kRp.corp.freq",
		representation=representation(
		meta="data.frame",
		words="data.frame",
		desc="data.frame"),
		prototype(
		meta=as.data.frame(matrix(ncol=2, dimnames=list(c(),c("meta", "value")))),
		words=as.data.frame(matrix(ncol=10, dimnames=list(c(),c("num", "word", "freq", "pct", "pmio", "log10", "rank.avg", "rank.min", "rank.rel.avg", "rank.rel.min")))),
		desc=as.data.frame(matrix(ncol=6, dimnames=list(c(),c("tokens", "types", "words.p.sntc", "chars.p.sntc", "chars.p.wform", "chars.p.word")))))
)

setValidity("kRp.corp.freq", function(object){
		meta <- object@meta
		words <- object@words
		desc <- object@desc

		meta.names <- dimnames(meta)[[2]]
		words.names <- dimnames(words)[[2]]
		desc.names <- dimnames(desc)[[2]]

		if(identical(meta.names, c("meta", "value")) &
				identical(words.names, c("num", "word", "freq", "pct", "pmio", "log10", "rank.avg", "rank.min", "rank.rel.avg", "rank.rel.min")) &
				identical(desc.names, c("tokens", "types", "words.p.sntc", "chars.p.sntc", "chars.p.wform", "chars.p.word"))){
			return(TRUE)
		} else {
			stop(simpleError("Invalid object: Wrong column names."))
		}
})
