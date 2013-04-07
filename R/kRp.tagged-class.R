## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.tagged
#
# This class is used for objects that are returned by \code{\link[koRpus:treetag]{treetag}} or \code{\link[koRpus:tokenize]{tokenize}}.
#
# @slot lang A character string, naming the language that is assumed for the tokenized text in this object.
# @slot desc Descriptive statistics of the tagged text.
# @slot TT.res Results of the called tokenizer and POS tagger. The data.frame has eight columns:
#		\describe{
#			\item{\code{token}:}{The tokenized text.}
#			\item{\code{tag}:}{POS tags for each token.}
#			\item{\code{lemma}:}{Lemma for each token.}
#			\item{\code{lttr}:}{Number of letters.}
#			\item{\code{wclass}:}{Word class.}
#			\item{\code{desc}:}{A short description of the POS tag.}
#			\item{\code{stop}:}{Logical, \code{TRUE} if token is a stopword.}
#			\item{\code{stem}:}{Stemmed token.}
#		}
# @note There is also \code{as()} methods to transform objects from other koRpus classes into kRp.tagged.
# @name kRp.tagged,-class
# @aliases kRp.tagged,-class kRp.tagged-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @exportClass kRp.tagged
# @rdname kRp.tagged-class

setClass("kRp.tagged",
		representation=representation(
			lang="character",
			desc="list",
			TT.res="data.frame"),
		prototype(
			lang=character(),
			desc=list(),
			TT.res=data.frame(
				token=NA,
				tag=NA,
				lemma=NA,
				lttr=NA,
				wclass=NA,
				desc=NA,
				stop=NA,
				stem=NA)
		)
)

valid.TT.res.kRp.tagged <- c("token","tag","lemma","lttr","wclass","desc","stop","stem")

setValidity("kRp.tagged", function(object){
		TT.res <- object@TT.res
		TT.res.names <- dimnames(TT.res)[[2]]

		if(!is.character(object@lang)){
			stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
		} else {}

		if(!identical(TT.res.names, valid.TT.res.kRp.tagged)){
			stop(simpleError("Invalid object: Wrong column names in slot \"TT.res\"!"))
		} else {}

	return(TRUE)
})
