## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.txt.trans
#
# This class is used for objects that are returned by \code{\link[koRpus:kRp.text.transform]{kRp.text.transform}}.
#
# @slot lang A character string, naming the language that is assumed for the analized text in this object.
# @slot desc Descriptive statistics of the tagged text.
# @slot TT.res A data.frame with the fully tagged and transformed text (like \code{TT.res} in class \code{koRpus.tagged}, plus
#		the new columns \code{token.old} and \code{equal}).
# @slot diff A list with atomic vectors, describing the amount of diffences between both text variants (percentage):
#		\describe{
#			\item{\code{all.tokens}:}{Percentage of all tokens, including punctuation, that were altered.}
#			\item{\code{words}:}{Percentage of altered words only.}
#			\item{\code{all.chars}:}{Percentage of all characters, including punctuation, that were altered.}
#			\item{\code{letters}:}{Percentage of altered letters in words only.}
#		}
# @name kRp.txt.trans,-class
# @aliases kRp.txt.trans,-class kRp.txt.trans-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @exportClass kRp.txt.trans
# @rdname kRp.txt.trans-class

#' @include kRp.tagged-class.R

setClass("kRp.txt.trans",
		representation=representation(
		diff="list"),
	prototype=prototype(
		lang=character(),
		desc=list(),
		TT.res=data.frame(token=NA, tag=NA, lemma=NA, lttr=NA, wclass=NA, desc=NA, token.old=NA, equal=NA),
		diff=list()),
	contains=c("kRp.tagged")
)

setAs(from="kRp.txt.trans", to="kRp.tagged", function(from){
		lang <- from@lang
		tagged.df <- as.data.frame(from@TT.res[, valid.TT.res.kRp.tagged])
		retagged.object <- new("kRp.tagged", lang=lang, TT.res=tagged.df)
		return(retagged.object)
		}
)

# setValidity("kRp.analysis", function(object){
#     TT.res <- object@TT.res
#     TT.res.names <- dimnames(TT.res)[[2]]
#     if(identical(TT.res.names, c("word","tag","lemma"))){
#       return(TRUE)
#     } else {
#       stop(simpleError("Invalid object: Wrong column names."))
#     }
# })
