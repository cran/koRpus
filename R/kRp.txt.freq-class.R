## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.txt.freq
#
# This class is used for objects that are returned by \code{\link[koRpus:freq.analysis]{freq.analysis}}.
#
# @slot lang A character string, naming the language that is assumed for the analized text in this object.
# @slot TT.res A data.frame with a version of the fully tagged text (like \code{TT.res} in class \code{koRpus.tagged}, plus frequency data).
# @slot desc A list with detailed descriptive statistics on the analyzed text.
# @slot freq.analysis A list with information on the word frequencies of the analyzed text.
# @name kRp.txt.freq,-class
# @aliaseskRp.txt.freq,-class kRp.txt.freq-class
#' @import methods
#' @include kRp.tagged-class.R
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @exportClass kRp.txt.freq
# @rdname kRp.txt.freq-class

#' @include kRp.tagged-class.R

setClass("kRp.txt.freq",
		representation=representation(
		freq.analysis="list"),
	prototype=prototype(
		lang=character(),
		TT.res=data.frame(),
		desc=list(),
		freq.analysis=list()),
	contains=c("kRp.tagged")
)


#' @include kRp.tagged-class.R
setAs(from="kRp.txt.freq", to="kRp.tagged", function(from){
		tagged.df <- as.data.frame(from@TT.res[, valid.TT.res.kRp.tagged])
		retagged.object <- new("kRp.tagged", lang=from@lang, desc=from@desc, TT.res=tagged.df)
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
