## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.analysis
#
# This class is used for objects that are returned by \code{\link[koRpus:kRp.text.analysis]{kRp.text.analysis}}.
#
# @slot lang A character string, naming the language that is assumed for the analized text in this object
# @slot TT.res A commented verion of the fully tagged text. Depending on input data, this is
#		identical to the slot \code{TT.res} of function \code{treetag} or \code{freq.analysis}.
# @slot desc Descriptive statistics
# @slot lex.div Information on lexical diversity
# @slot freq.analysis Information on the word frequencies of the analyzed text.
# @name kRp.analysis,-class
# @aliases kRp.analysis,-class kRp.analysis-class
# --->
#' @import methods
# <---
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
# --->
#' @exportClass kRp.analysis
# <---
# @rdname kRp.analysis-class

#' @include kRp.txt.freq-class.R

setClass("kRp.analysis",
		representation=representation(
		lex.div="kRp.TTR"),
	prototype=prototype(
		lang=character(),
		TT.res=data.frame(),
		desc=list(),
		lex.div=new("kRp.TTR"),
		freq.analysis=list()),
	contains=c("kRp.txt.freq")
)

setAs(from="kRp.analysis", to="kRp.tagged", function(from){
		lang <- from@lang
		tagged.df <- as.data.frame(from@TT.res[, c("token","tag","lemma","lttr","wclass","desc")])
		retagged.object <- new("kRp.tagged", lang=lang, TT.res=tagged.df)
		return(retagged.object)
		}
)

setAs(from="kRp.analysis", to="kRp.txt.freq", function(from){
		lang <- from@lang
		desc <- from@desc
		freq.analysis <- from@freq.analysis
		tagged.df <- from@TT.res
		retagged.object <- new("kRp.txt.freq", lang=lang, TT.res=tagged.df, desc=desc, freq.analysis=freq.analysis)
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
