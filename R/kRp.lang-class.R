## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.lang
#
# This class is used for objects that are returned by \code{\link[koRpus:guess.lang]{guess.lang}}.
#
# @slot lang A character string, naming the language (by a short identifier) that was estimated for the analized text in this object.
# @slot lang.name A character string, full name of the estimated language.
# @slot txt A character string containing the analized part of the text.
# @slot txt.full A character string containing the full text.
# @slot udhr A data.frame with full analysis results for each language tried.
# @name kRp.lang,-class
# @aliases kRp.lang,-class kRp.lang-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @exportClass kRp.lang
# @rdname kRp.lang-class

setClass("kRp.lang",
		representation=representation(
		lang="character",
		lang.name="character",
		txt="character",
		txt.full="character",
		udhr="data.frame"),
	prototype(
		lang=character(),
		lang.name=character(),
		txt=character(),
		txt.full=character(),
		udhr=data.frame())
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
