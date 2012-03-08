## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.hyphen
#
# This class is used for objects that are returned by \code{\link[koRpus:hyphen]{hyphen}}.
#
# @slot lang A character string, naming the language that is assumed for the analized text in this object
# @slot desc Descriptive statistics of the analyzed text.
# @slot hyphen A data.frame with two columns:
# \describe{
# 	\item {\code{syll}:}{Number of recognized syllables}
# 	\item {\code{word}:}{The hyphenated word}
# }
# @name kRp.hyphen,-class
# @aliases kRp.hyphen,-class kRp.hyphen-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @exportClass kRp.hyphen
# @rdname kRp.hyphen-class

setClass("kRp.hyphen",
		representation=representation(
		lang="character",
		desc="list",
		hyphen="data.frame"),
	prototype(
		lang=character(),
		desc=list(
			num.syll=NA,
			syll.distrib=NA,
			syll.uniq.distrib=NA,
			avg.syll.word=NA,
			syll.per100=NA
		),
		hyphen=data.frame(syll=numeric(), word=character()))
)

setValidity("kRp.hyphen", function(object){
		hyphen <- object@hyphen
		hyphen.names <- dimnames(hyphen)[[2]]

		if(!is.character(object@lang)){
			stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
		} else {}

		if(!identical(hyphen.names, c("syll", "word"))){
			stop(simpleError("Invalid object: Wrong column names in slot \"hyphen\"!"))
		} else {}

	return(TRUE)
})
