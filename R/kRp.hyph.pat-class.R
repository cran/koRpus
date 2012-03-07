## temporarily turned off most of the roxygen comments
## class docs will remain static until roxygen2 supports "@slot"

# S4 Class kRp.hyph.pat
#
# This class is used for objects that are returned by \code{\link[koRpus:read.hyph.pat]{read.hyph.pat}}.
#
# @slot lang A character string, naming the language that is assumed for the patterns in this object
# @slot pattern A matrix with three colums:
#		\describe{
#			\item{\code{orig}:}{The unchanged patgen patterns.}
#			\item{\code{char}:}{Only the characters used for matching.}
#			\item{\code{nums}:}{The hyphenation number code for the pattern.}
#		}
# @name kRp.hyph.pat,-class
# @aliases kRp.hyph.pat,-class kRp.hyph.pat-class
#' @import methods
# @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @exportClass kRp.hyph.pat
# @rdname kRp.hyph.pat-class

setClass("kRp.hyph.pat",
		representation=representation(
		lang="character",
		pattern="matrix"),
	prototype(
		lang=character(),
		pattern=matrix(c(character(),character(),character()), ncol=3, dimnames=list(c(), c("orig", "char", "nums"))))
)

setValidity("kRp.hyph.pat", function(object){
	lang <- object@lang
	pattern <- object@pattern

	pattern.names <- dimnames(pattern)[[2]]

	if(length(lang) > 1){
		stop(simpleError("Invalid object: Slot \"lang\" mustn't have more than one entry."))
	} else {}

	if(!identical(pattern.names, c("orig", "char", "nums"))){
		stop(simpleError("Invalid object: Wrong column names in slot \"pattern\"."))
	} else {}

	return(TRUE)
})
