#' Summary method for objects of class kRp.txt.freq
#'
#' Summary method for S4 objects of class \code{\link[koRpus]{kRp.txt.freq-class}}
#'
#' @param object An object of class \code{kRp.txt.freq}
#' @aliases summary,-methods summary,kRp.txt.freq-method
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso \code{\link[koRpus]{kRp.txt.freq-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#' summary(kRp.freq.analysis(tagged.txt))
#' }
#' @exportMethod summary
#' @rdname summary-methods
setGeneric("summary")

#' @rdname summary-methods
setMethod("summary", signature(object="kRp.txt.freq"), function(object){

	summary.table <- t(data.frame(
		sentences=object@desc[["sentences"]],
		avg.sentence.length=object@desc[["avg.sentc.length"]],
		words=object@desc[["words"]],
		avg.word.length=object@desc[["avg.word.length"]],
		all.characters=object@desc[["all.chars"]],
		letters=object@desc[["letters"]],
		lemmata=object@desc[["lemmata"]],
		questions=object@desc[["questions"]],
		exclamations=object@desc[["exclam"]],
		semicolon=object@desc[["semicolon"]],
		colon=object@desc[["colon"]],
		stringsAsFactors=FALSE))

	dimnames(summary.table)[[2]] <- "freq"

	return(summary.table)
})
