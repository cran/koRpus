#' @rdname summary-methods
#' @include summary.kRp.lang.R
#' @examples
#' \dontrun{
#' summary(freq.analysis(tagged.txt))
#' }
setMethod("summary", signature(object="kRp.txt.freq"), function(object){

	summary.table <- t(data.frame(
		sentences=object@desc[["sentences"]],
		avg.sentence.length=object@desc[["avg.sentc.length"]],
		words=object@desc[["words"]],
		avg.word.length=object@desc[["avg.word.length"]],
		all.characters=object@desc[["all.chars"]],
		letters=object@desc[["letters"]][["all"]],
		lemmata=object@desc[["lemmata"]],
		questions=object@desc[["questions"]],
		exclamations=object@desc[["exclam"]],
		semicolon=object@desc[["semicolon"]],
		colon=object@desc[["colon"]],
		stringsAsFactors=FALSE))

	dimnames(summary.table)[[2]] <- "freq"

	return(summary.table)
})
