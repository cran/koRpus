#' Summary method for objects of class kRp.tagged
#'
#' Summary method for S4 objects of class \code{\link[koRpus]{kRp.tagged-class}}
#'
#' @param object An object of class \code{kRp.tagged}
#' @aliases summary,-methods summary,kRp.tagged-method
#' @seealso \code{\link[koRpus]{kRp.tagged-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#' tagged.results <- treetag("~/my.data/sample_text.txt", treetagger="manual", lang="en",
#'    TT.options=list(path="~/bin/treetagger", preset="en"))
#' summary(tagged.results)
#' }
#' @rdname summary-methods
setGeneric("summary")

#' @exportMethod summary
#' @rdname summary-methods
setMethod("summary", signature(object="kRp.tagged"), function(object){
	desc <- object@desc
	word.tags <- kRp.POS.tags(object@lang, list.classes=TRUE, tags="words")
	wclass.num <- summary(as.factor(object@TT.res[["wclass"]]))
	wclass.nopunct <- names(wclass.num)[names(wclass.num) %in% word.tags]
	wclass.punct <- names(wclass.num)[!names(wclass.num) %in% word.tags]
	wclass.nopunct.num <- wclass.num[wclass.nopunct]
	wclass.punct.num <- wclass.num[wclass.punct]

	wclass.nopunct.num <- wclass.nopunct.num[order(wclass.nopunct.num, decreasing=TRUE)]
	wclass.nopunct.num <- rbind(wclass.nopunct.num, 100 * wclass.nopunct.num / sum(wclass.nopunct.num))
	rownames(wclass.nopunct.num) <- c("num", "pct")
	wclass.nopunct.num <- t(cbind(wclass.nopunct.num, rbind(wclass.punct.num, NA)))

# 	desc.summary <- data.frame(
# 			letters=desc[["letters"]][["all"]],
# 			words=desc[["words"]],
# 			sentences=desc[["sentences"]],
# 			avg.word.length=desc[["avg.word.length"]],
# 			avg.sentc.length=desc[["avg.sentc.length"]])
# 
# 	print(desc.summary)

	cat(
	"\n  Sentences: ", desc[["sentences"]], "\n",
	"  Words:     ", desc[["words"]], " (", round(desc[["avg.sentc.length"]], digits=2), " per sentence)\n",
	"  Letters:   ", desc[["letters"]][["all"]], " (", round(desc[["avg.word.length"]], digits=2), " per word)\n\n  Word class distribution:\n\n",
	sep="")

	return(wclass.nopunct.num)
})
