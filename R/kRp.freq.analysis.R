#' Analyze word frequencies
#'
#' The function \code{kRp.freq.analysis} analyzes texts regarding frequencies of tokens, word classes etc.
#'
#' The easiest way to see what kinds of analyses are done is probably to look at the slot description of \code{\link[koRpus]{kRp.txt.freq-class}}.
#'
#' By default, if the text has yet to be tagged, the language definition is queried by calling \code{get.kRp.env(lang=TRUE)} internally.
#' Or, if \code{txt.file} has already been tagged, by default the language definition of that tagged object is read
#' and used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to any other valid value, if you want to forcibly overwrite this
#' default behaviour, and only then. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}},
#'		\code{\link[koRpus]{kRp.analysis-class}} or \code{\link[koRpus]{kRp.txt.trans-class}}, or a character vector which must
#'		be a valid path to a file containing the text to be analyzed.
#' @param corp.freq An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#' @param desc.stat Logical, whether a descriptive statistical analysis should be performed.
#' @param force.lang A character string defining the language to be assumed for the text, by force.
#' @param tagger A character string defining the tokenizer/tagger command you want to use for basic text analysis. Can be omitted if
#'		\code{txt.file} is already of class \code{kRp.tagged-class}. Defaults to \code{"kRp.env"} to get the settings by
#'		\code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param corp.rm.class A character vector with word classes which should be ignored for frequency analysis. The default value
#'		\code{"nonpunct"} has special meaning and will cause the result of
#'		\code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be ignored for frequency analysis.
#' @param ... Additional options to be passed through to the function defined with \code{tagger}.
#' @return An object of class \code{\link[koRpus]{kRp.txt.freq-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}, \code{\link[koRpus]{kRp.tagged-class}},
#'		\code{\link[koRpus]{kRp.corp.freq-class}}
#' @export
#' @examples
#' \dontrun{
#' kRp.freq.analysis("/some/text.txt", corp.freq=my.LCC.data)
#' }

kRp.freq.analysis <- function(txt.file, corp.freq=NULL, desc.stat=TRUE, force.lang=NULL,
											 tagger="kRp.env", corp.rm.class="nonpunct",
											 corp.rm.tag=c(), ...){

	if("lang" %in% names(list(...))){
		# since 'lang' is a valid argument for treetag(), it might have been set
		stop(simpleError("You defined 'lang' in the '...' argument. This is confusing me! Use 'force.lang' instead."))
	} else {}
	# for backward compatibility
	if("treetagger" %in% names(list(...))){
		stop(simpleError("The option 'treetagger' is deprecated and was removed. Use 'tagger' instead."))
	} else {}

	# the internal function tag.kRp.txt() will return the object unchanged if it
	# is already tagged, so it's safe to call it with the lang set here
	tagged.text <- tag.kRp.txt(txt.file, tagger=tagger, lang=force.lang, objects.only=FALSE, ...)
	# set the language definition
	lang <- language.setting(tagged.text, force.lang)
	commented <- tagged.text@TT.res

	if(identical(corp.rm.class, "nonpunct")){
		corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
	} else {}

	if(!is.null(corp.freq)){
		# before we even start, check if we're alright:
		stopifnot(inherits(corp.freq, "kRp.corp.freq"))
		frequency.pre <- text.freq.analysis(txt.commented=commented, corp.freq=corp.freq, corp.rm.class=corp.rm.class,  corp.rm.tag=corp.rm.tag, lang=lang)
		# commented will be overwritten with a new version containing percentages for each word
		commented <- frequency.pre[["commented"]]
		frequency.res <- frequency.pre[["freq.analysis"]]
	} else {
		frequency.res <- list(NA)
	}

	if(isTRUE(desc.stat)){
		desc.stat.res <- text.analysis(commented, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, desc=tagged.text@desc)
	} else {
		desc.stat.res <- tagged.text@desc
	}

	results <- new("kRp.txt.freq", lang=lang, TT.res=commented, desc=desc.stat.res, freq.analysis=frequency.res)
	return(results)
}
