#' Transform text into cloze test format
#' 
#' If you feed a tagged text object to this function, its text will be transformed into
#' a format used for cloze deletion tests. That is, by default every fifth word (or as specified by
#' \code{every}) will be replaced by a line. You can also set an offset value to specify where
#' to begin.
#' 
#' The option \code{offset="all"} will not return one single object, but print the results after iterating
#' through all possible offset values.
#'
#' @exportMethod clozeDelete
#' @docType methods
#' @return And object of class kRp.tagged, with an additional list \code{cloze} in its
#'		\code{desc} slot, listing the words which were changed.
#' @rdname clozeDelete-methods
setGeneric("clozeDelete", function(obj, ...){standardGeneric("clozeDelete")})

#### internal function 
## function clozify()
# replaces a word with undercores
clozify <- function(words, replace.by="_"){
	num.chars <- nchar(words)
	word.rest <- sapply(1:length(num.chars), function(idx){
			return(paste(rep(replace.by, num.chars[idx]), collapse=""))
		})
	return(word.rest)
} ## end function clozify()

#' @rdname clozeDelete-methods
#' @aliases clozeDelete,kRp.taggedText-method
#' @usage clozeDelete(obj, every=5, offset=0, replace.by="_", fixed=10)
#' @param obj An object of class "kRp.tagged"
#' @param every Integer numeric, setting the frequency of words to be manipulated. By default,
#'		every fifth word is being transformed.
#' @param offset Either an integer numeric, sets the number of words to offset the transformations. Or the
#'		special keyword \code{"all"}, which will cause the method to iterate through all possible offset values
#'		and not return an object, but print the results (including the list with changed words).
#' @param replace.by Character, will be used as the replacement for the removed words.
#' @param fixed Integer numberic, defines the length of the replacement (\code{replace.by} will
#'		be repeated this much times). If set to 0, the replacement wil be as long as the replaced word.
#' @include koRpus-internal.R
#' @include kRp.analysis-class.R
#' @include kRp.TTR-class.R
#' @include kRp.txt.trans-class.R
#' @include kRp.txt.freq-class.R
#' @include kRp.tagged-class.R
setMethod("clozeDelete",
	# "kRp.taggedText" is a ClassUnion defined in koRpus-internal.R
	signature(obj="kRp.taggedText"),
	function (obj, every=5, offset=0, replace.by="_", fixed=10){

		if(identical(offset, "all")){
			for(idx in 0:(every-1)){
				clozeTxt <- clozeDelete(obj=obj, every=every, offset=idx, replace.by=replace.by, fixed=fixed)
				changedTxt <- slot(clozeTxt, "desc")[["cloze"]][["origText"]]
				rmLetters <- sum(changedTxt[["lttr"]])
				allLetters <- slot(obj, "desc")[["letters.only"]]
				cat(headLine(paste("Cloze variant ", idx+1, " (offset ", idx, ")", sep="")), "\n\n",
					kRp.text.paste(clozeTxt), "\n\n\n", headLine(paste("Changed text (offset ", idx, "):", sep=""), level=2), "\n\n",
					sep="")
				print(changedTxt)
				cat("\n\n", headLine(paste("Statistics (offset ", idx, "):", sep=""), level=2), "\n", sep="")
				print(summary(as(clozeTxt, "kRp.tagged")))
				cat("\nCloze deletion took ", rmLetters, " letters (", round(rmLetters * 100 / allLetters, digits=2),"%)\n\n\n", sep="")
			}
			return(invisible(NULL))
		} else {
			stopifnot(is.numeric(offset))
			if(offset > every){
				stop(simpleError("'offset' can't be greater than 'every'!"))
			} else {}

			lang <- slot(obj, "lang")
			tagged.text <- slot(obj, "TT.res")

			# now do the actual text alterations
			word.tags <- kRp.POS.tags(lang=lang, list.tags=TRUE, tags="words")
			# we'll only care for actual words
			txtToChange <- tagged.text[["tag"]] %in% word.tags
			txtToChangeTRUE <- which(txtToChange)
			# implement the offset by removing the first words
			txtToChangeTRUE <- txtToChangeTRUE[offset+1:length(txtToChangeTRUE)]
			changeIndex <- txtToChangeTRUE[!1:length(txtToChangeTRUE) %% every == 0]
			txtToChange[changeIndex] <- FALSE
			txtToChange[0:max(0,offset-1)] <- FALSE

			relevant.text <- tagged.text[txtToChange, "token"]
			textThatWasChanged <- tagged.text[txtToChange, ]
			# check if the deleted text should be replaced by a line with fixed length
			if(identical(fixed, 0)){
				relevant.text <- clozify(relevant.text, replace.by=replace.by)
			} else {
				relevant.text <- rep(paste(rep("_", fixed), collapse=""), length(relevant.text))
			}
			tagged.text[txtToChange, "token"] <- relevant.text

			clozeDesc <- list(origText=textThatWasChanged)

			# put the altered text back into the tagged object
			slot(obj, "TT.res") <- tagged.text
			slot(obj, "desc")[["cloze"]] <- clozeDesc
			return(obj)
		}
	}
)
