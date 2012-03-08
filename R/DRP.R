#' Readability: Degrees of Reading Power (DRP)
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the Degrees of Reading Power, using the Bormuth Mean Cloze Score. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'		a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'		is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param word.list A vector or matrix (with exactly one column) which defines familiar words. For valid results
#'		the long Dale-Chall list with 3000 words should be used.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' 	DRP(tagged.text, word.list=new.dale.chall.wl)
#' }

DRP <- function(txt.file, word.list, ...){
	if(is.list(txt.file)){
		results <- readability.num(txt.features=txt.file, index="DRP", ...)
	} else {
		results <- readability(txt.file=txt.file, index="DRP", word.lists=list(Bormuth=word.list), ...)
	}
	return(results)
}
