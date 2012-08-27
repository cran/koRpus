#' Readability: Kuntzsch's Text-Redundanz-Index
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates Kuntzsch's Text-Redundanz-Index (text redundancy index). In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'		a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'		is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' 	TRI(tagged.text)
#' }

TRI <- function(txt.file, hyphen=NULL, parameters=c(syll=1, word=0.449, pnct=2.467, frgn=0.937, const=14.417), ...){
	if(is.list(txt.file)){
		results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="TRI", parameters=list(TRI=parameters), ...)
	} else {
		results <- readability(txt.file=txt.file, hyphen=hyphen, index="TRI", parameters=list(TRI=parameters), ...)
	}
	return(results)
}
