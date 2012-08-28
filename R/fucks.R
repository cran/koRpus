#' Readability: Fucks' Stilcharakteristik
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates Fucks' Stilcharakteristik ("characteristics of style"). In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'		a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'		is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @references
#'		Fucks, W. (1955). Der Unterschied des Prosastils von Dichtern und anderen Schriftstellern. \emph{Sprachforum}, 1, 233--244.
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' 	fucks(tagged.text)
#' }

fucks <- function(txt.file, ...){
	if(is.list(txt.file)){
		results <- readability.num(txt.features=txt.file, index="Fucks", ...)
	} else {
		results <- readability(txt.file=txt.file, index="Fucks", ...)
	}
	return(results)
}
