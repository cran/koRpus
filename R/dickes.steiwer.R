#' Readability: Dickes-Steiwer Handformel
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the shortcut formula by Dickes-Steiwer. In contrast to
#' \code{\link[koRpus:readability]{readability}}, which by default calculates all possible indices,
#' this function will only calculate the index value.
#'
#' This formula doesn't need syllable count.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'		a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'		is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param case.sens Logical, whether types should be counted case sensitive.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' 	dickes.steiwer(tagged.text)
#' }

dickes.steiwer <- function(txt.file, parameters=c(const=235.95993, awl=73.021, asl=12.56438, ttr=50.03293), case.sens=FALSE, ...){
	all.parameters <- list(const=parameters[["const"]], awl=parameters[["awl"]], asl=parameters[["asl"]], ttr=parameters[["ttr"]], case.sens=case.sens)
	if(is.list(txt.file)){
		results <- readability.num(txt.features=txt.file, index="Dickes.Steiwer", parameters=list(Dickes.Steiwer=all.parameters), ...)
	} else {
		results <- readability(txt.file=txt.file, index="Dickes.Steiwer", parameters=list(Dickes.Steiwer=all.parameters), ...)
	}
	return(results)
}
