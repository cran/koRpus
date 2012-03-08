#' Readability: Linsear Write Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' This function calculates the Linsear Write index. In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'		a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'		is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @export
#' @examples
#' \dontrun{
#' linsear.write(tagged.text)
#' }

linsear.write <- function(txt.file, hyphen=NULL, parameters=c(short.syll=2, long.syll=3, thrs=20), ...){
	if(is.list(txt.file)){
		results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="Linsear.Write", parameters=list(Linsear.Write=parameters), ...)
	} else {
		results <- readability(txt.file=txt.file, hyphen=hyphen, index="Linsear.Write", parameters=list(Linsear.Write=parameters), ...)
	}
	return(results)
}
