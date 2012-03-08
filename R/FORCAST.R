#' Readability: FORCAST Index
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:readability]{readability}}.
#'
#' Calculates the FORCAST index (both grade level and reading age). In contrast to \code{\link[koRpus:readability]{readability}},
#' which by default calculates all possible indices, this function will only calculate the index value.
#'
#' If \code{parameters="RGL"}, the parameters for the precise Reading Grade Level are used.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, a character vector which must be be
#'		a valid path to a file containing the text to be analyzed, or a list of text features. If the latter, calculation
#'		is done by \code{\link[koRpus:readability.num]{readability.num}}. 
#' @param hyphen An object of class kRp.hyphen. If \code{NULL}, the text will be hyphenated automatically.
#' @param parameters A numeric vector with named magic numbers, defining the relevant parameters for the index, or \code{"RGL"}.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:readability]{readability}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.readability-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords readability
#' @references
#'		Klare, G.R. (1975). Assessing readability. \emph{Reading Research Quarterly}, 10(1), 62--102.
#' @export
#' @examples
#' \dontrun{
#' FORCAST(tagged.text)
#' }

FORCAST <- function(txt.file, hyphen=NULL, parameters=c(syll=1, mult=.10, const=20), ...){
	if(is.list(txt.file)){
		results <- readability.num(txt.features=txt.file, hyphen=hyphen, index="FORCAST", parameters=list(FORCAST=parameters), ...)
	} else {
		results <- readability(txt.file=txt.file, hyphen=hyphen, index="FORCAST", parameters=list(FORCAST=parameters), ...)
	}
	return(results)
}
