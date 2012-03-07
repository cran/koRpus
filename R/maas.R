#' Lexical diversity: Maas' indices
#' 
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates Maas' indices (\eqn{a^2} & \eqn{\lg{V_0}}). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the index values, and characteristics are
#' off by default.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'	\code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @export
#' @examples
#' \dontrun{
#' maas(tagged.text)
#' }

maas <- function(txt, char=FALSE, ...){
	if(isTRUE(char)){
		char.value <- "Maas"
	} else {
		char.value <- c()
	}
	results <- lex.div(txt=txt, measure="Maas", char=char.value, ...)
	return(results)
}
