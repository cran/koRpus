#' Lexical diversity: Measure of Textual Lexical Diversity (MTLD)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the measure of textual lexical diversity (MTLD; see McCarthy & Jarvis, 2010). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MTLD value, and characteristics are
#' off by default.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param factor.size A real number between 0 and 1, defining the MTLD factor size.
#' @param char Logical, defining whether data for plotting characteristic curves should be calculated.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'	\code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @references McCarthy, P. M. & Jarvis, S. (2010). MTLD, vocd-D, and HD-D: A validation study of sophisticated approaces to lexical diversity assessment.
#'		\emph{Behaviour Research Methods}, 42(2), 381--392.
#' @export
#' @examples
#' \dontrun{
#' MTLD(tagged.text)
#' }

MTLD <- function(txt, factor.size=0.72, char=FALSE, ...){
	if(isTRUE(char)){
		char.value <- "MTLD"
	} else {
		char.value <- c()
	}

	results <- lex.div(txt=txt, factor.size=factor.size, measure="MTLD", char=char.value, ...)
	return(results)
}
