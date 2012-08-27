#' Lexical diversity: Mean Segmental Type-Token Ratio (MSTTR)
#'
#' This is just a convenient wrapper function for \code{\link[koRpus:lex.div]{lex.div}}.
#'
#' This function calculates the mean segmental type-token ratio (MSTTR). In contrast to
#' \code{\link[koRpus:lex.div]{lex.div}}, which by default calculates all possible measures and
#' their progressing characteristics, this function will only calculate the MSTTR value.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.analysis-class}}, containing the tagged text to be analyzed.
#' @param segment An integer value, defining how many tokens should form one segment.
#' @param ... Further valid options for the main function, see \code{\link[koRpus:lex.div]{lex.div}} for details.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'	\code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @export
#' @examples
#' \dontrun{
#' MSTTR(tagged.text)
#' }

MSTTR <- function(txt, segment=100, ...){
	results <- lex.div(txt=txt, segment=segment, measure="MSTTR", char=c(), ...)
	return(results)
}
