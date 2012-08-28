#' Summary methods for koRpus objects
#'
#' Summary method for S4 objects of classes \code{\link[koRpus]{kRp.lang-class}},
#' \code{\link[koRpus]{kRp.readability-class}}, \code{\link[koRpus]{kRp.tagged-class}},
#' \code{\link[koRpus]{kRp.TTR-class}} or \code{\link[koRpus]{kRp.txt.freq-class}}.
#'
#' @param object An object of class \code{kRp.lang}, \code{kRp.readability}, 
#'		\code{kRp.tagged}, \code{kRp.TTR} or \code{kRp.txt.freq}.
#' @aliases summary,-methods summary,kRp.lang-method summary,kRp.txt.freq-method
#'		summary,kRp.readability-method summary,kRp.tagged-method summary,kRp.TTR-method
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso
#'		\code{\link[koRpus]{kRp.lang-class}},
#'		\code{\link[koRpus]{kRp.readability-class}},
#'		\code{\link[koRpus]{kRp.tagged-class}},
#'		\code{\link[koRpus]{kRp.TTR-class}},
#'		\code{\link[koRpus]{kRp.txt.freq-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#' summary(guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/"))
#' }
#' @export
#' @rdname summary-methods
#' @docType methods
setGeneric("summary")

setMethod("summary", signature(object="kRp.lang"), function(object){
	# show the main results
	show(object)

	# then some statistics
	cat("Distribution of compression differences:\n")
	print(summary(object@udhr[["diff"]]))
	cat("\n  SD:", round(sd(object@udhr[["diff"]]), digits=2), "\n\n")

	langs.available <- dim(object@udhr)[1]
	top5 <- object@udhr[1:5,c("name","uli","country","region","diff")]
	last5 <- object@udhr[(langs.available - 5):langs.available, c("name","uli","country","region","diff")]
	summary.list <- list(top5=top5, last5=last5)

	return(summary.list)
})
