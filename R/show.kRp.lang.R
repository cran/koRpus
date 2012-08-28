#' Show methods for koRpus objects
#'
#' Show methods for S4 objects of classes \code{\link[koRpus]{kRp.lang-class}},
#' \code{\link[koRpus]{kRp.readability-class}}, \code{\link[koRpus]{kRp.corp.freq-class}} or
#' \code{\link[koRpus]{kRp.TTR-class}}.
#'
#' @param object An object of class \code{kRp.lang}, \code{kRp.readability}, \code{kRp.corp.freq}
#'		or \code{kRp.TTR}.
#' @aliases show,-methods show,kRp.lang-method
#' @seealso
#'		\code{\link[koRpus]{kRp.lang-class}},
#'		\code{\link[koRpus]{kRp.readability-class}},
#'		\code{\link[koRpus]{kRp.corp.freq-class}},
#'		\code{\link[koRpus]{kRp.TTR-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#'   guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/")
#' }
#' @export
#' @docType methods
#' @rdname show-methods
setGeneric("show")

#' @rdname show-methods
setMethod("show", signature(object="kRp.lang"), function(object){
	estim.lang <- object@lang.name
	estim.lang.uli <- object@lang
	estim.lang.country <- object@udhr[1,"country"]
	estim.lang.region <- object@udhr[1,"region"]
	langs.available <- dim(object@udhr)[1]

	cat("\n  Estimated language: ", estim.lang,
		 "\n          Identifier: ", estim.lang.uli,
		 "\n             Country: ", estim.lang.country, " (", estim.lang.region,")\n",
		 "\n", langs.available, " different languages were checked.\n\n",
		 sep="")

})
