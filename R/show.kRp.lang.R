#' Show methods for objects of class kRp.lang
#'
#' Show methods for S4 objects of class \code{\link[koRpus]{kRp.lang-class}}
#'
#' @param object An object of class \code{kRp.lang}
#' @aliases show,-methods show,kRp.lang-method
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso \code{\link[koRpus]{kRp.lang-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#'   guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/")
#' }
#' @rdname show-methods
setGeneric("show")

#' @exportMethod show
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
