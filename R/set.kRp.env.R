#' A function to set information on your koRpus environmenton
#'
#' The function \code{set.kRp.env} can be called once before any of the analysing functions. It writes information
#' on your session environment regarding the koRpus package, e.g. path to a local TreeTagger installation,
#' to a hidden environment.
#'
#' To get the contents of the hitten environment, the function \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' can be used.
#'
#' @param TT.cmd A character string pointing to the tagger command you want to use for basic text analysis, or \code{"manual"} if you want to set \code{TT.options} as well. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param lang A character string specifying a valid language.
#' @param TT.options A list with arguments to be used as \code{TT.options} by \code{treetag}.
#' @return Returns an invisible \code{NULL}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}
#' @export
#' @examples
#' \dontrun{
#' set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="de")
#' get.kRp.env(TT.cmd=TRUE)
#' }

	set.kRp.env <- function(TT.cmd=NULL, lang=NULL, TT.options=NULL){
	if(!is.null(TT.cmd)){
		if(!identical(TT.cmd, "manual") & !identical(TT.cmd, "tokenize")){
			stopifnot(check.file(TT.cmd, mode="exec"))
			assign("TT.cmd", file.path(TT.cmd), envir=.koRpus.env)
		} else {
			assign("TT.cmd", TT.cmd, envir=.koRpus.env)
		}
	} else {}

	if(!is.null(lang)){
		stopifnot(is.character(lang))
		assign("lang", lang, envir=.koRpus.env)
	} else {}

	if(!is.null(TT.options)){
		stopifnot(is.list(TT.options))
		# do some sanitiy checks
		if(!"path" %in% names(TT.options)){
			stop(simpleError("Manual TreeTagger configuration demanded, but not even a path was defined!"))
		} else {}
		stopifnot(check.file(TT.options$path, mode="dir"))
		# TODO: move TT.options checks to internal function to call it here

		assign("TT.options", TT.options, envir=.koRpus.env)
	} else {}
	return(invisible(NULL))
}
