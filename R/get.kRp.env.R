#' Get koRpus session environment
#'
#' The function \code{get.kRp.env} returns information on your session environment regarding the koRpus package, e.g.
#' A function to get information on your koRpus environment
#'
#' where your local TreeTagger installation resides, if it was set before using
#' \code{\link[koRpus:set.kRp.env]{set.kRp.env}}.
#'
#' @param TT.cmd Logical, whether the set tagger command should be returned.
#' @param lang Logical, whether the set language should be returned.
#' @param TT.options Logical, whether the set TT.options for \code{treetag} should be returned.
#' @return A character string or list, possibly including:
#'	\item{TT.cmd}{Path information for the TreeTagger command}
#'	\item{lang}{The specified language}
#'	\item{TT.options}{A list with options for \code{treetag}}
#' If the desired property is not set at all, the function will fail with an error message.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @seealso \code{\link[koRpus:set.kRp.env]{set.kRp.env}}
#' @export
#' @examples
#' \dontrun{
#' set.kRp.env(TT.cmd="~/bin/treetagger/cmd/tree-tagger-german", lang="de")
#' get.kRp.env(TT.cmd=TRUE)
#' }

	get.kRp.env <- function(TT.cmd=FALSE, lang=FALSE, TT.options=FALSE){
	tt.env <- list()
	if(isTRUE(TT.cmd)){
		if(exists("TT.cmd", envir=.koRpus.env, inherits=FALSE)){
			tt.env$TT.cmd <- get("TT.cmd", envir=.koRpus.env)
			if(!identical(tt.env$TT.cmd, "manual") & !identical(tt.env$TT.cmd, "tokenize")){
				stopifnot(check.file(tt.env$TT.cmd, mode="exec"))
			} else {}
		} else {
			stop(simpleError("No TreeTagger command specified! If you want to use TreeTagger, you must tell treetag() where it is installed, either by using set.kRp.env() or setting 'treetagger' an 'TT.options' accordingly."))
		}
	} else {}

	if(isTRUE(lang)){
		if(exists("lang", envir=.koRpus.env, inherits=FALSE)){
			tt.env$lang <- get("lang", envir=.koRpus.env)
		} else {
			stop(simpleError("No language specified!"))
		}
	} else {}

	if(isTRUE(TT.options)){
		if(exists("TT.options", envir=.koRpus.env, inherits=FALSE)){
			tt.env$TT.options <- get("TT.options", envir=.koRpus.env)
		} else {
			stop(simpleError("No TT.options specified!"))
		}
	} else {}

	if(length(tt.env) == 1){
		tt.env <- tt.env[[1]]
	} else {}

	return(tt.env)
}
