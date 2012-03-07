#' A method to get information out of koRpus objects
#'
#' The method \code{query} returns query information from objects of classes \code{\link[koRpus]{kRp.corp.freq-class}} and
#' \code{\link[koRpus]{kRp.tagged-class}}.
#'
#' \emph{kRp.corp.freq:} Depending on the setting of the \code{var} parameter, will return entries with a matching character (\code{var="word"}),
#' or all entries of the desired frequency (see the examples). A special case is the need for a range of frequencies,
#' which can be achieved by providing a nomerical vector of two values as the \code{query} value, for start and end of
#' the range, respectively. In these cases, if \code{rel} is set to \code{"gt"} or \code{"lt"},
#' the given range borders are excluded, otherwise they will be included as true matches.
#'
#' \emph{kRp.tagged:} \code{var} can be any of the variables in slot \code{TT.res}. For \code{rel} currently only
#' "eq" and "num" are implemented. The latter isn't a relation, but will return a vector with the row numbers in which
#' the query was found.
#'
#' This method calls \code{\link[base]{subset}}, which might actually be even more flexible if you need more control.
#'
#' @param obj An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#' @param var A character string naming a variable in the object.
#' @param query A character vector (for words) or single number naming values to be matched in the variable.
#'		Can be a vector of two numbers to query a range of frequency data.
#' @param rel A character string defining the relation of the queried value and desired results.
#'		Must either be \code{"eq"} (equal, the default), \code{"gt"} (greater than), \code{"ge"} (greater of equal),
#'		\code{"lt"} (less than) or \code{"le"} (less or equal). If \code{var="word"}, is always interpreted as \code{"eq"}
#' @aliases query,-methods query,kRp.corp.freq-method query,kRp.tagged-method
#' @return Depending on the arguments, might include whole objects, lists, single values etc.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords methods
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}, \code{\link[base]{subset}}
#' @examples
#' \dontrun{
#' # look up frequencies for the word "aber"
#' query(LCC.data, var="word", query="aber")
#' # show all entries with a frequency of exactly 3000 in the corpus
#' query(LCC.data, "freq", 3000)
#' # now, which words appear more than 40000 times in a million?
#' query(LCC.data, "pmio", 40000, "gt")
#' # example for a range request: words with a log10 between 2 and 2.1
#' # (including these two values)
#' query(LCC.data, "log10", c(2, 2.1))
#' # (and without them)
#' query(LCC.data, "log10", c(2, 2.1), "gt")
#' # get all "he" lemmata in a previously tagged text object
#' query(tagged.txt, "lemma", "he")
#' }
#' @rdname query-methods

setGeneric("query", function(obj, var, query, rel="eq")	standardGeneric("query"))

#' @exportMethod query
#' @rdname query-methods
setMethod("query",
    signature(obj="kRp.corp.freq"),
    function (obj, var, query, rel){
			stopifnot(length(var) == 1)
			results <- ""
			# basically, we have to see if we're looking for a word or for frequencies
			if(identical(var, "word")){
				results <- subset(obj@words, word == query)
			} else{}
			# in case we're looking for anything frequency related
			if(var %in% c("freq", "log10", "num", "pmio", "pct", "rank.avg", "rank.rel.min", "rank.rel.avg", "rank.min")){
				if(length(query) == 1){
					if(identical(rel, "eq")){
						results <- subset(obj@words, eval(parse(text=paste(var, "== query"))))
					} else{}
					if(identical(rel, "gt")){
						results <- subset(obj@words, eval(parse(text=paste(var, "> query"))))
					} else{}
					if(identical(rel, "ge")){
						results <- subset(obj@words, eval(parse(text=paste(var, ">= query"))))
					} else{}
					if(identical(rel, "lt")){
						results <- subset(obj@words, eval(parse(text=paste(var, "< query"))))
					} else{}
					if(identical(rel, "le")){
						results <- subset(obj@words, eval(parse(text=paste(var, "<= query"))))
					} else{}
				} else if(length(query) == 2){
					if(rel %in% c("gt", "lt")){
						results <- subset(obj@words, eval(parse(text=paste("(", var, "> query[1] ) & (", var, "< query[2] )"))))
					} else{
						results <- subset(obj@words, eval(parse(text=paste("(", var, ">= query[1] ) & (", var, "<= query[2] )"))))
					}
				} else{}
			} else{}

		if(identical(results, "")){
			stop(simpleError("Unable to comply."))
		} else{}
		return(results)
    }
)

#' @rdname query-methods
setMethod("query",
    signature(obj="kRp.tagged"),
    function (obj, var, query, rel){
			if(!rel %in% c("eq", "num")){
				stop(simpleError(paste("Invalid rel for class kRp.tagged:", rel)))
			} else {}

			stopifnot(length(var) == 1)
			if(var %in% names(obj@TT.res)){
				num.findings <- which(obj@TT.res[[var]] %in% query)
				if(identical(rel, "num")){
					results <- num.findings
				} else {
					results <- obj@TT.res[num.findings, ]
				}
			} else {
				stop(simpleError(paste("Invalid var for class kRp.tagged:", var)))
			}
		if(identical(results, "")){
			stop(simpleError("Unable to comply."))
		} else{}
		return(results)
    }
)
