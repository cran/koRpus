#' Methods to correct koRpus objects
#' 
#' The methods \code{correct.tag} and \code{correct.hyph} can be used to alter objects of class \code{\link[koRpus]{kRp.tagged-class}},
#' or of class \code{\link[koRpus]{kRp.hyphen-class}} respectively.
#'
#' Although automatic POS tagging, lemmatization and hyphenation are remarkably accurate, the algorithms do ususally produce
#' some errors. If you want to correct for these flaws, these methods can be of help, because they might prevent you from
#' introducing new errors. That is, the will do some sanitiy checks before the object is actually manipulated and returned:
#'
#' \describe{
#'		\item{\code{correct.tag}}{will read the \code{lang} slot from the given object and check whether the \code{tag}
#'			provided is actually valid. If so, it will not only change the \code{tag} field in the object, but also update
#'			\code{wclass} and \code{desc} accordingly.
#'
#'			If \code{check.token} is set it must also match \code{token} in the given row(s). Note that no check is done on the lemmata.}
#'		\item{\code{correct.hyph}}{will check whether \code{word} and \code{hyphen} are actually hyphenations of the
#'			same token before proceeding. If so, it will also recalculate the number of syllables and update the \code{syll}
#'			field.
#'
#'			If both \code{word} and \code{hyphen} are \code{NULL}, \code{correct.hyph} will try to simply recalculate the syllable count
#'			for each word, by counting the hyphenation marks (and adding 1 to the number). This can be usefull if you changed hyphenation
#'			some other way, e.g. in a spreadsheet GUI, but don't want to have to correct the syllable count yourself as well.}
#' }
#'
#' @usage # for S4 objects of class kRp.tagged
#' correct.tag(obj, row, tag=NULL, lemma=NULL, check.token=NULL)
#'
#' # for S4 objects of class kRp.hyphen
#' correct.hyph(obj, word=NULL, hyphen=NULL, cache=TRUE)
#' @param obj An object of class \code{\link[koRpus]{kRp.tagged-class}} or \code{\link[koRpus]{kRp.hyphen-class}}.
#' @param row Integer, the row number of the entry to be changed. Can be an integer vector
#'		to change several rows in one go.
#' @param word A character string, the (possibly incorrectly hyphenated) \code{word} entry to be replaced with \code{hyphen}.
#' @param tag A character string with a valid POS tag to replace the current tag entry.
#'		If \code{NULL} (the default) the entry remains unchanged.
#' @param lemma A character string naming the lemma to to replace the current lemma entry.
#'		If \code{NULL} (the default) the entry remains unchanged.
#' @param check.token A character string naming the token you expect to be in this row.
#'		If not \code{NULL}, \code{correct} will stop with an error if this values don't match.
#' @param hyphen A character string, the new manually hyphenated version of \code{word}. Mustn't contain
#'		anything other than characters of \code{word} plus the hyphenation mark \code{"-"}.
#' @param cache Logical, if \code{TRUE}, the given hyphenation will be added to the sessions' hyphenation cache.
#'		Existing entries for the same word will be replaced.
#' @return An object of the same class as \code{obj}.
#' @aliases correct.tag,-methods correct.tag,kRp.tagged-method correct.hyph,-methods correct.hyph,kRp.hyphen-method
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords methods
#' @seealso \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus:treetag]{treetag}},
#'		\code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}}.
#' @include kRp.TTR-class.R
#' @include kRp.tagged-class.R
#' @include kRp.hyphen-class.R
#' @examples
#' \dontrun{
#' tagged.txt <- correct.tag(tagged.txt, row=21, tag="NN")
#'
#' hyphenated.txt <- correct.hyph(hyphenated.txt, "Hilfe", "Hil-fe")
#' }
#' @rdname correct-methods
setGeneric("correct.tag", function(obj, row, tag=NULL, lemma=NULL, check.token=NULL){standardGeneric("correct.tag")})

#' @exportMethod correct.tag
#' @rdname correct-methods
setMethod("correct.tag",
    signature(obj="kRp.tagged"),
    function (obj, row, tag=NULL, lemma=NULL, check.token=NULL){

			if(!is.numeric(row)){
				stop(simpleError("Not a valid row number!"))
			} else {}

			local.obj.copy <- obj
			lang <- obj@lang

			if(!is.null(tag)){
				# before we attempt anything, let's check if this is a valid tag
				valid.POS.tags <- kRp.POS.tags(lang, list.tags=TRUE)
				if(is.na(match(tag, valid.POS.tags))){
					stop(simpleError(paste("Not a valid POS tag for language \"", lang, "\": ", tag, sep="")))
				} else {}
				all.POS.tags <- kRp.POS.tags(lang)
				# this object will hold the columns "tag", "wclass" and "desc" for our tag
				new.tag <- all.POS.tags[all.POS.tags[,"tag"] == tag, ]
				for (cur.row in row){
					if(!is.null(check.token) & !identical(local.obj.copy@TT.res[cur.row, "token"], check.token)){
						stop(simpleError(paste("In row ", cur.row,", expected \"", check.token,"\" but got \"", local.obj.copy@TT.res[cur.row, "token"],"\"!", sep="")))
					} else {}
					local.obj.copy@TT.res[cur.row, c("tag","wclass","desc")] <- new.tag[c("tag","wclass","desc")]
				}
			} else {}
			if(!is.null(lemma)){
				for (cur.row in row){
					if(!is.null(check.token) & !identical(local.obj.copy@TT.res[cur.row, "token"], check.token)){
						stop(simpleError(paste("In row ", cur.row,", expected \"", check.token,"\" but got \"", local.obj.copy@TT.res[cur.row, "token"],"\"!", sep="")))
					} else {}
					local.obj.copy@TT.res[cur.row, "lemma"] <- lemma
				}
			} else {}

			cat("Changed\n\n")
			print(obj@TT.res[row, ])
			cat("\n  into\n\n")
			print(local.obj.copy@TT.res[row, ])

			return(local.obj.copy)
    }
)

#' @rdname correct-methods
setGeneric("correct.hyph", function(obj, word=NULL, hyphen=NULL, cache=TRUE){standardGeneric("correct.hyph")})

#' @exportMethod correct.hyph
#' @rdname correct-methods
setMethod("correct.hyph",
    signature(obj="kRp.hyphen"),
    function (obj, word=NULL, hyphen=NULL, cache=TRUE){
			lang <- obj@lang
			local.obj.copy <- obj

			if(!is.null(word) & !is.null(hyphen)){
				# recount syllables
				new.syll <- sum(grepl("-", unlist(strsplit(hyphen, split="")))) + 1

				matching.rows <- which(local.obj.copy@hyphen[, "word"] == word)
				# any matches at all?
				if(length(matching.rows) == 0){
					warning(paste("Sorry, no matches for \"", word,"\" in ", substitute(obj), "!", sep=""))
					return(obj)
				} else {}

				# check if hyphen is actually a hyphenated version of word!
				old.word <- gsub("-", "", word)
				new.word <- gsub("-", "", hyphen)
				if(!identical(old.word, new.word)){
					stop(simpleError(paste("\"", hyphen, "\" is not a valid hyphenation of \"", old.word, "\"!", sep="")))
				} else {}
				local.obj.copy@hyphen[matching.rows, "syll"] <- new.syll
				local.obj.copy@hyphen[matching.rows, "word"] <- hyphen

				# now check the cache
				if(isTRUE(cache)){
					# get current koRpus environment
					all.kRp.env <- as.list(as.environment(.koRpus.env))
					recent.cache <- all.kRp.env[["hyphenCache"]][[lang]]
					# could be there is no such entries in the environment yet
					if(is.null(recent.cache)){
						recent.cache <- data.frame(token="", syll=0, word="", stringsAsFactors=FALSE)[-1,]
					} else {
						# check if this word was hyphenated before
						token <- gsub("-", "", word)
						inCache <- which(recent.cache[,"token"] == token)
						if(length(inCache) > 0){
							recent.cache[inCache,"syll"] <- new.syll
							recent.cache[inCache,"word"] <- hyphen
						} else {
							recent.cache <- rbind(recent.cache, c(word, new.syll, hyphen))
						}
					}
					# write back the changes
					all.kRp.env[["hyphenCache"]][[lang]] <- recent.cache
					list2env(all.kRp.env, envir=as.environment(.koRpus.env))
				} else {}

			} else if(is.null(word) & is.null(hyphen)){
				new.syll <- as.numeric(sapply(local.obj.copy@hyphen$word, function(word){sum(grepl("-", unlist(strsplit(word, split="")))) + 1}))
				matching.rows <- which(local.obj.copy@hyphen$syll != new.syll)
				# any mathes at all?
				if(length(matching.rows) == 0){
					message("Nothing was changed!")
					return(obj)
				} else {}
				# recount syllables
				local.obj.copy@hyphen[matching.rows, "syll"] <- new.syll[matching.rows]
			} else {
				stop(simpleError(paste("Either \"word\" or \"hyphen\" is missing!")))
			}

			cat("Changed\n\n")
			print(obj@hyphen[matching.rows, ])
			cat("\n  into\n\n")
			print(local.obj.copy@hyphen[matching.rows, ])

			return(local.obj.copy)
    }
)
