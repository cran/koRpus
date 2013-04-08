#' Automatic hyphenation
#'
#' This function implements word hyphenation, based on Liang's algorithm.
#'
#' For this to work the function must be told which pattern set it should use to
#' find the right hyphenation spots. If \code{words} is already a tagged object,
#' its language definition might be used. Otherwise, in addition to the words to
#' be processed you must specify \code{hyph.pattern}. You have two options: If you
#' want to use one of the built-in language patterns, just set it to the according
#' language abbrevation. As of this version valid choices are:
#' \itemize{
#'	\item {\code{"de"}} {--- German (new spelling, since 1996)}
#'	\item {\code{"de.old"}} {--- German (old spelling, 1901--1996)}
#'	\item {\code{"en"}} {--- English (UK)}
#'	\item {\code{"en.us"}} {--- English (US)}
#'	\item {\code{"es"}} {--- Spanish}
#'	\item {\code{"fr"}} {--- French}
#'	\item {\code{"it"}} {--- Italian}
#'	\item {\code{"ru"}} {--- Russian}
#' }
#' In case you'd rather use your own pattern set, \code{hyph.pattern} can be an
#' object of class \code{kRp.hyph.pat}, alternatively.
#'
#' The built-in hyphenation patterns were derived from the patterns available on CTAN[1]
#' under the terms of the LaTeX Project Public License[2], see \code{\link[koRpus:hyph.XX]{hyph.XX}}
#' for detailed information.
#'
#' @param words Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}} or
#'		\code{\link[koRpus]{kRp.analysis-class}}, or a character vector with words to be hyphenated.
#' @param hyph.pattern Either an object of class \code{\link[koRpus]{kRp.hyph.pat-class}}, or
#'		a valid character string naming the language of the patterns to be used. See details.
#' @param min.length Integer, number of letters a word must have for considering a hyphenation.
#' @param rm.hyph Logical, whether appearing hyphens in words should be removed before pattern matching.
#' @param corp.rm.class A character vector with word classes which should be ignored. The default value
#'		\code{"nonpunct"} has special meaning and will cause the result of
#'		\code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used. Relevant only if \code{words}
#'		is a valid koRpus object.
#' @param corp.rm.tag A character vector with POS tags which should be ignored. Relevant only if \code{words}
#'		is a valid koRpus object.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param cache Logical. \code{hyphen()} can cache results to speed up the process. If this option is set to \code{TRUE}, the
#'		current cache will be queried and new tokens also be added. Caches are language-specific and reside in an environment,
#'		i.e., they are cleaned at the end of a session.
#' @return An object of class \code{\link[koRpus]{kRp.hyphen-class}}
#' @keywords hyphenation
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso
#'		\code{\link[koRpus:read.hyph.pat]{read.hyph.pat}},
#'		\code{\link[koRpus:manage.hyph.pat]{manage.hyph.pat}}
#' @references
#'	Liang, F.M. (1983). \emph{Word Hy-phen-a-tion by Com-put-er}.
#'			Dissertation, Stanford University, Dept. of Computer Science.
#'
#' [1] \url{http://tug.ctan.org/tex-archive/language/hyph-utf8/tex/generic/hyph-utf8/patterns/}
#'
#' [2] \url{http://www.ctan.org/tex-archive/macros/latex/base/lppl.txt}
#' @export

hyphen <- function(words, hyph.pattern=NULL, min.length=3, rm.hyph=TRUE,
		corp.rm.class="nonpunct",
		corp.rm.tag=c(), quiet=FALSE, cache=TRUE){

	# to avoid needless NOTEs from R CMD check
	token <- NULL

	## global stuff
	# deal with the words object
	if(inherits(words, "kRp.tagged")){
		# get class kRp.tagged from words object
		# the internal function tag.kRp.txt() will return the object unchanged if it
		# is already tagged, so it's safe to call it with the lang set here
		tagged.text <- tag.kRp.txt(words, objects.only=TRUE)
		words <- tagged.txt.rm.classes(tagged.text@TT.res, lemma=FALSE,
			lang=tagged.text@lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag)
		lang.from.tagged <- TRUE
	} else {
		lang.from.tagged <- FALSE
	}
	stopifnot(is.character(words))

	# check for hyphenation pattern.
	if(is.null(hyph.pattern)){
		if(isTRUE(lang.from.tagged)){
			# this way the text object defines pattern language
			lang <- tagged.text@lang
			hyph.pattern <- load.hyph.pattern(lang)
		} else {
			stop(simpleError("No language definition available. Set \"hyph.pattern\"!"))
		}
	} else {
		if(!inherits(hyph.pattern, "kRp.hyph.pat")){
			# the internal function load.hyph.pattern() will return what we need
			hyph.pattern <- load.hyph.pattern(hyph.pattern)
		} else {}
		# the other way: take language from hyph.pattern
		# overwrites lang in tagged.text
		lang <- hyph.pattern@lang
	}

	if(!isTRUE(quiet)){
		# feed back the hypenation we're using
		message(paste0("Hyphenation (language: ", lang, ")"))
	} else {}
	# extract only the pattern matrix
	hyph.pattern <- hyph.pattern@pattern

	# min-lenth and max-length of patterns
	min.pat <- min(nchar(hyph.pattern[,"char"]))
	max.pat <- max(nchar(hyph.pattern[,"char"]))

	## main function
	# build a vector with all possible word fragments
	# check for matches of the fragment vector in the pattern db

	# counter to get some feedback
	.iter.counter <- new.env()
	assign("counter", 1, envir=.iter.counter)
	if(!isTRUE(quiet)){
		# give some feedback, so we know the machine didn't just freeze...
		prgBar <- txtProgressBar(min=0, max=length(words), style=3)
	} else {}

	hyphenate.results <- t(sapply(words, function(word){
		if(!isTRUE(quiet)){
			# update prograss bar
			iteration.counter <- get("counter", envir=.iter.counter)
			setTxtProgressBar(prgBar, iteration.counter)
			assign("counter", iteration.counter + 1, envir=.iter.counter)
		} else {}

		if(isTRUE(cache)){
			# get current koRpus environment
			all.kRp.env <- as.list(as.environment(.koRpus.env))
			recent.cache <- all.kRp.env[["hyphenCache"]][[lang]]
			# could be there is no such entries in the environment yet
			if(is.null(recent.cache)){
				recent.cache <- data.frame(token="", syll=0, word="", stringsAsFactors=FALSE)[-1,]
			} else {
				# check if this word was hyphenated before
				cached.word <- recent.cache[recent.cache[,"token"] == word,]
				if(nrow(cached.word) == 1){
					return(subset(cached.word, select=-token))
				} else {}
			}
		} else {}

		# consider min length of word?
		if(nchar(word) < min.length){
			return(data.frame(syll=1, word=word, stringsAsFactors=FALSE))
		} else {}
		word.orig <- word
		## remove hyphens in word
		if(isTRUE(rm.hyph)){
			word <- gsub("-", "", word)
		} else {}
		# non-letters like leading dots confuse the algorithm. we'll remove any non-alphabetic character
		word <- gsub("[^\\p{L}]+", "", word, perl=TRUE)
		## convert to lowercase
		word <- tolower(word)
		## transform "word" to ".word."
		word.dotted <- paste0(".", word, ".")
		word.length <- nchar(word.dotted)

		## create word fragments ".w", ".wo", ".wor"... "rd."
		# first, define all possible start values. obviously it starts with the first letter
		# since minimal patten length is known, the last start value is (last character - min-length + 1)
		iter.start.points <- c(1:(word.length - min.pat))

		word.fragments <- data.frame(sapply(iter.start.points, function(start){
				# if there's less of the word left than there's patterns to match,
				# don't care about too long patterns
				rest.of.word <- word.length - start
				iter.counter <- min.pat
				iter.counter.max <- min(c(max.pat, max(rest.of.word, min.pat))) + 1
				sub.fragments <- sapply(iter.counter:iter.counter.max, function(frag.stop){
						frag.stop <- (start + frag.stop - 1)
						word.part <- substr(word.dotted, start, frag.stop)
						# return a vector with the fragment and its start/end points in the word
						return(c(frag=word.part, on=start, off=frag.stop))
					})
			}), stringsAsFactors=FALSE)
		# find all matching patterns of the word fragments
		matched.patterns <- rbind(word.fragments, match=hyph.pattern[match(word.fragments["frag",], hyph.pattern[,"char"]),"nums"])
		# now let's add the found matches and find the maximum
		matched.pat.index <- !is.na(matched.patterns["match",])
		if(sum(matched.pat.index) > 0){
			pattern.matrix <- sapply(which(matched.pat.index), function(got.match){
					word.on <- max(1, (as.numeric(matched.patterns["on",got.match]) - 1))
					word.off <- as.numeric(matched.patterns["off",got.match])
					match.num.code <- unlist(strsplit(matched.patterns["match",got.match], split=""))
					results <- c(rep(0, word.on - 1), match.num.code, rep(0, word.length - word.off))
				})
			# this is the vector with the max values for the dotted word
			pattern.max <- as.numeric(apply(pattern.matrix, 1, max))

			# we'll never hyphenate before a word...
			pattern.max <- pattern.max[-c(1, length(pattern.max))]
			# ... and never after the last letter
			pattern.max[length(pattern.max)] <- 0

			# filter odd positions (count syllables)
			possible.hyphs <- (pattern.max %% 2) != 0
			syllables <- sum(possible.hyphs) + 1
			# recreate word with hyphens
			add.hyphen <- which(possible.hyphs)
			if(isTRUE(rm.hyph)){
				hyph.word <- unlist(strsplit(gsub("-", "", word.orig), split=""))
			} else {
				hyph.word <- unlist(strsplit(word.orig, split=""))
			}
			for (letter in add.hyphen) {
				hyph.word[letter] <- paste0(hyph.word[letter], "-")
			}
			hyph.word <- paste(hyph.word, collapse="")
			# in cases where previous hyphenations were already removed and here returned,
			# don't return double them up
			hyph.word <- gsub("-+", "-", hyph.word)
			hyph.result <- data.frame(token=word.orig, syll=syllables, word=hyph.word, stringsAsFactors=FALSE)
		} else {
			## no hyphenation
			hyph.result <- data.frame(token=word.orig, syll=1, word=word, stringsAsFactors=FALSE)
		}
		if(isTRUE(cache)){
			# append result to environment
			recent.cache <- rbind(recent.cache, hyph.result)
			all.kRp.env[["hyphenCache"]][[lang]] <- recent.cache
			list2env(all.kRp.env, envir=as.environment(.koRpus.env))
		} else {}
		return(subset(hyph.result, select=-token))
	}))
	if(!isTRUE(quiet)){
		# close prograss bar
		close(prgBar)
	} else {}

	# final result tuning
	hyph.df <- data.frame(
		syll=as.numeric(hyphenate.results[,"syll"]),
		word=as.character(hyphenate.results[,"word"]),
		stringsAsFactors=FALSE)
	dimnames(hyph.df)[[1]] <- c(1:dim(hyph.df)[[1]])

	# compute descriptive statistics
	num.syll <- sum(hyph.df$syll, na.rm=TRUE)
	syll.distrib <- value.distribs(hyph.df$syll)
	syll.uniq.distrib <- value.distribs(unique(hyph.df)$syll)
	avg.syll.word <- mean(hyph.df$syll, na.rm=TRUE)
	syll.per100 <- avg.syll.word * 100

	desc.stat.res <- list(
		num.syll=num.syll,
		syll.distrib=syll.distrib,
		syll.uniq.distrib=syll.uniq.distrib,
		avg.syll.word=avg.syll.word,
		syll.per100=syll.per100
	)

	results <- new("kRp.hyphen", lang=lang, desc=desc.stat.res, hyphen=hyph.df)

	return(results)
}
