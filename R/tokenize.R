#' A simple tokenizer
#'
#' This tokenizer can be used to try replace TreeTagger. Its results are not as detailed when it comes to word classes, and no
#' lemmatization is done. However, for most cases this should suffice.
#'
#' \code{tokenize} can try to guess what's a headline and where a paragraph was inserted (via the \code{detect} parameter).
#' A headline is assumed if a line of text without sentence ending punctuation is found, a paragraph if two blocks of text
#' are separated by space. This will add extra tags into the text: "<kRp.h>" (headline starts), "</kRp.h>" (headline ends)
#' and "<kRp.p/>" (paragraph), respectively. This can be useful in two cases: "</kRp.h>" will be treated like a sentence ending,
#' which gives you more control for automatic analyses. And adding to that, \code{\link[koRpus:kRp.text.paste]{kRp.text.paste}}
#' can replace these tags, which probably preserves more of the original layout.
#'
#' @param txt Either an open connection, the path to directory with txt files to read and tokenize, or a vector object
#'		already holding the text corpus.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the given object.
#' @param fileEncoding A character string naming the encoding of all files.
#' @param split A regular expression to define the basic split method. Should only need refinement
#'		for languages that don't separate words by space.
#' @param ign.comp A character vector defining punctuation which might be used in composita that should 
#'		not be split.
#' @param heuristics A vector to indicate if the tokenizer should use some heuristics. Can be none, one or several of the following:
#'		\itemize{
#'			\item{\code{"abbr"}}{Assume that "letter-dot-letter-dot" combinations are abbreviations and leave them intact.}
#'			\item{\code{"en"}}{Try to detect possesive suffixes like "'s", or shorting suffixes like "'ll" and treat them as one token}
#'			\item{\code{"fr"}}{Try to detect prefixes like "s'" or "l'" and treat them as one token}
#'		}
#' @param heur.fix A list with the named vectors \code{pre} and \code{suf}. These will be used if \code{heuristics} were
#'		set to use one of the presets that try to detect pre- and/or suffixes. Change them if you document uses other
#'		characters than the ones defined by default.
#' @param abbrev Path to a text file with abbreviations to take care of, one per line. Note that
#'		this file must have the same encoding as defined by \code{fileEncoding}.
#' @param tag Logical. If \code{TRUE}, the text will be rudimentarily tagged and returned as an object
#'		of class \code{kRp.tagged}.
#' @param lang A character string naming the language of the analyzed text. If set to \code{"kRp.env"} this is got from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Only needed if \code{tag=TRUE}.
#' @param sentc.end A character vector with tokens indicating a sentence ending. Only needed if \code{tag=TRUE}.
#' @param detect A named logical vector, indicating by the setting of \code{parag} and \code{hline} whether \code{tokenize} should try
#'		to detect paragraphs and headlines.
#' @return If \code{tag=FALSE}, a character vector with the tokenized text. If \code{tag=TRUE}, returns an object of class \code{\link[koRpus]{kRp.tagged-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @export
#' @examples
#' \dontrun{
#' tokenized.obj <- tokenize("~/mydata/corpora/russian_corpus/")
#' }

tokenize <- function(txt, format="file", fileEncoding=NULL, split="[[:space:]]",
					ign.comp="-", heuristics="abbr", heur.fix=list(pre=c("\u2019","'"), suf=c("\u2019","'")),
					abbrev=NULL, tag=TRUE, lang="kRp.env", sentc.end=c(".","!","?",";",":"),
					detect=c(parag=FALSE, hline=FALSE)){

	if(is.null(fileEncoding)){
		fileEncoding <- ""
	} else {}

	# basic checks before we even proceed...
	if(inherits(txt, "connection")){
		takeAsTxt <- readLines(txt, encoding=fileEncoding)
		read.txt.files <- FALSE
	} else if(identical(format, "file")){
		# valid path? file or directory?
		if(check.file(txt, mode="exist", stopOnFail=FALSE)){
			txt.file <- txt
			read.txt.files <- TRUE
		} else if(check.file(txt, mode="dir", stopOnFail=FALSE)){
			txt.file <- file.path(txt, dir(txt))
			read.txt.files <- TRUE
		} else {
			stop(simpleError(paste("Unable to locate\n ",txt, sep="")))
		}
	} else if(identical(format, "obj")){
		takeAsTxt <- txt
		read.txt.files <- FALSE
	} else {
		stop(simpleError(paste("Invalid value for format: ",format, sep="")))
	}

	## read file or text vector?
	if(isTRUE(read.txt.files)){
		# read in files
		# make sure we end up with UTF-8 to avoid nasty character problems
		txt.vector <- unlist(lapply(txt.file, function(txt){
				readLines(txt, encoding=fileEncoding)
			}))
		# force text into UTF-8 format
		txt.vector <- enc2utf8(txt.vector)
	} else {
		# process object
		txt.vector <- enc2utf8(as.vector(takeAsTxt))
	}

	## run the tokenizer
	# tokenz() is an internal function
	tokens <- tokenz(txt.vector, split=split, ign.comp=ign.comp, encoding=fileEncoding,
					heuristics=heuristics, heur.fix=heur.fix, abbrev=abbrev, tag=tag, sntc=sentc.end, detect=detect)

	if(isTRUE(tag)){
		if(identical(lang, "kRp.env")){
			lang <- get.kRp.env(lang=TRUE)
		} else {}
		# prepare commenting by adding empty lemma column
		tagged.mtrx <- cbind(tokens, lemma="")
		# add word classes, comments and numer of letters ("wclass", "desc", "lttr")
		tagged.mtrx <- treetag.com(tagged.mtrx, lang=lang)
		# create object, combine descriptives afterwards
		tokens <- new("kRp.tagged", lang=lang, TT.res=tagged.mtrx)
		## descriptive statistics
		tokens@desc <- basic.tagged.descriptives(tokens, lang=lang, txt.vector=txt.vector)
	} else {}

	return(tokens)
}
