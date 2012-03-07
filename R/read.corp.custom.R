#' Import custom corpus data
#'
#' Read data from a custom corpus into a valid object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#'
#' The function should enable you to perform a basic text corpus frequency analysis. That is, not just to
#' import analysis results like LCC files, but to import the corpus material itself. The resulting object
#' is of class \code{\link[koRpus]{kRp.corp.freq-class}}, so it can be used for frequency analysis by
#' other functions of this package.
#'
#' @param corpus Either the path to directory with txt files to read and analyze, or a vector object already holding the text corpus.
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the given object.
#' @param fileEncoding A character string naming the encoding of the corpus files.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param ... Additional options to be passed through to the \code{tokenize} function.
#' @return An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords corpora
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}
#' @export
#' @examples
#' \dontrun{
#' ru.corp <- read.corp.custom("~/mydata/corpora/russian_corpus/")
#' }

read.corp.custom <- function(corpus, format="file", fileEncoding="UTF-8", quiet=FALSE, ...){

	# leftover of an earlier attempt; the matrix idea could be resurrected sometime
	# 	} else if(is.matrix(corpus) & identical(dimnames(corpus)[[2]], c("token", "freq"))){
	# 		run.tokenizer <- FALSE
	# 	} else {
	# 		stop(simpleError("\"corpus\" must be an existing file or directory, or a matrix object with columns \"token\" and \"freq\"!"))
	# 	}

	tokens <- tokenize(txt=corpus, format=format, fileEncoding=fileEncoding, tag=FALSE, ...)

	# get types
	types <- unique(tokens)
	num.tokens <- length(tokens)
	num.types <- length(types)

	## now do the counting!
	corp.freq <- matrix(ncol=2, dimnames=list(c(), c("word", "freq")))[-1,]
	type.counter <- 1
	for (tp in types){
		if(!isTRUE(quiet)){
			cat(paste("\t", floor(100*type.counter/num.types), "% complete, processing token ", type.counter, " of ", num.types, ": \"", tp, "\"", sep=""))
		} else {}
		type.freq <- sum(match(tokens, tp), na.rm=TRUE)
		if(!isTRUE(quiet)){
			cat(paste(" (found ", type.freq, " times in ", num.tokens, " tokens)\n", sep=""))
		} else {}
		corp.freq <- rbind(corp.freq, c(word=tp, freq=type.freq))
		type.counter <- type.counter + 1
	}

	# sort the matrix
	corp.freq <- corp.freq[order(as.numeric(corp.freq[,"freq"]), decreasing=TRUE), ]
	# add num variable
	corp.freq <- cbind(num=1:num.types, corp.freq)

#	return(corp.freq)

	# descriptive statistics
	dscrpt.meta <- data.frame(
		tokens=num.tokens,
		types=num.types,
		words.p.sntc=NA,
		chars.p.sntc=NA,
		chars.p.wform=NA,
		chars.p.word=NA)

	# call internal function create.corp.freq.object()
	results <- create.corp.freq.object(matrix.freq=corp.freq,
						num.running.words=num.tokens,
						df.meta=as.data.frame(matrix(ncol=2, dimnames=list(c(),c("meta", "value")))),
						df.dscrpt.meta=dscrpt.meta)

	return(results)
}
