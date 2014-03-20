# Copyright 2010-2014 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.
#
# koRpus is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.  If not, see <http://www.gnu.org/licenses/>.


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
#'    Can also be an already tokenized and tagged text object which inherits class \code{kRp.tagged} (then the column \code{"token"} of
#'    the \code{"TT.res"} slot is used).
#' @param format Either "file" or "obj", depending on whether you want to scan files or analyze the given object.
#' @param fileEncoding A character string naming the encoding of the corpus files.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#' @param caseSens Logical. If \code{FALSE}, all tokens will be matched in their lower case form.
#' @param log.base A numeric value defining the base of the logarithm used for inverse document frequency (idf). See
#'    \code{\link[base:log]{log}} for details.
#' @param ... Additional options to be passed through to the \code{tokenize} function.
#' @return An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords corpora
#' @seealso \code{\link[koRpus]{kRp.corp.freq-class}}
#' @export
#' @examples
#' \dontrun{
#' ru.corp <- read.corp.custom("~/mydata/corpora/russian_corpus/")
#' }

read.corp.custom <- function(corpus, format="file", fileEncoding="UTF-8", quiet=TRUE, caseSens=TRUE, log.base=10, ...){

  if(inherits(corpus, "kRp.tagged")){
    tokens <- slot(corpus, "TT.res")[["token"]]
  } else {
    # for inverse document frequency we need statistics per document,
    # therefore keep a list with individual results and flatten that later
    if(identical(format, "file") && check.file(corpus, mode="dir", stopOnFail=FALSE)){
      txt.files <- dir(corpus)
      tokensList <- lapply(txt.files, function(this.txt.file){
          txt.full.path <- file.path(corpus, this.txt.file)
          return(tokenize(txt=txt.full.path, format=format, fileEncoding=fileEncoding, tag=FALSE, ...))
        })
      names(tokensList) <- txt.files
      numDocs <- length(txt.files)
      tokens <- unlist(tokensList)
    } else {
      tokens <- tokenize(txt=corpus, format=format, fileEncoding=fileEncoding, tag=FALSE, ...)
      # for compatibility with the directory option
      txt.files <- "vector"
      numDocs <- 1
      tokensList <- list(vector=tokens)
    }
  }
  if(!isTRUE(caseSens)){
    tokens <- tolower(tokens)
  } else {}

  # this can be handled quick if quiet=TRUE, by using table()
  if(isTRUE(quiet)){
    freq.df <- frqcy.of.types(tokens=tokens, byTypes=TRUE, byTokens=FALSE)
    types <- freq.df[["byTypes"]][["type"]]
    num.tokens <- sum(freq.df[["byTypes"]][["freq"]])
    num.types <- nrow(freq.df[["byTypes"]])
    corp.freq <- matrix(
      data=c(types, freq.df[["byTypes"]][["freq"]]),
      ncol=2, dimnames=list(c(), c("word", "freq")))
  } else {
    # get types
    types <- unique(tokens)
    num.tokens <- length(tokens)
    num.types <- length(types)

    ## now do the counting!
    corp.freq <- matrix(ncol=2, dimnames=list(c(), c("word", "freq")))[-1,]
    type.counter <- 1
    for (tp in types){
      cat(paste0("\t", floor(100 * type.counter / num.types), "% complete, processing token ", type.counter, " of ", num.types, ": \"", tp, "\""))
      type.freq <- sum(match(tokens, tp), na.rm=TRUE)
      if(!isTRUE(quiet)){
        cat(paste0(" (found ", type.freq, " times in ", num.tokens, " tokens)\n"))
      } else {}
      corp.freq <- rbind(corp.freq, c(word=tp, freq=type.freq))
      type.counter <- type.counter + 1
    }
  }

  ## calculate inverse document frequency
  # log(total number of documents / number of documents containing type)
  # first create a matrix with types (rows) by documents
  inDocsMatrix <- sapply(tokensList, function(this.doc){
    return(types %in% this.doc)
  })
  rownames(inDocsMatrix) <- types
  # now count how often each type was present in the documents
  inDocsNum <- rowSums(inDocsMatrix)
  # and finally the idf
  idf <- log(numDocs / inDocsNum, base=log.base)
  corp.freq <- cbind(corp.freq, inDocs=inDocsNum)
  corp.freq <- cbind(corp.freq, idf=idf)
  
  # sort the matrix
  corp.freq <- corp.freq[order(as.numeric(corp.freq[,"freq"]), decreasing=TRUE), ]
  # add num variable
  corp.freq <- cbind(num=1:num.types, corp.freq)

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
