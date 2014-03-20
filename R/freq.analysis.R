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


#' Analyze word frequencies
#'
#' The function \code{freq.analysis} analyzes texts regarding frequencies of tokens, word classes etc.
#'
#' The easiest way to see what kinds of analyses are done is probably to look at the slot description of \code{\link[koRpus]{kRp.txt.freq-class}}.
#'
#' By default, if the text has yet to be tagged, the language definition is queried by calling \code{get.kRp.env(lang=TRUE)} internally.
#' Or, if \code{txt.file} has already been tagged, by default the language definition of that tagged object is read
#' and used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to any other valid value, if you want to forcibly overwrite this
#' default behaviour, and only then. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'
#' @note Prior to \code{koRpus} 0.04-29, this function was named \code{kRp.freq.analysis()}.
#'    For backwards compatibility there is a wrapper function, but it should be considered
#'    deprecated.
#'
#' @param txt.file Either an object of class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}},
#'    \code{\link[koRpus]{kRp.analysis-class}} or \code{\link[koRpus]{kRp.txt.trans-class}}, or a character vector which must
#'    be a valid path to a file containing the text to be analyzed.
#' @param corp.freq An object of class \code{\link[koRpus]{kRp.corp.freq-class}}.
#' @param desc.stat Logical, whether a descriptive statistical analysis should be performed.
#' @param force.lang A character string defining the language to be assumed for the text, by force.
#' @param tagger A character string defining the tokenizer/tagger command you want to use for basic text analysis. Can be omitted if
#'    \code{txt.file} is already of class \code{kRp.tagged-class}. Defaults to \code{"kRp.env"} to get the settings by
#'    \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Set to \code{"tokenize"} to use \code{\link[koRpus:tokenize]{tokenize}}.
#' @param corp.rm.class A character vector with word classes which should be ignored for frequency analysis. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be ignored for frequency analysis.
#' @param tfidf Logical, whether the term frequency--inverse document frequency statistic (tf-idf) should be computed. Requires
#'    \code{corp.freq} to provide appropriate idf values for the types in \code{txt.file}. Missing idf values will result in \code{NA}.
#' @param ... Additional options to be passed through to the function defined with \code{tagger}.
#' @return An object of class \code{\link[koRpus]{kRp.txt.freq-class}}.
#' @keywords misc
#' @seealso \code{\link[koRpus:get.kRp.env]{get.kRp.env}}, \code{\link[koRpus]{kRp.tagged-class}},
#'    \code{\link[koRpus]{kRp.corp.freq-class}}
#' @export
#' @rdname freq.analysis
#' @examples
#' \dontrun{
#' freq.analysis("~/some/text.txt", corp.freq=my.LCC.data)
#' }
#' @include 00_class_03_kRp.txt.freq.R
freq.analysis <- function(txt.file, corp.freq=NULL, desc.stat=TRUE, force.lang=NULL,
                       tagger="kRp.env", corp.rm.class="nonpunct",
                       corp.rm.tag=c(), tfidf=TRUE, ...){

  if("lang" %in% names(list(...))){
    # since 'lang' is a valid argument for treetag(), it might have been set
    stop(simpleError("You defined 'lang' in the '...' argument. This is confusing me! Use 'force.lang' instead."))
  } else {}
  # for backward compatibility
  if("treetagger" %in% names(list(...))){
    stop(simpleError("The option 'treetagger' is deprecated and was removed. Use 'tagger' instead."))
  } else {}

  # the internal function tag.kRp.txt() will return the object unchanged if it
  # is already tagged, so it's safe to call it with the lang set here
  tagged.text <- tag.kRp.txt(txt.file, tagger=tagger, lang=force.lang, objects.only=FALSE, ...)
  # set the language definition
  lang <- language.setting(tagged.text, force.lang)
  commented <- slot(tagged.text, "TT.res")

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  if(!is.null(corp.freq)){
    # before we even start, check if we're alright:
    stopifnot(inherits(corp.freq, "kRp.corp.freq"))
    frequency.pre <- text.freq.analysis(
      txt.commented=commented,
      corp.freq=corp.freq,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      lang=lang,
      tfidf=tfidf)
    # commented will be overwritten with a new version containing percentages for each word
    commented <- frequency.pre[["commented"]]
    frequency.res <- frequency.pre[["freq.analysis"]]
  } else {
    frequency.res <- list(NA)
  }

  if(isTRUE(desc.stat)){
    desc.stat.res <- text.analysis(commented, lang=lang, corp.rm.class=corp.rm.class, corp.rm.tag=corp.rm.tag, desc=tagged.text@desc)
  } else {
    desc.stat.res <- tagged.text@desc
  }

  results <- new("kRp.txt.freq", lang=lang, TT.res=commented, desc=desc.stat.res, freq.analysis=frequency.res)
  return(results)
}

# function for backward compatibility
#' @export
#' @rdname freq.analysis
kRp.freq.analysis <- function(txt.file, corp.freq=NULL, desc.stat=TRUE, force.lang=NULL,
                       tagger="kRp.env", corp.rm.class="nonpunct",
                       corp.rm.tag=c(), ...){
  warning("kRp.freq.analysis() is deprecated and will be removed from this package. Please use freq.analysis() instead.")
  return(freq.analysis(
    txt.file=txt.file,
    corp.freq=corp.freq,
    desc.stat=desc.stat,
    force.lang=force.lang,
    tagger=tagger,
    corp.rm.class=corp.rm.class,
    corp.rm.tag=corp.rm.tag,
    ...))
}
