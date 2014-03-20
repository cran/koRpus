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


#' Letter case transformation
#' 
#' Transforms text in koRpus objects token by token.
#'
#' This function is mainly intended to produce text material for experiments.
#'
#' @param txt An object of class \code{\link[koRpus]{kRp.txt.trans-class}}, \code{\link[koRpus]{kRp.tagged-class}},
#'    \code{\link[koRpus]{kRp.txt.freq-class}} or \code{\link[koRpus]{kRp.analysis-class}}.
#' @param scheme One of the following character strings:
#' \itemize{
#'   \item {\code{"minor"}} {Start each word with a lowercase letter.}
#'   \item {\code{"all.minor"}} {Forces all letters into lowercase.}
#'   \item {\code{"major"}} {Start each word with a uppercase letter.}
#'   \item {\code{"all.major"}} {Forces all letters into uppercase.}
#'   \item {\code{"random"}} {Randomly start words with uppercase or lowercase letters.}
#'   \item {\code{"de.norm"}} {German norm: All names, nouns and sentence beginnings start with an uppercase letter,
#'      anything else with a lowercase letter.}
#'   \item {\code{"de.inv"}} {Inversion of \code{"de.norm"}.}
#'   \item {\code{"eu.norm"}} {Usual European cases: Only names and sentence beginnings start with an uppercase letter,
#'      anything else with a lowercase letter.}
#'   \item {\code{"eu.inv"}} {Inversion of \code{"eu.norm"}.}
#'  }
#' @param p Numeric value between 0 and 1. Defines the probability for upper case letters (relevant only
#'    if \code{scheme="random"}).
#' @param paste Logical, see value section.
#' @return By default an object of class \code{\link[koRpus]{kRp.txt.trans-class}} is returned. If \code{paste=TRUE}, returns
#'    an atomic character vector (via \code{\link[koRpus:kRp.text.paste]{kRp.text.paste}}).
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords misc
#' @export
#' @examples
#' \dontrun{
#' tagged.text.obj <- freq.analysis("/some/text.txt", corp.freq=my.LCC.data)
#' kRp.text.transform(tagged.text.obj, scheme="random", paste=TRUE)
#' }

kRp.text.transform <- function(txt, scheme, p=0.5, paste=FALSE){

  # deal with the txt object
  if(inherits(txt, "kRp.tagged")){
    # get class kRp.tagged from txt object
    # the internal function tag.kRp.txt() will return the object unchanged if it
    # is already tagged
    txt.orig <- tag.kRp.txt(txt, objects.only=TRUE)
  } else {
    stop(simpleError("Wrong object class (txt)!"))
  }

  # local copy for alterations
  txt <- txt.orig

  if(identical(scheme, "minor")){
    # change first letter to lower case
    txt@TT.res$token <- text.1st.letter(txt@TT.res$token, "lower")
  } else if(identical(scheme, "all.minor")){
    # change all to lower case
    txt@TT.res$token <- tolower(txt@TT.res$token)
  } else if(identical(scheme, "major")){
    # change first letter to upper case
    txt@TT.res$token <- text.1st.letter(txt@TT.res$token, "upper")
  } else if(identical(scheme, "all.major")){
    # change all to upper case
    txt@TT.res$token <- toupper(txt@TT.res$token)
  } else if(identical(scheme, "random")){
    # randomly begin with upper or lower case letters
    # p defines the probability of upper case
    num.words <- dim(txt@TT.res)[1]
    num.upper <- round(num.words * p)
    upper.select <- 1:num.words %in% sample(1:num.words, num.upper)
    txt@TT.res$token[upper.select] <- text.1st.letter(txt@TT.res$token[upper.select], "upper")
    txt@TT.res$token[!upper.select] <- text.1st.letter(txt@TT.res$token[!upper.select], "lower")
  } else if(scheme %in% c("de.norm", "de.inv", "eu.norm", "eu.inv")){
    # beginning of sentences must begin in upper case
    # we'll define "beginning" as anything after a fullstop
    fullstop.classes <- kRp.POS.tags(txt@lang, list.classes=TRUE, tags="sentc")
    sentc.ends <- txt@TT.res[,"wclass"] %in% fullstop.classes
      # usually the semicolon indicates a new sentence, but you don't begin in uppercase afterwards
      sentc.ends[which(txt@TT.res[,"token"] %in% ";")] <- FALSE
    sentc.begins <- c(TRUE, sentc.ends[-length(sentc.ends)])
      # we must correct for cases where the sentence starts with punctuation, like double quotes
      punctuation.classes <- kRp.POS.tags(txt@lang, list.classes=TRUE, tags="punct")
      sntc.punct <- txt@TT.res[,"wclass"] %in% punctuation.classes
      # which cases need to be taken care of?
      start.with.punct <- which(sntc.punct & sentc.begins)
      for (txt.index in start.with.punct){
        # move the TRUE one up
        sentc.begins[txt.index] <- FALSE
        if(txt.index < length(sentc.begins)){
          sentc.begins[txt.index + 1] <- TRUE
        } else {}
      }
    if(scheme %in% c("de.norm", "de.inv")){
      # find nouns an names
      nouns <- txt@TT.res[,"wclass"] %in% c("noun", "name")
    } else {
      # find proper nouns
      nouns <- txt@TT.res[,"wclass"] %in% "name"
    }
    all.to.upper <- nouns | sentc.begins
    if(scheme %in% c("de.norm", "eu.norm")){
      # write all nouns, names and sentence beginnings starting with upper case
      txt@TT.res$token[all.to.upper] <- text.1st.letter(txt@TT.res$token[all.to.upper], "upper")
      txt@TT.res$token[!all.to.upper] <- text.1st.letter(txt@TT.res$token[!all.to.upper], "lower")
    } else {
      # full inversion of "de.norm"
      txt@TT.res$token[!all.to.upper] <- text.1st.letter(txt@TT.res$token[!all.to.upper], "upper")
      txt@TT.res$token[all.to.upper] <- text.1st.letter(txt@TT.res$token[all.to.upper], "lower")
    }
  } else {
    stop(simpleError("Unknown scheme specified!"))
  }

  if(isTRUE(paste)){
    results <- kRp.text.paste(txt)
  } else {
      tokens.orig      <- txt.orig@TT.res$token
      tokens.orig.np   <- tagged.txt.rm.classes(txt.orig@TT.res, lang=txt.orig@lang, corp.rm.class="nonpunct", corp.rm.tag=c())
      tokens.trans     <- txt@TT.res$token
      tokens.trans.np  <- tagged.txt.rm.classes(txt@TT.res, lang=txt@lang, corp.rm.class="nonpunct", corp.rm.tag=c())

      letters.orig     <- unlist(strsplit(tokens.orig, ""))
      letters.orig.np  <- unlist(strsplit(tokens.orig.np, ""))
      letters.trans    <- unlist(strsplit(tokens.trans, ""))
      letters.trans.np <- unlist(strsplit(tokens.trans.np, ""))

      tokens.equal     <- tokens.orig == tokens.trans
      tokens.equal.np  <- tokens.orig.np == tokens.trans.np
         letters.equal    <- letters.orig == letters.trans
         letters.equal.np <- letters.orig.np == letters.trans.np

      diff.pct.words.all <- 100 * sum(!tokens.equal) / length(tokens.equal)
      diff.pct.words     <- 100 * sum(!tokens.equal.np) / length(tokens.equal.np)
      diff.pct.lett.all  <- 100 * sum(!letters.equal) / length(letters.equal)
      diff.pct.lett      <- 100 * sum(!letters.equal.np) / length(letters.equal.np)

      old.new.comp <- data.frame(
                  token=tokens.trans,
                  tag=txt@TT.res[,"tag"],
                  lemma=txt@TT.res[,"lemma"],
                  lttr=txt@TT.res[,"lttr"],
                  wclass=txt@TT.res[,"wclass"],
                  desc=txt@TT.res[,"desc"],
                  token.orig=tokens.orig,
                  equal=tokens.equal,
                  stringsAsFactors=FALSE)
      results <- new("kRp.txt.trans",
                lang=txt@lang,
                TT.res=old.new.comp,
                diff=list(all.tokens=diff.pct.words.all, words=diff.pct.words, all.chars=diff.pct.lett.all, letters=diff.pct.lett))
      message(paste0("Difference between objects\n    Words: ", round(diff.pct.words, digits=2), "%\n  Letters: ", round(diff.pct.lett, digits=2),"%"))
  }

  return(results)
}
