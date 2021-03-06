# Copyright 2010-2021 Meik Michalke <meik.michalke@hhu.de>
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


#' S4 Class kRp.readability
#'
#' This class is used for objects that are returned by \code{\link[koRpus:readability]{readability}} and its wrapper functions
#' (e.g., \code{Flesch}, \code{FOG} or \code{LIX}).
#'
#' @section Contructor function:
#' Should you need to manually generate objects of this class (which should rarely be the case), the contructor function 
#' \code{kRp_readability(...)} can be used instead of
#' \code{new("kRp.readability", ...)}.
#'
#' @slot lang A character string, naming the language that is assumed for the text in this object.
#' @slot tokens The tokenized and POS-tagged text. See \code{\link[koRpus:kRp.text-class]{kRp.text}} for details.
#' @slot desc Descriptive measures which were computed from the text:
#'    \describe{
#'      \item{\code{sentences}:}{Number of sentences.}
#'      \item{\code{words}:}{Number of words.}
#'      \item{\code{letters}:}{Named vector with total number of letters (\code{"all"}) and possibly several entries called \code{"l<digit>"}, giving the number of words
#'        with \code{<digit>} letters.}
#'      \item{\code{all.chars}:}{Number of all characters, including spaces.}
#'      \item{\code{syllables}:}{Named vector with the number of syllables, simlar to \code{letters}, but entries are called \code{"s<digit>"} (\code{NA} if hyphenation was skipped).}
#'      \item{\code{lttr.distrib}:}{Distribution of letters: Absolute numbers, cumulative sum, inversed cumulative sum, percent, cumulative percent, and inversed cumulative percent.}
#'      \item{\code{syll.distrib}:}{Distribution of syllables (see \code{lttr.distrib}, \code{NA} if hyphenation was skipped).}
#'      \item{\code{syll.uniq.distrib}:}{Distribution of unique syllables (see \code{lttr.distrib}, \code{NA} if hyphenation was skipped).}
#'      \item{\code{punct}:}{Number of punctuation characters.}
#'      \item{\code{conjunctions}:}{Number of conjunctions.}
#'      \item{\code{prepositions}:}{Number of prepositions.}
#'      \item{\code{pronouns}:}{Number of pronouns.}
#'      \item{\code{foreign}:}{Number of foreign words.}
#'      \item{\code{TTR}:}{Type-token ratio.}
#'      \item{\code{avg.sentc.length}:}{Average number of words per sentence.}
#'      \item{\code{avg.word.length}:}{Average number of characters per word.}
#'      \item{\code{avg.syll.word}:}{Average number of syllables per word (\code{NA} if hyphenation was skipped).}
#'      \item{\code{sntc.per.word}:}{Number of sentences per word.}
#'      \item{\code{sntc.per100}:}{Number of sentences per 100 words.}
#'      \item{\code{lett.per100}:}{Number of letters per 100 words.}
#'      \item{\code{syll.per100}:}{Number of syllables per 100 words (\code{NA} if hyphenation was skipped).}
#'      \item{\code{FOG.hard.words}:}{Number of hard words, counted according to FOG (\code{NULL} if measure was not computed).}
#'      \item{\code{Bormuth.NOL}:}{Number of words not on the Bormuth word list (\code{NULL} if measure was not computed).}
#'      \item{\code{Dale.Chall.NOL}:}{Number of words not on the Dale-Chall word list (\code{NULL} if measure was not computed).}
#'      \item{\code{Harris.Jacobson.NOL}:}{Number of words not on the Harris-Jacobson word list (\code{NULL} if measure was not computed).}
#'      \item{\code{Spache.NOL}:}{Number of words not on the Spache word list (\code{NULL} if measure was not computed).}
#'    }
#' @slot hyphen The hyphenated text that was actually analyzed (i.e. without certain word classes, if they were to be removed).
#' @slot param Relevant parameters of the given analysis, as given to the function call. See \code{\link[koRpus:readability]{readability}}
#'    for detailed onformation.
#' @slot ARI The "flavour" of the parameter settings and the calculated value of the ARI level. NA if not calculated.
#' @slot ARI.NRI See "ARI".
#' @slot ARI.simple See "ARI".
#' @slot Bormuth The "flavour" of the parameter settings and the calculated value of Bormuth's Mean Cloze and grade level. NA if not calculated.
#' @slot Coleman The "flavour" of the parameter settings and the calculated value of the four Coleman formulas. NA if not calculated.
#' @slot Coleman.Liau The "flavour" of the parameter settings and the calculated value of the Coleman-Liau index. NA if not calculated.
#' @slot Dale.Chall The "flavour" of the parameter settings and the calculated value of the Dale-Chall Readability Formula. NA if not calculated.
#' @slot Dale.Chall.PSK See "Dale.Chall".
#' @slot Dale.Chall.old See "Dale.Chall".
#' @slot Danielson.Bryan The "flavour" of the parameter settings and the calculated value of the Danielson-Bryan Formula. NA if not calculated.
#' @slot Dickes.Steiwer The "flavour" of the parameter settings and the calculated value of Dickes-Steiwer's shortcut formula. NA if not calculated.
#' @slot DRP The "flavour" of the parameter settings and the calculated value of the Degrees of Reading Power. NA if not calculated.
#' @slot ELF The "flavour" of the parameter settings and the calculated value of the Easy Listening Formula. NA if not calculated.
#' @slot Farr.Jenkins.Paterson The "flavour" of the parameter settings and the calculated value of the Farr-Jenkins-Paterson index. NA if not calculated.
#' @slot Farr.Jenkins.Paterson.PSK See "Farr.Jenkins.Paterson".
#' @slot Flesch The "flavour" of the parameter settings and the calculated value of Flesch Reading Ease. NA if not calculated.
#' @slot Flesch.PSK See "Flesch".
#' @slot Flesch.Brouwer See "Flesch".
#' @slot Flesch.Szigriszt See "Flesch".
#' @slot Flesch.de See "Flesch".
#' @slot Flesch.es See "Flesch".
#' @slot Flesch.fr See "Flesch".
#' @slot Flesch.nl See "Flesch".
#' @slot Flesch.Kincaid The "flavour" of the parameter settings and the calculated value of the Flesch-Kincaid Grade Level. NA if not calculated.
#' @slot FOG The "flavour" of the parameter settings, a list of proper nouns, combined words and verbs that were not counted as hard words
#'     (\code{"dropped"}), the considered number of hard words, and the calculated value of Gunning's FOG index. NA if not calculated.
#' @slot FOG.PSK See "FOG".
#' @slot FOG.NRI See "FOG".
#' @slot FORCAST The "flavour" of the parameter settings and the calculated value of the FORCAST grade level. NA if not calculated.
#' @slot FORCAST.RGL See "FORCAST".
#' @slot Fucks The calculated value of Fucks' Stilcharakteristik. NA if not calculated.
#' @slot Gutierrez The "flavour" of the parameter settings and the calculated value of the Gutierrez index. NA if not calculated.
#' @slot Harris.Jacobson The "flavour" of the parameter settings and the calculated value of the Harris-Jacobson index.
#'    the word list used, all words not found on the list, the percentage of difficult words,  the percentage of long words,
#'    as well as HJ1 to HJ5 for the five indices. NA if not calculated.
#' @slot Linsear.Write The "flavour" of the parameter settings and the calculated value of the Linsear Write index. NA if not calculated.
#' @slot LIX The "flavour" of the parameter settings and the calculated value of the LIX index. NA if not calculated.
#' @slot RIX The "flavour" of the parameter settings and the calculated value of the RIX index. NA if not calculated.
#' @slot SMOG The "flavour" of the parameter settings and the calculated value of the SMOG grade level. NA if not calculated.
#' @slot SMOG.de See "SMOG".
#' @slot SMOG.C See "SMOG".
#' @slot SMOG.simple See "SMOG".
#' @slot Spache The "flavour" of the parameter settings and the calculated value of the Spache formula. NA if not calculated.
#' @slot Spache.old See "Spache".
#' @slot Strain The "flavour" of the parameter settings and the calculated value of the Strain index. NA if not calculated.
#' @slot Traenkle.Bailer The "flavour" of the parameter settings, percentages of prepositions and conjunctions, and the calculated values of both Tr\"ankle-Bailer formulae. NA if not calculated.
#' @slot TRI The calculated value of Kuntzsch' Text-Redundanz-Index. NA if not calculated.
#' @slot Tuldava The calculated value of the Tuldava text difficulty formula. NA if not calculated.
#' @slot Wheeler.Smith The "flavour" of the parameter settings and the calculated value of the Wheeler-Smith index. NA if not calculated.
#' @slot Wheeler.Smith.de See "Wheeler.Smith"
#' @slot Wiener.STF The "flavour" of the parameter settings and the calculated value of the Wiener Sachtextformel. NA if not calculated.
#' @name kRp.readability,-class
#' @aliases kRp.readability-class
#' @import methods
#' @keywords classes
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @export kRp_readability
#' @exportClass kRp.readability
#' @rdname kRp.readability-class

#' @include 01_class_01_kRp.text.R

kRp_readability <- setClass("kRp.readability",
    representation=representation(
      hyphen="kRp.hyphen",
      param="list",
      ARI="list",
      ARI.NRI="list",
      ARI.simple="list",
      Bormuth="list",
      Coleman="list",
      Coleman.Liau="list",
      Dale.Chall="list",
      Dale.Chall.PSK="list",
      Dale.Chall.old="list",
      Danielson.Bryan="list",
      Dickes.Steiwer="list",
      DRP="list",
      ELF="list",
      Flesch="list",
      Flesch.PSK="list",
      Flesch.Brouwer="list",
      Flesch.Szigriszt="list",
      Flesch.de="list",
      Flesch.es="list",
      Flesch.fr="list",
      Flesch.nl="list",
      Flesch.Kincaid="list",
      Farr.Jenkins.Paterson="list",
      Farr.Jenkins.Paterson.PSK="list",
      FOG="list",
      FOG.PSK="list",
      FOG.NRI="list",
      FORCAST="list",
      FORCAST.RGL="list",
      Fucks="list",
      Gutierrez="list",
      Harris.Jacobson="list",
      Linsear.Write="list",
      LIX="list",
      RIX="list",
      SMOG="list",
      SMOG.de="list",
      SMOG.C="list",
      SMOG.simple="list",
      Spache="list",
      Spache.old="list",
      Strain="list",
      Traenkle.Bailer="list",
      TRI="list",
      Tuldava="list",
      Wheeler.Smith="list",
      Wheeler.Smith.de="list",
      Wiener.STF="list"
    ),
    contains=c("kRp.text")
)

setMethod("initialize", "kRp.readability",
  function(
    .Object,
    lang=character(),
    tokens=init.kRp.text.df(),
    desc=list(NA),
    hyphen=new("kRp.hyphen"),
    param=list(NA),
    ARI=list(NA),
    ARI.NRI=list(NA),
    ARI.simple=list(NA),
    Bormuth=list(NA),
    Coleman=list(NA),
    Coleman.Liau=list(NA),
    Dale.Chall=list(NA),
    Dale.Chall.PSK=list(NA),
    Dale.Chall.old=list(NA),
    Danielson.Bryan=list(NA),
    Dickes.Steiwer=list(NA),
    DRP=list(NA),
    ELF=list(NA),
    Flesch=list(NA),
    Flesch.PSK=list(NA),
    Flesch.Brouwer=list(NA),
    Flesch.Szigriszt=list(NA),
    Flesch.de=list(NA),
    Flesch.es=list(NA),
    Flesch.fr=list(NA),
    Flesch.nl=list(NA),
    Flesch.Kincaid=list(NA),
    Farr.Jenkins.Paterson=list(NA),
    Farr.Jenkins.Paterson.PSK=list(NA),
    FOG=list(NA),
    FOG.PSK=list(NA),
    FOG.NRI=list(NA),
    FORCAST=list(NA),
    FORCAST.RGL=list(NA),
    Fucks=list(NA),
    Gutierrez=list(NA),
    Harris.Jacobson=list(NA),
    Linsear.Write=list(NA),
    LIX=list(NA),
    RIX=list(NA),
    SMOG=list(NA),
    SMOG.de=list(NA),
    SMOG.C=list(NA),
    SMOG.simple=list(NA),
    Spache=list(NA),
    Spache.old=list(NA),
    Strain=list(NA),
    Traenkle.Bailer=list(NA),
    TRI=list(NA),
    Tuldava=list(NA),
    Wheeler.Smith=list(NA),
    Wheeler.Smith.de=list(NA),
    Wiener.STF=list(NA)
  ){
    slot(.Object, "lang") <- lang
    slot(.Object, "tokens") <- tokens
    slot(.Object, "desc") <- desc
    slot(.Object, "hyphen") <- hyphen
    slot(.Object, "param") <- param
    slot(.Object, "ARI") <- ARI
    slot(.Object, "ARI.NRI") <- ARI.NRI
    slot(.Object, "ARI.simple") <- ARI.simple
    slot(.Object, "Bormuth") <- Bormuth
    slot(.Object, "Coleman") <- Coleman
    slot(.Object, "Coleman.Liau") <- Coleman.Liau
    slot(.Object, "Dale.Chall") <- Dale.Chall
    slot(.Object, "Dale.Chall.PSK") <- Dale.Chall.PSK
    slot(.Object, "Dale.Chall.old") <- Dale.Chall.old
    slot(.Object, "Danielson.Bryan") <- Danielson.Bryan
    slot(.Object, "Dickes.Steiwer") <- Dickes.Steiwer
    slot(.Object, "DRP") <- DRP
    slot(.Object, "ELF") <- ELF
    slot(.Object, "Flesch") <- Flesch
    slot(.Object, "Flesch.PSK") <- Flesch.PSK
    slot(.Object, "Flesch.Brouwer") <- Flesch.Brouwer
    slot(.Object, "Flesch.Szigriszt") <- Flesch.Szigriszt
    slot(.Object, "Flesch.de") <- Flesch.de
    slot(.Object, "Flesch.es") <- Flesch.es
    slot(.Object, "Flesch.fr") <- Flesch.fr
    slot(.Object, "Flesch.nl") <- Flesch.nl
    slot(.Object, "Flesch.Kincaid") <- Flesch.Kincaid
    slot(.Object, "Farr.Jenkins.Paterson") <- Farr.Jenkins.Paterson
    slot(.Object, "Farr.Jenkins.Paterson.PSK") <- Farr.Jenkins.Paterson.PSK
    slot(.Object, "FOG") <- FOG
    slot(.Object, "FOG.PSK") <- FOG.PSK
    slot(.Object, "FOG.NRI") <- FOG.NRI
    slot(.Object, "FORCAST") <- FORCAST
    slot(.Object, "FORCAST.RGL") <- FORCAST.RGL
    slot(.Object, "Fucks") <- Fucks
    slot(.Object, "Gutierrez") <- Gutierrez
    slot(.Object, "Harris.Jacobson") <- Harris.Jacobson
    slot(.Object, "Linsear.Write") <- Linsear.Write
    slot(.Object, "LIX") <- LIX
    slot(.Object, "RIX") <- RIX
    slot(.Object, "SMOG") <- SMOG
    slot(.Object, "SMOG.de") <- SMOG.de
    slot(.Object, "SMOG.C") <- SMOG.C
    slot(.Object, "SMOG.simple") <- SMOG.simple
    slot(.Object, "Spache") <- Spache
    slot(.Object, "Spache.old") <- Spache.old
    slot(.Object, "Strain") <- Strain
    slot(.Object, "Traenkle.Bailer") <- Traenkle.Bailer
    slot(.Object, "TRI") <- TRI
    slot(.Object, "Tuldava") <- Tuldava
    slot(.Object, "Wheeler.Smith") <- Wheeler.Smith
    slot(.Object, "Wheeler.Smith.de") <- Wheeler.Smith.de
    slot(.Object, "Wiener.STF") <- Wiener.STF
    validObject(.Object)
    return(.Object)
  }
)

setValidity("kRp.readability", function(object){
  ## TODO: this can probably be improved if necessary
  validate_df(
    df=slot(object, "tokens"),
    valid_cols=valid.tokens.kRp.text,
    strict=FALSE,
    warn_only=FALSE,
    name="tokens"
  )

  if(!is.character(slot(object, "lang"))){
    stop(simpleError("Invalid object: Slot \"lang\" must be of class character!"))
  } else {}

  return(TRUE)
})
