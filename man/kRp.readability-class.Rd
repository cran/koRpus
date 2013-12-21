\name{kRp.readability-class}
\alias{kRp.readability-class}
\alias{kRp.readability,-class
-class}
\alias{kRp.readability,-class}
\title{S4 class kRp.readability}
\description{Class kRp.readability}
\details{This class is used for objects that are returned by \code{\link[koRpus:readability]{readability}} and its wrapper functions
(e.g., \code{Flesch}, \code{FOG} or \code{LIX}).}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{lang}:}{(\code{\link{character}}) A character string,
      naming the language that is assumed for the text in this object.}\item{\code{TT.res}:}{(\code{\link{data.frame}}) The tokenized and POS-tagged text. See \code{\link[koRpus]{kRp.tagged-class}} for details.}\item{\code{desc}:}{(\code{\link{list}}) Descriptive measures which were computed of the text:
\describe{
\item{\code{sentences}:}{Number of sentences.}
\item{\code{words}:}{Number of words.}
\item{\code{letters}:}{Named vector with total number of letters (\code{"all"}) and possibly several entries called \code{"l<digit>"},
      giving the number of words with \code{<digit>} letters.}
\item{\code{all.chars}:}{Number of all characters, including spaces.}
\item{\code{syllables}:}{Named vector with the number of syllables,
      simlar to \code{letters},
      but entries are called \code{"s<digit>"} (\code{NA} if hyphenation was skipped).}
\item{\code{lttr.distrib}:}{Distribution of letters: Absolute numbers, cumulative sum,
      inversed cumulative sum, percent, cumulative percent,
      and inversed cumulative percent.}
\item{\code{syll.distrib}:}{Distribution of syllables (see \code{lttr.distrib},
      \code{NA} if hyphenation was skipped).}
\item{\code{syll.uniq.distrib}:}{Distribution of unique syllables (see \code{lttr.distrib},
      \code{NA} if hyphenation was skipped).}
\item{\code{punct}:}{Number of punctuation characters.}
\item{\code{conjunctions}:}{Number of conjunctions.}
\item{\code{prepositions}:}{Number of prepositions.}
\item{\code{pronouns}:}{Number of pronouns.}
\item{\code{foreign}:}{Number of foreign words.}
\item{\code{TTR}:}{Type-token ratio.}
\item{\code{avg.sentc.length}:}{Average number of words per sentence.}
\item{\code{avg.word.length}:}{Average number of characters per word.}
\item{\code{avg.syll.word}:}{Average number of syllables per word (\code{NA} if hyphenation was skipped).}
\item{\code{sntc.per.word}:}{Number of sentences per word.}
\item{\code{sntc.per100}:}{Number of sentences per 100 words.}
\item{\code{lett.per100}:}{Number of letters per 100 words.}
\item{\code{syll.per100}:}{Number of syllables per 100 words (\code{NA} if hyphenation was skipped).}
\item{\code{FOG.hard.words}:}{Number of hard words,
      counted according to FOG (\code{NULL} if measure was not computed).}
\item{\code{Bormuth.NOL}:}{Number of words not on the Bormuth word list (\code{NULL} if measure was not computed).}
\item{\code{Dale.Chall.NOL}:}{Number of words not on the Dale-Chall word list (\code{NULL} if measure was not computed).}
\item{\code{Harris.Jacobson.NOL}:}{Number of words not on the Harris-Jacobson word list (\code{NULL} if measure was not computed).}
\item{\code{Spache.NOL}:}{Number of words not on the Spache word list (\code{NULL} if measure was not computed).}
}}
\item{\code{hyphen}:}{(\code{\link[koRpus]{kRp.hyphen-class}}) The hyphenated text that was actually analyzed (i.e. without certain word classes,
      if they were to be removed).}
\item{\code{param}:}{(\code{\link{list}}) Relevant parameters of the given analysis,
      as given to the function call. See \code{\link[koRpus:readability]{readability}} for detailed onformation.}
\item{\code{Flesch}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of Flesch Reading Ease. NA if not calculated.}
\item{\code{Flesch.PSK}:}{(\code{\link{list}}) See "Flesch".}
\item{\code{Flesch.de}:}{(\code{\link{list}}) See "Flesch".}
\item{\code{Flesch.es}:}{(\code{\link{list}}) See "Flesch".}
\item{\code{Flesch.fr}:}{(\code{\link{list}}) See "Flesch".}
\item{\code{Flesch.nl}:}{(\code{\link{list}}) See "Flesch".}
\item{\code{Flesch.Kincaid}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Flesch-Kincaid Grade Level. NA if not calculated.}
\item{\code{Farr.Jenkins.Paterson}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Farr-Jenkins-Paterson index. NA if not calculated.}
\item{\code{Farr.Jenkins.Paterson.PSK}:}{(\code{\link{list}}) See "Farr.Jenkins.Paterson".}\item{\code{FOG}:}{(\code{\link{list}}) The "flavour" of the parameter settings,
      a list of proper nouns,
      combined words and verbs that were not counted as hard words (\code{"dropped"}),
      the considered number of hard words,
      and the calculated value of Gunning's FOG index. NA if not calculated.}
\item{\code{FOG.PSK}:}{(\code{\link{list}}) See "FOG".}
\item{\code{FOG.NRI}:}{(\code{\link{list}}) See "FOG".}
\item{\code{Coleman.Liau}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Coleman-Liau index. NA if not calculated.}
\item{\code{SMOG}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the SMOG grade level. NA if not calculated.}
\item{\code{SMOG.de}:}{(\code{\link{list}}) See "SMOG".}
\item{\code{SMOG.C}:}{(\code{\link{list}}) See "SMOG".}
\item{\code{SMOG.simple}:}{(\code{\link{list}}) See "SMOG".}
\item{\code{LIX}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the LIX index. NA if not calculated.}
\item{\code{RIX}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the RIX index. NA if not calculated.}
\item{\code{Wheeler.Smith}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Wheeler-Smith index. NA if not calculated.}
\item{\code{Wheeler.Smith.de}:}{(\code{\link{list}}) See "Wheeler.Smith"}
\item{\code{FORCAST}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the FORCAST grade level. NA if not calculated.}
\item{\code{FORCAST.RGL}:}{(\code{\link{list}}) See "FORCAST".}
\item{\code{ARI}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the ARI level. NA if not calculated.}
\item{\code{ARI.NRI}:}{(\code{\link{list}}) See "ARI".}
\item{\code{ARI.simple}:}{(\code{\link{list}}) See "ARI".}
\item{\code{Linsear.Write}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Linsear Write index. NA if not calculated.}
\item{\code{Strain}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Strain index. NA if not calculated.}
\item{\code{Wiener.STF}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Wiener Sachtextformel. NA if not calculated.}
\item{\code{Coleman}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the four Coleman formulas. NA if not calculated.}
\item{\code{Dale.Chall}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Dale-Chall Readability Formula. NA if not calculated.}
\item{\code{Dale.Chall.PSK}:}{(\code{\link{list}}) See "Dale.Chall".}\item{\code{Dale.Chall.old}:}{(\code{\link{list}}) See "Dale.Chall".}
\item{\code{Bormuth}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of Bormuth's Mean Cloze and grade level. NA if not calculated.}
\item{\code{DRP}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Degrees of Reading Power. NA if not calculated.}
\item{\code{Spache}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Spache formula. NA if not calculated.}
\item{\code{Spache.old}:}{(\code{\link{list}}) See "Spache".}
\item{\code{ELF}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Easy Listening Formula. NA if not calculated.}
\item{\code{Dickes.Steiwer}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of Dickes-Steiwer's shortcut formula. NA if not calculated.}
\item{\code{TRI}:}{(\code{\link{list}}) The calculated value of Kuntzsch' Text-Redundanz-Index. NA if not calculated.}
\item{\code{Fucks}:}{(\code{\link{list}}) The calculated value of Fucks' Stilcharakteristik. NA if not calculated.}
\item{\code{Danielson.Bryan}:}{(\code{\link{list}}) The "flavour" of the parameter settings and the calculated value of the Danielson-Bryan Formula. NA if not calculated.}}}
