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


#' Analyze lexical diversity
#' 
#' This function analyzes the lexical diversity/complexity of a text corpus.
#'
#' \code{lex.div} calculates a variety of proposed indices for lexical diversity. In the following formulae, \eqn{N} refers to
#' the total number of tokens, and \eqn{V} to the number of types:
#' \describe{
#'  \item{\code{"TTR"}:}{The ordinary \emph{Type-Token Ratio}: \deqn{TTR = \frac{V}{N}}{TTR =  V / N}
#'    Wrapper function: \code{\link[koRpus:TTR]{TTR}}}
#'  \item{\code{"MSTTR"}:}{For the \emph{Mean Segmental Type-Token Ratio} (sometimes referred to as \emph{Split TTR}) tokens are split up into 
#'    segments of the given size, TTR for each segment is calculated and the mean of these values returned. Tokens at the end which do 
#'    not make a full segment are ignored. The number of dropped tokens is reported.
#'
#'    Wrapper function: \code{\link[koRpus:MSTTR]{MSTTR}}}
#'  \item{\code{"MATTR"}:}{The \emph{Moving-Average Type-Token Ratio} (Covington & McFall, 2010) calculates TTRs for a defined number of tokens
#'    (called the "window"), starting at the beginning of the text and moving this window over the text, until the last token is reached.
#'    The mean of these TTRs is the MATTR.
#'
#'    Wrapper function: \code{\link[koRpus:MATTR]{MATTR}}}
#'  \item{\code{"C"}:}{Herdan's \emph{C} (Herdan, 1960, as cited in Tweedie & Baayen, 1998; sometimes referred to as \emph{LogTTR}): \deqn{C = \frac{\lg{V}}{\lg{N}}}{C = lg(V) / lg(N)}}
#'
#'    Wrapper function: \code{\link[koRpus:C.ld]{C.ld}}
#'  \item{\code{"R"}:}{Guiraud's \emph{Root TTR} (Guiraud, 1954, as cited in Tweedie & Baayen, 1998): \deqn{R = \frac{V}{\sqrt{N}}}{R = V / sqrt(N)}}
#'
#'    Wrapper function: \code{\link[koRpus:R.ld]{R.ld}}
#'  \item{\code{"CTTR"}:}{Carroll's \emph{Corrected TTR}: \deqn{CTTR = \frac{V}{\sqrt{2N}}}{CTTR = V / sqrt(2N)}}
#'
#'    Wrapper function: \code{\link[koRpus:CTTR]{CTTR}}
#'  \item{\code{"U"}:}{Dugast's \emph{Uber Index}  (Dugast, 1978, as cited in Tweedie & Baayen, 1998): \deqn{U = \frac{(\lg{N})^2}{\lg{N} - \lg{V}}}{U = lg(N)^2 / lg(N) - lg(V)}}
#'
#'    Wrapper function: \code{\link[koRpus:U.ld]{U.ld}}
#'  \item{\code{"S"}:}{Summer's index: \deqn{S = \frac{\lg{\lg{V}}}{\lg{\lg{N}}}}{S = lg(lg(V)) / lg(lg(N))}}
#'
#'    Wrapper function: \code{\link[koRpus:S.ld]{S.ld}}
#'  \item{\code{"K"}:}{Yule's \emph{K}  (Yule, 1944, as cited in Tweedie & Baayen, 1998) is calculated by: \deqn{K = 10^4 \times \frac{(\sum_{X=1}^{X}{{f_X}X^2}) - N}{N^2}}{K = 10^4 * (sum(fX*X^2) - N) / N^2}
#'    where \eqn{N} is the number of tokens, \eqn{X} is a vector with the frequencies of each type, and \eqn{f_X}{fX} is
#'    the frequencies for each X.
#'
#'    Wrapper function: \code{\link[koRpus:K.ld]{K.ld}}}
#'  \item{\code{"Maas"}:}{Maas' indices (\eqn{a}, \eqn{\lg{V_0}} & \eqn{\lg{}_{e}{V_0}}): \deqn{a^2 = \frac{\lg{N} - \lg{V}}{\lg{N}^2}}{a^2 = lg(N) - lg(V) / lg(N)^2}
#'  \deqn{\lg{V_0} = \frac{\lg{V}}{\sqrt{1 - \frac{\lg{V}}{\lg{N}}^2}}}{lg(V0) = lg(V) / sqrt(1 - (lg(V) / lg(N)^2))}
#'    Earlier versions (\code{koRpus} < 0.04-12) reported \eqn{a^2}, and not \eqn{a}. The measure was derived from a formula by M\"uller (1969, as cited in Maas, 1972).
#'    \eqn{\lg{}_{e}{V_0}} is equivalent to \eqn{\lg{V_0}}, only with \eqn{e} as the base for the logarithms. Also calculated are \eqn{a}, \eqn{\lg{V_0}} (both not the same
#'    as before) and \eqn{V'} as measures of relative vocabulary growth while the text progresses. To calculate these measures, the first half of the text and the full text
#'    will be examined (see Maas, 1972, p. 67 ff. for details).
#'
#'    Wrapper function: \code{\link[koRpus:maas]{maas}}}
#'  \item{\code{"MTLD"}:}{For the \emph{Measure of Textual Lexical Diversity} (McCarthy & Jarvis, 2010) so called factors are counted. Each factor is a subsequent stream of 
#'    tokens which ends (and is then counted as a full factor) when the TTR value falls below the given factor size. The value of
#'    remaining partial factors is estimated by the ratio of their current TTR to the factor size threshold. The MTLD is the total number 
#'    of tokens divided by the number of factors. The procedure is done twice, both forward and backward for all tokens, and the mean of 
#'    both calculations is the final MTLD result.
#'
#'    Wrapper function: \code{\link[koRpus:MTLD]{MTLD}}}
#'  \item{\code{"MTLD-MA"}:}{The \emph{Moving-Average Measure of Textual Lexical Diversity} (Jarvis, no year) combines factor counting and a moving
#'    window similar to MATTR: After each full factor the the next one is calculated from one token after the last starting point. This is repeated
#'    until the end of text is reached for the first time. The average of all full factor lengths is the final MTLD-MA result. Factors below the
#'    \code{min.tokens} threshold are dropped.
#'
#'    Wrapper function: \code{\link[koRpus:MTLD]{MTLD}}}
#'  \item{\code{"HD-D"}:}{The \emph{HD-D} value can be interpreted as the idealized version of \emph{vocd-D} (see McCarthy & Jarvis, 2007). For each type,
#'    the probability is computed (using the hypergeometric distribution) of drawing it at least one time when drawing randomly a certain
#'    number of tokens from the text -- 42 by default. The sum of these probabilities make up the HD-D value. The sum of probabilities relative to
#'    the drawn sample size (ATTR) is also reported.
#'
#'    Wrapper function: \code{\link[koRpus:HDD]{HDD}}}
#' }
#'
#' By default, if the text has to be tagged yet, the language definition is queried by calling \code{get.kRp.env(lang=TRUE)} 
#' internally.
#' Or, if \code{txt} has already been tagged, by default the language definition of that tagged object is read
#' and used. Set \code{force.lang=get.kRp.env(lang=TRUE)} or to any other valid value, if you want to forcibly overwrite this
#' default behaviour, and only then. See \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}} for all supported languages.
#'
#' @param txt An object of either class \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.txt.freq-class}},
#'    \code{\link[koRpus]{kRp.analysis-class}} or  \code{\link[koRpus]{kRp.txt.trans-class}}, containing the tagged text to be analyzed.
#' @param segment An integer value for MSTTR, defining how many tokens should form one segment.
#' @param factor.size A real number between 0 and 1, defining the MTLD factor size.
#' @param min.tokens An integer value, how many tokens a full factor must at least have to be considered for the MTLD-MA result.
#' @param rand.sample An integer value, how many tokens should be assumed to be drawn for calculating HD-D.
#' @param window An integer value for MATTR, defining how many tokens the moving window should include.
#' @param case.sens Logical, whether types should be counted case sensitive.
#' @param lemmatize Logical, whether analysis should be carried out on the lemmatized tokens rather than all running word forms.
#' @param detailed Logical, whether full details of the analysis should be calculated. This currently affects MTLD and MTLD-MA, defining
#'    if all factors should be kept in the object. This slows down calculations considerably.
#' @param measure A character vector defining the measures which should be calculated. Valid elements are "TTR", "MSTTR", "MATTR", "C", "R", 
#'    "CTTR", "U", "S", "K", "Maas", "HD-D", "MTLD" and "MTLD-MA".
#' @param char A character vector defining whether data for plotting characteristic curves should be calculated. Valid elements are 
#'    "TTR","MATTR", "C", "R", "CTTR", "U", "S", "K", "Maas", "HD-D", "MTLD" and "MTLD-MA".
#' @param char.steps An integer value defining the stepwidth for characteristic curves, in tokens.
#' @param log.base A numeric value defining the base of the logarithm. See \code{\link[base:log]{log}} for details.
#' @param force.lang A character string defining the language to be assumed for the text, by force. See details.
#' @param keep.tokens Logical. If \code{TRUE} all raw tokens and types will be preserved in the resulting object, in a slot called 
#'    \code{tt}. For the types, also their frequency in the analyzed text will be listed.
#' @param corp.rm.class A character vector with word classes which should be dropped. The default value
#'    \code{"nonpunct"} has special meaning and will cause the result of
#'    \code{kRp.POS.tags(lang, c("punct","sentc"), list.classes=TRUE)} to be used.
#' @param corp.rm.tag A character vector with POS tags which should be dropped.
#' @param quiet Logical. If \code{FALSE}, short status messages will be shown.
#'    \code{TRUE} will also suppress all potential warnings regarding the validation status of measures.
#' @return An object of class \code{\link[koRpus]{kRp.TTR-class}}.
# @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @keywords LD
#' @seealso \code{\link[koRpus:kRp.POS.tags]{kRp.POS.tags}},
#'    \code{\link[koRpus]{kRp.tagged-class}}, \code{\link[koRpus]{kRp.TTR-class}}
#' @references
#'    Covington, M.A. & McFall, J.D. (2010). Cutting the Gordian Knot: The Moving-Average Type-Token Ratio (MATTR). 
#'      \emph{Journal of Quantitative Linguistics}, 17(2), 94--100.
#'
#'    Maas, H.-D., (1972). \"Uber den Zusammenhang zwischen Wortschatzumfang und L\"ange eines Textes. \emph{Zeitschrift f\"ur 
#'      Literaturwissenschaft und Linguistik}, 2(8), 73--96.
#'
#'   McCarthy, P.M. & Jarvis, S. (2007). vocd: A theoretical and empirical evaluation. \emph{Language Testing}, 24(4), 459--488.
#'
#'    McCarthy, P.M. & Jarvis, S. (2010). MTLD, vocd-D, and HD-D: A validation study of sophisticated approaces to lexical diversity 
#'      assessment. \emph{Behaviour Research Methods}, 42(2), 381--392.
#'
#'    Tweedie. F.J. & Baayen, R.H. (1998). How Variable May a Constant Be? Measures of Lexical Richness in Perspective.
#'     \emph{Computers and the Humanities}, 32(5), 323--352.
#' @export
#' @examples
#' \dontrun{
#' lex.div(tagged.text)
#' }
lex.div <- function(txt, segment=100, factor.size=0.72, min.tokens=9, rand.sample=42, window=100,
    case.sens=FALSE, lemmatize=FALSE, detailed=FALSE,
    measure=c("TTR","MSTTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char=c("TTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"),
    char.steps=5, log.base=10,
    force.lang=NULL,
    keep.tokens=FALSE,
    corp.rm.class="nonpunct",
    corp.rm.tag=c(), quiet=FALSE){

## TODO:
# - Tuldava (1997): T = LogLog(tokens)/LogLog((tokens/types)+A)^5
#   (was ist A?)
# - Zipf's Z
# - check MTLD charactersitics -- can't this be simplified to save a lot of time?

  ## TODO: validation
  # the following implementations have already been checked against various tools
  # to validate the correctness of calculation. this doesn't mean they always came to identical
  # results at once, since the accuracy of input data (like number of tokens) might vary.
  # but if these differences were manually corrected, the results were similar/identical:
  # - C                    [AYG]
  # - CTTR                 [AYG]
  # - HD-D                 [JMC]
  # - Maas                 [MAS]
  # - MSTTR                [AYG]
  # - MTLD                 [JMC]
  # - R                    [AYG]
  # - TTR                  [TAL]
  # - U                    [AYG]
  # - MTLD-MA              [JMC]
  # 
  # these measures produce plausible results, but need checking:
  # - MATTR
  # - S
  # - K
  # 
  # tools used:
  # AYG: http://aihaiyang.com/synlex/lexical
  # TAL: http://www.textalyser.net
  # 
  # other:
  # JMC: re-calculations by jarvis & mccarthy (thanks!!!)
  # MAS: example data in the original paper by Maas

  if(!is.numeric(factor.size) | factor.size > 1 | factor.size < 0){
    stop(simpleError(paste("Invalid factor size value (must be 0 < factor.size < 1):",factor.size)))
  } else {}
  if(!is.numeric(segment) | segment < 1){
    stop(simpleError(paste("Invalid segment value (must be > 0):",factor.size)))
  } else {}
  # check for optional measures
  if(!any(measure %in% c("TTR","MSTTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA")) &
    !any(char %in% c("TTR","MATTR","C","R","CTTR","U","S","K","Maas","HD-D","MTLD","MTLD-MA"))){
      stop(simpleError(paste("You didn't specify at least one valid measure or characteristic!")))
  } else {}

  # get class kRp.tagged from txt object
  # the internal function tag.kRp.txt() will return the object unchanged if it
  # is already tagged, so it's safe to call it with the lang set here
  tagged.text <- tag.kRp.txt(txt, lang=force.lang, objects.only=TRUE)
  # set the language definition
  lang <- language.setting(tagged.text, force.lang)
  if(!isTRUE(quiet)){
    cat("Language: \"",lang,"\"\n", sep="")
  } else {}

  if(identical(corp.rm.class, "nonpunct")){
    corp.rm.class <- kRp.POS.tags(lang, tags=c("punct","sentc"), list.classes=TRUE)
  } else {}

  # calling internal function tagged.txt.rm.classes()
  txt.all.clean <- tagged.txt.rm.classes(tagged.text@TT.res,
      # "lemma" only affects the results if "as.vector=TRUE"
      # so lemmatizing will be done by type.freq() below
      lemma=FALSE, lang=lang,
      corp.rm.class=corp.rm.class,
      corp.rm.tag=corp.rm.tag,
      as.vector=FALSE)
  if(isTRUE(lemmatize)){
    txt.all.tokens <- txt.all.clean[,"lemma"]
  } else {
    txt.all.tokens <- txt.all.clean[,"token"]
  }
  txt.type.freq <- type.freq(txt.all.clean, case.sens=case.sens, lemma=FALSE)
  txt.lemma.freq <- type.freq(txt.all.clean, case.sens=case.sens, lemma=TRUE, fail.if.no.lemmas=lemmatize)
  txt.all.types <- txt.type.freq[,"type"]
  txt.all.lemmas <- txt.lemma.freq[,"type"]
  if(!isTRUE(case.sens)){
    txt.all.tokens <- tolower(txt.all.tokens)
  } else {}
  num.all.tokens <- length(txt.all.tokens)
  num.all.types <- length(txt.all.types)
  num.all.lemmas <- length(txt.all.lemmas)
  num.type.or.lemma <- ifelse(isTRUE(lemmatize), num.all.lemmas, num.all.types)
  # some sanity checks
  if(num.all.tokens < 100){
    warning("Text is relatively short (<100 tokens), results are probably not reliable!", call.=FALSE)
  } else {}

  if(!is.null(char)){
    if(num.all.tokens < char.steps){
      warning("Text is shorter than step size, adjusting \"char.steps\" to 1!", call.=FALSE)
      char.steps <- 1
    } else {}
    # global value for steps if char=c(something)
    num.all.steps <- num.all.tokens %/% char.steps
  } else {}


  # initialize result object
  lex.div.results <- new("kRp.TTR")


  ###################################
  ## diversity measures start here ##
  ###################################


  ## calculate TTR
  if("TTR" %in% measure){
    lex.div.results@TTR <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="TTR")
  } else {}

  ## calculate Herdan's C: log(types) / log(tokens)
  if("C" %in% measure){
    lex.div.results@C.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="C", log.base=log.base)
  } else {}

  ## calculate Guiraud's R: types / sqrt(tokens)
  if("R" %in% measure){
    lex.div.results@R.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="R")
  } else {}

  ## calculate Carroll's CTTR: types / 2*sqrt(tokens)
  if("CTTR" %in% measure){
    lex.div.results@CTTR <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="CTTR")
  } else {}

  ## calculate Uber Index U: (log(tokens))^2 / (log(tokens) - log(types)) 
  if("U" %in% measure){
    lex.div.results@U.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="U", log.base=log.base)
  } else {}

  ## calculate Summer's S: LogLog(types) / LogLog(tokens)
  if("S" %in% measure){
    lex.div.results@S.ld <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="S", log.base=log.base)
  } else {}

  ## calculate Maas' a and lgV0 indices
  if("Maas" %in% measure){
    lex.div.results@Maas <- ttr.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, type="Maas", log.base=log.base)
    lex.div.results@lgV0 <- lgV0.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, x=0)
    lex.div.results@lgeV0 <- lgV0.calc(num.tokens=num.all.tokens, num.types=num.type.or.lemma, x=0, log.base=exp(1))
    # calculate relative lexical growth, using first half of the text and full text
    Maas.txt.half <- txt.all.tokens[1:(num.all.tokens %/% 2)]
    Maas.num.tokens.half <- length(Maas.txt.half)
    Maas.num.types.half <- length(unique(Maas.txt.half))
    lex.div.results@Maas.grw <- lex.growth(N1=Maas.num.tokens.half, V1=Maas.num.types.half, N2=num.all.tokens, V2=num.type.or.lemma)
  } else {}

  ## calculate MSTTR
  if("MSTTR" %in% measure){
    lex.div.results@MSTTR <- MSTTR.calc(txt.tokens=txt.all.tokens, segment=segment, num.tokens=num.all.tokens)
  } else {}

  ## calculate MATTR
  # needed also for characteristics
  if(any(c("MATTR", "MATTR.char") %in% measure)){
    lex.div.results@MATTR <- MATTR.calc(txt.tokens=txt.all.tokens, window=window, num.tokens=num.all.tokens)
  } else {}

  ## Yule's K, frequency correction
  if("K" %in% measure){
    lex.div.results@K.ld <- k.calc(txt.all.tokens)
  } else {}

  ## calculate HD-D
  if("HD-D" %in% measure){
    lex.div.results@HDD <- hdd.calc(txt.all.tokens, drawn=rand.sample)
  } else {}

  ## calculate MTLD
  if("MTLD" %in% measure){
    lex.div.results@MTLD <- MTLD.calc(txt.all.tokens, factor.size=factor.size, num.tokens=num.all.tokens, detailed=detailed)
  } else {}

  ## calculate MTLD-MA
  if("MTLD-MA" %in% measure){
    # characteristics need detailed results, so discard another setting if present to speed up things
    # the alternative would be to calculate this twice, so it's a no-brainer
    # to comply with user preferences, we'll drop the detailed stuff again, see MTLDMA.char section!
    if("MTLD-MA" %in% char && !isTRUE(detailed)){
      detailed.mtldma <- TRUE
    } else {
      detailed.mtldma <- detailed
    }
    lex.div.results@MTLDMA <- MTLDMA.calc(txt.all.tokens, factor.size=factor.size, num.tokens=num.all.tokens, min.tokens=min.tokens,
      detailed=detailed.mtldma, quiet=quiet)
  } else {}

  ## calculate TTR, C, R, CTTR, U, S and Maas characteristics
  # set up the base function
  ttr.calc.chars <- function(txt.tokens, type="TTR", log.base=log.base){
    if(!isTRUE(quiet)){
      message(paste0(type, ".char: Calculate ",type," values"))
      # give some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    char.results <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          char.temp <- c(token=curr.token, value=ttr.calc(txt.tokens=txt.all.tokens[1:curr.token], type=type, log.base=log.base))
          return(char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
    return(char.results)
  }

  # do the actual calculations
  if("TTR" %in% char){
    lex.div.results@TTR.char <- ttr.calc.chars(txt.all.tokens, type="TTR")
  } else {}

  if("C" %in% char){
    lex.div.results@C.char <- ttr.calc.chars(txt.all.tokens, type="C", log.base=log.base)
  } else {}

  if("R" %in% char){
    lex.div.results@R.char <- ttr.calc.chars(txt.all.tokens, type="R")
  } else {}

  if("CTTR" %in% char){
    lex.div.results@CTTR.char <- ttr.calc.chars(txt.all.tokens, type="CTTR")
  } else {}

  if("U" %in% char){
    lex.div.results@U.char <- ttr.calc.chars(txt.all.tokens, type="U", log.base=log.base)
  } else {}

  if("S" %in% char){
    lex.div.results@S.char <- ttr.calc.chars(txt.all.tokens, type="S", log.base=log.base)
  } else {}

  if("MATTR" %in% char && num.all.tokens > window){
    # mattr.all.TTR should be available in lex.div.results@MATTR$TTR.win
    # characteristics are just the progressing mean of these TTRs
    mattr.all.TTR <- slot(lex.div.results, "MATTR")$TTR.win
    mattr.num.TTRs <- length(mattr.all.TTR)
    mattr.num.all.steps <- mattr.num.TTRs %/% char.steps
    lex.div.results@MATTR.char <- t(sapply(1:mattr.num.all.steps, function(x){
        mattr.curr.token <- x * char.steps
        mattr.char.temp <- c(token=mattr.curr.token + window, value=mean(mattr.all.TTR[1:mattr.curr.token]))
        return(mattr.char.temp)
      }))
  } else {}

  if("Maas" %in% char){
    lex.div.results@Maas.char <- ttr.calc.chars(txt.all.tokens, type="Maas", log.base=log.base)
    maas.lgV.chars <- function(base){
      if(!isTRUE(quiet)){
        # give some feedback, so we know the machine didn't just freeze...
        prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
      } else {}
      lgV0.char.res <- t(sapply(1:num.all.steps, function(x){
            curr.token <- x * char.steps
            if(!isTRUE(quiet)){
              # update progress bar
              setTxtProgressBar(prgBar, x)
            } else {}
            lgV0.char.temp <- c(token=curr.token, value=lgV0.calc(txt.all.tokens[1:curr.token], x=0, log.base=base))
            return(lgV0.char.temp)
          }
        ))
      if(!isTRUE(quiet)){
        # close prograss bar
        close(prgBar)
      } else {}
      return(lgV0.char.res)
    }
    if(!isTRUE(quiet)){
      message("lgV0.char: Calculate lgV0 values")
    } else {}
    lex.div.results@lgV0.char <- maas.lgV.chars(base=10)
    if(!isTRUE(quiet)){
      message("lgeV0.char: Calculate lgeV0 values")
    } else {}
    lex.div.results@lgeV0.char <- maas.lgV.chars(base=exp(1))
  } else {}

  ## calculate K characteristics
  if("K" %in% char){
    if(!isTRUE(quiet)){
      message("K.char: Calculate K values")
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    lex.div.results@K.char <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          k.char.temp <- c(token=curr.token, value=k.calc(txt.all.tokens[1:curr.token]))
          return(k.char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  } else {}

  ## calculate HD-D characteristics
  if("HD-D" %in% char){
    if(!isTRUE(quiet)){
      message("HDD.char: Calculate HD-D values")
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    lex.div.results@HDD.char <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          hdd.value <- hdd.calc(txt.all.tokens[1:curr.token], drawn=rand.sample)[["HDD"]]
          hdd.char.temp <- c(token=curr.token, value=hdd.value)
          return(hdd.char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  } else {}

  ## calculate MTLD characteristics
  if("MTLD" %in% char){
    if(!isTRUE(quiet)){
      message("MTLD.char: Calculate MTLD values")
      # just some feedback, so we know the machine didn't just freeze...
      prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
    } else {}
    if(isTRUE(is.na(lex.div.results@MTLD[["all.forw"]]))){
      mtld.char.forw <- mtld.sub.calc(txt.tokens=txt.all.tokens, factor.size=factor.size,
        num.tokens=num.all.tokens, detailed=TRUE)[["MTLD.all"]]
    } else {
      # if MTLD results have already been calculated, recycle the "all.forw" data
      mtld.char.forw <- lex.div.results@MTLD[["all.forw"]]
    }
    lex.div.results@MTLD.char <- t(sapply(1:num.all.steps, function(x){
          curr.token <- x * char.steps
          if(!isTRUE(quiet)){
            # update progress bar
            setTxtProgressBar(prgBar, x)
          } else {}
          mtld.back.value <- mtld.sub.nodata(txt.tokens=rev(txt.all.tokens[1:curr.token]), factor.size=factor.size,
            partial=TRUE, stopAtFirstFactor=FALSE, detailed=FALSE)[["factors"]]
          mtld.forw.value <- mtld.char.forw[which(mtld.char.forw[["end"]] == curr.token), "factors"]
          mtld.char.mean <- mean(c(mtld.forw.value, mtld.back.value))
          # uncomment to debug:
          # print(paste0("token: ", curr.token, "(", txt.all.tokens[curr.token],") forw: ", mtld.forw.value, "back: ", mtld.back.value, " -- MTLD: ", mtld.char.mean))
          mtld.char.value <- curr.token / mtld.char.mean
          mtld.char.temp <- c(token=curr.token, value=mtld.char.value)
          return(mtld.char.temp)
        }
      ))
    if(!isTRUE(quiet)){
      # close prograss bar
      close(prgBar)
    } else {}
  } else {}

  ## calculate MTLD-MA characteristics
  if("MTLD-MA" %in% char){
    # this needs the detailed results, already taken care of by calculating MTLDMA in the first place
    all.factorEnds <- sapply(lex.div.results@MTLDMA[["all"]], function(x) max(x[["end"]]))
    all.factorLengths <- lex.div.results@MTLDMA[["lengths"]][["all"]]
    # if the text is too short, no usable results can be expected
    if(!is.null(all.factorLengths) && length(all.factorEnds) > 0){
      if(!isTRUE(quiet)){
        message("MTLDMA.char: Calculate MTLD-MA values")
        # just some feedback, so we know the machine didn't just freeze...
        prgBar <- txtProgressBar(min=0, max=num.all.steps, style=3)
      } else {}
      lex.div.results@MTLDMA.char <- t(sapply(1:num.all.steps, function(x){
            curr.token <- x * char.steps
            if(!isTRUE(quiet)){
              # update progress bar
              setTxtProgressBar(prgBar, x)
            } else {}
            # well, of course a factor can't be complete if there's less text than the very first factor
            if(curr.token < all.factorEnds[1]){
              mtldma.value <- NA
            } else {
              # see at which point a next full factor would need more text than we have
              lastValidIndex <- min(which(all.factorEnds > curr.token)) - 1
              relevantFactorLenghts <- all.factorLengths[1:lastValidIndex]
              mtldma.value <- mean(relevantFactorLenghts[relevantFactorLenghts > min.tokens])
            }
            # uncomment to debug:
            # print(paste0("token: ", curr.token, "(", txt.all.tokens[curr.token],") -- MTLD-MA: ", mtldma.value, "lastValidIndex: ", lastValidIndex))
            mtldma.char.temp <- c(token=curr.token, value=mtldma.value)
            return(mtldma.char.temp)
          }
        ))
      if(!isTRUE(quiet)){
        # close prograss bar
        close(prgBar)
      } else {}
    } else {
      warning("MTLDMA.char: Not even one factor found, skipping!", call.=FALSE)
    }
    if(!isTRUE(detailed)){
      # in case the user actually didn't want details, remove them from the result
      # anything else would seem confusing
      lex.div.results@MTLDMA[["all"]] <- NA
    } else {}
  }

  lex.div.results@param <- list(segment=segment, factor.size=factor.size, min.tokens=min.tokens,
    rand.sample=rand.sample, window=window, case.sens=case.sens, lemmatize=lemmatize, log.base=log.base)

  # keep raw text material only if explicitly told so
  if(isTRUE(keep.tokens)){
    lex.div.results@tt <- list(tokens=txt.all.tokens, types=txt.type.freq, lemmas=txt.lemma.freq, num.tokens=num.all.tokens, num.types=num.all.types, num.lemmas=num.all.lemmas)
  } else {
    lex.div.results@tt <- list(tokens=character(), types=character(), lemmas=character(), num.tokens=num.all.tokens, num.types=num.all.types, num.lemmas=num.all.lemmas)
  }

  ## for the time being, give a warning until all implementations have been validated
  needs.warning <- measure %in% c("MATTR","S","K")
  if(!isTRUE(quiet) && any(needs.warning)){
    warning(paste0("Note: The implementations of these formulas are still subject to validation:\n  ",
    paste(measure[needs.warning], collapse=", "),
    "\n  Use the results with caution, even if they seem plausible!"), call.=FALSE)
  } else {}
  return(lex.div.results)
}
