\name{kRp.TTR-class}
\alias{kRp.TTR-class}
\alias{kRp.TTR,-class
-class}
\alias{kRp.TTR,-class}
\title{S4 class kRp.TTR}
\description{Class kRp.TTR}
\details{This class is used for objects that are returned by \code{\link[koRpus:lex.div]{lex.div}} and its wrapper functions
(like \code{TTR}, \code{MSTTR}, \code{MTLD}, etc.).}
\keyword{classes}
\author{m.eik michalke \email{meik.michalke@uni-duesseldorf.de}}
\section{Slots}{\describe{\item{\code{param}:}{(\code{\link{list}}) Relevant parameters of the given analysis,
      as given to the function call ,
      see \code{\link[koRpus:lex.div]{lex.div}} for details.}\item{\code{tt}:}{(\code{\link{list}}) The analyzed text in tokenized form,
      with four elements ("tokens", "types", "lemmas", "num.tokens",
      "num.types",
      "num.lemmas").}\item{\code{TTR}:}{(\code{\link{numeric}}) Value of the classic type-token ratio. NA if not calculated.}\item{\code{MSTTR}:}{(\code{\link{list}}) Mean segmental type-token ratio,
      including the actual "MSTTR",
TTR values of each segment ("TTR.seg"),
      and the number of dropped words due to segment size ("dropped"). NA if not calculated.}\item{\code{C.ld}:}{(\code{\link{numeric}}) Herdan's C. NA if not calculated.}\item{\code{R.ld}:}{(\code{\link{numeric}}) Guiraud's R. NA if not calculated.}\item{\code{CTTR}:}{(\code{\link{numeric}}) Carroll's CTTR. NA if not calculated.}\item{\code{U.ld}:}{(\code{\link{numeric}}) Uber Index. NA if not calculated.}\item{\code{S.ld}:}{(\code{\link{numeric}}) Summer's S. NA if not calculated.}\item{\code{K.ld}:}{(\code{\link{numeric}}) Yule's K. NA if not calculated.}\item{\code{Maas}:}{(\code{\link{numeric}}) Maas' a. NA if not calculated.}\item{\code{lgV0}:}{(\code{\link{numeric}}) Maas' lgV0. NA if not calculated.}\item{\code{lgeV0}:}{(\code{\link{numeric}}) Maas' lgeV0. NA if not calculated.}\item{\code{Maas.grw}:}{(\code{\link{vector}}) Maas' relative vocabulary growth measures. A vecor of NAs if not calculated.}\item{\code{HDD}:}{(\code{\link{list}}) The actual HD-D value ("HDD")
, a vector with the probabilies for each type ("type.probs"), a "summary"
on these probabilities and their standard deviation "sd".}\item{\code{MTLD}:}{(\code{\link{list}}) Measure of textual lexical diversity,
      including the actual "MTLD", two matrices with detailed
information on forward and backward factorization ("all.forw" & "all.back"),
      and a named vector holding both calculated 
factors and their mean value ("factors"). NA if not calculated.}\item{\code{MTLDMA}:}{(\code{\link{list}}) Moving-average MTLD,
      including the actual
"MTLDMA" its standard deviation,
      a list ("all") with detailed information on factorization and a named list with information on the number or tokens in
each factor,
      as well as their mean and standard deviation ("lengths"). NA if not calculated.}
\item{\code{TTR.char}:}{(\code{\link{matrix}}) TTR values,
      starting with the first steplength of tokens, then adding the next one,
      progressing until
the whole text is analyzed. The matrix has two colums,
      one for the respective step ("token") and one for the actual values
("value"). Can be used to plot TTR characteristic curves. NA if not calculated.}\item{\code{C.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Herdan's C algorithm. NA if not calculated.}\item{\code{R.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Guiraud's R algorithm. NA if not calculated.}\item{\code{CTTR.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Carroll's CTTR algorithm. NA if not calculated.}\item{\code{U.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using the Uber Index algorithm. NA if not calculated.}\item{\code{S.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Summer's S algorithm. NA if not calculated.}\item{\code{K.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Yule's K algorithm. NA if not calculated.}\item{\code{Maas.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Maas' a algorithm. NA if not 
calculated.}\item{\code{lgV0.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Maas' lgV0 algorithm. NA if not calculated.}\item{\code{lgeV0.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using Maas' lgeV0 algorithm. NA if not calculated.}\item{\code{HDD.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using the HD-D algorithm. NA if not calculated.}\item{\code{MTLD.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using the MTLD algorithm. NA if not calculated.}\item{\code{MTLDMA.char}:}{(\code{\link{matrix}}) Equivalent to TTR.char,
      but calculated using the moving-average MTLD algorithm. NA if not calculated.}}}

