\name{tokenize}
\alias{tokenize}
\title{A simple tokenizer}
\usage{
  tokenize(txt, format = "file", fileEncoding = NULL,
    split = "[[:space:]]", ign.comp = "-",
    heuristics = "abbr",
    heur.fix = list(pre = c("’", "'"), suf = c("’", "'")),
    abbrev = NULL, tag = TRUE, lang = "kRp.env",
    sentc.end = c(".", "!", "?", ";", ":"),
    detect = c(parag = FALSE, hline = FALSE))
}
\arguments{
  \item{txt}{Either an open connection, the path to
  directory with txt files to read and tokenize, or a
  vector object already holding the text corpus.}

  \item{format}{Either "file" or "obj", depending on
  whether you want to scan files or analyze the given
  object.}

  \item{fileEncoding}{A character string naming the
  encoding of all files.}

  \item{split}{A regular expression to define the basic
  split method. Should only need refinement for languages
  that don't separate words by space.}

  \item{ign.comp}{A character vector defining punctuation
  which might be used in composita that should not be
  split.}

  \item{heuristics}{A vector to indicate if the tokenizer
  should use some heuristics. Can be none, one or several
  of the following: \itemize{ \item{\code{"abbr"}}{Assume
  that "letter-dot-letter-dot" combinations are
  abbreviations and leave them intact.}
  \item{\code{"en"}}{Try to detect possesive suffixes like
  "'s", or shorting suffixes like "'ll" and treat them as
  one token} \item{\code{"fr"}}{Try to detect prefixes like
  "s'" or "l'" and treat them as one token} }}

  \item{heur.fix}{A list with the named vectors \code{pre}
  and \code{suf}. These will be used if \code{heuristics}
  were set to use one of the presets that try to detect
  pre- and/or suffixes. Change them if you document uses
  other characters than the ones defined by default.}

  \item{abbrev}{Path to a text file with abbreviations to
  take care of, one per line. Note that this file must have
  the same encoding as defined by \code{fileEncoding}.}

  \item{tag}{Logical. If \code{TRUE}, the text will be
  rudimentarily tagged and returned as an object of class
  \code{kRp.tagged}.}

  \item{lang}{A character string naming the language of the
  analyzed text. If set to \code{"kRp.env"} this is got
  from \code{\link[koRpus:get.kRp.env]{get.kRp.env}}. Only
  needed if \code{tag=TRUE}.}

  \item{sentc.end}{A character vector with tokens
  indicating a sentence ending. Only needed if
  \code{tag=TRUE}.}

  \item{detect}{A named logical vector, indicating by the
  setting of \code{parag} and \code{hline} whether
  \code{tokenize} should try to detect paragraphs and
  headlines.}
}
\value{
  If \code{tag=FALSE}, a character vector with the
  tokenized text. If \code{tag=TRUE}, returns an object of
  class \code{\link[koRpus]{kRp.tagged-class}}.
}
\description{
  This tokenizer can be used to try replace TreeTagger. Its
  results are not as detailed when it comes to word
  classes, and no lemmatization is done. However, for most
  cases this should suffice.
}
\details{
  \code{tokenize} can try to guess what's a headline and
  where a paragraph was inserted (via the \code{detect}
  parameter). A headline is assumed if a line of text
  without sentence ending punctuation is found, a paragraph
  if two blocks of text are separated by space. This will
  add extra tags into the text: "<kRp.h>" (headline
  starts), "</kRp.h>" (headline ends) and "<kRp.p/>"
  (paragraph), respectively. This can be useful in two
  cases: "</kRp.h>" will be treated like a sentence ending,
  which gives you more control for automatic analyses. And
  adding to that,
  \code{\link[koRpus:kRp.text.paste]{kRp.text.paste}} can
  replace these tags, which probably preserves more of the
  original layout.
}
\examples{
\dontrun{
tokenized.obj <- tokenize("~/mydata/corpora/russian_corpus/")
}
}
\keyword{misc}

