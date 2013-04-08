\name{NEWS}
\title{News for Package 'koRpus'}
\encoding{UTF-8}
\section{Changes in koRpus version 0.04-40 (2013-04-07)}{
  \subsection{fixed}{
    \itemize{
      \item removed some non-ASCII characters, mostly from comments, to keep the
        package on CRAN; some author names are now spelled wrong, though...
    }
  }
}
\section{Changes in koRpus version 0.04-39 (2013-03-12)}{
  \subsection{fixed}{
    \itemize{
      \item optimized \code{tokenize()} to also detect prefixes/suffixes of the defined
        heuristics if they co-occur with punctuation
      \item re-saved hyph.fr.rda with explicitly UTF-8 ecoded vectors
      \item renamed LICENSE to LINCENSE.txt, so it won't get installed, as demnanded
        by Writing R Extensions
    }
  }
  \subsection{changed}{
    \itemize{
      \item the language specific heuristics \code{"en"} and \code{"fr"} in \code{tokenize()} were renamed
        into \code{"suf"} and \code{"pre"}. but they are still available, with \code{"fr"} now
        activating both \code{"suf"} and \code{"pre"}.
      \item \code{read.hyph.pat()} now explicitly sets vector encoding to UTF-8 with
        \code{Encoding()}<-, to ensure that the generated objects don't cause warnings from R
        CMD check if they're included in packages
      \item internally replaced paste(..., sep=\code{""}) with paste0(...)
    }
  }
  \subsection{added}{
    \itemize{
      \item added new getter/setter methods \code{taggedText()}, \code{taggedText()}<-, \code{describe()},
        \code{describe()}<-, \code{language()} and \code{language()}<- for tagged text objects
      \item added \code{is.taggedText()} test function
      \item added a warning to \code{treetag()} if \code{"TT.options"} is not a list (because this
        will likely render the options meaningless if they *contain* a list).
      \item \code{tokenize()} can now apply a list of patterns/replacements to given texts
        via the new \code{"clean.raw"} attribute, and even supports perl-like regular
        expressions. the replacements are done before the texts are tokenized, so this
        can be tried to globally clean up bad characters or simply replace
        strings, etc.
      \item \code{tokenize()} and \code{treetag()} have a new option \code{"stopwords"} to enable stopword
        detection
      \item \code{kRp.filter.wclass()} can now remove detected stopwords
      \item \code{tokenize()} and \code{treetag()} have a new option \code{"stemmer"} to interface with
        stemmer functions/methods like \code{Snowball::SnowballStemmer()}
    }
  }
}
\section{Changes in koRpus version 0.04-38 (2012-11-30)}{
  \subsection{added}{
    \itemize{
      \item added support for french (thanks to alexandre brulet)
    }
  }
}
\section{Changes in koRpus version 0.04-37 (2012-09-15)}{
  \subsection{fixed}{
    \itemize{
      \item a typo in Spache calculation (substraction instead of addition of a
        constant) lead to wrong results
      \item Spache now counts unfamiliar words only once, as explained in the
        original article
      \item old Spache formula was missing in readability(index=\code{"all"})
    }
  }
  \subsection{changed}{
    \itemize{
      \item validated Linsear Write, Dale-Chall (1948) and Spache (1953) results and
        removed warnings
      \item status messages of \code{hyphen()} and \code{lex.div()} have been replaced by a space
        saving prograss bar added
      \item added tests for \code{lex.div()}, \code{hyphen()} and \code{readability()}
    }
  }
}
\section{Changes in koRpus version 0.04-36 (2012-08-27)}{
  \subsection{fixed}{
    \itemize{
      \item tests should now work on any machine
    }
  }
}
\section{Changes in koRpus version 0.04-35 (2012-08-21)}{
  \subsection{changed}{
    \itemize{
      \item using utf8-tokenizer.perl now in all UTF-8 presets, also on windows
        systems. the script is part of the windows installer of TreeTagger 3.2 (at
        least since june 2012)
    }
  }
  \subsection{fixed}{
    \itemize{
      \item correct.*() methods now also update the descriptive statistics in
        corrected objects
    }
  }
}
\section{Changes in koRpus version 0.04-34 (2012-06-02)}{
  \subsection{added}{
    \itemize{
      \item there's now a class union \code{"kRp.taggedText"} with the members \code{"kRp.tagged"},
        \code{"kRp.analysis"}, \code{"kRp.txt.freq"} and \code{"kRp.txt.trans"}
    }
  }
  \subsection{changed}{
    \itemize{
      \item advanced \code{summary()} statistics for objects returned by \code{clozeDelete()}
      \item clozeDelete(offset=\code{"all"}) now iterates through all cloze variants and
        prints the results, including the new \code{summary()} data
      \item \code{clozeDelete()} now uses the new class union \code{"kRp.taggedText"} as signature
      \item \code{read.corp.custom()} now uses \code{table()}, \code{"quiet"} is TRUE by default, the new
        option \code{"caseSens"} can be used to ignore character case, and \code{"corpus"} can
        now also be a tagged text object
    }
  }
  \subsection{fixed}{
    \itemize{
      \item \code{summary()} for objects of class kRp.txt.freq was broken
      \item as(\code{"kRp.tagged"}) for objects of class kRp.txt.freq was broken
    }
  }
}
\section{Changes in koRpus version 0.04-33 (2012-05-26)}{
  \subsection{changed}{
    \itemize{
      \item elaborated documentation for method \code{cTest()}
    }
  }
  \subsection{added}{
    \itemize{
      \item added new method \code{clozeDelete()}
      \item added new list \code{"cTest"} in desc slot of the objects returned by \code{cTest()},
        which lists all words that were changed (in \code{clozeDelete()} this list is
        called \code{"cloze"})
    }
  }
}
\section{Changes in koRpus version 0.04-32 (2012-05-11)}{
  \subsection{added}{
    \itemize{
      \item added new function \code{jumbledWords()} and new method \code{cTest()}
    }
  }
  \subsection{fixed}{
    \itemize{
      \item \code{kRp.text.paste()} now also removes superfluous spaces at the end of texts
        (i.e., before the last fullstop)
    }
  }
}
\section{Changes in koRpus version 0.04-31 (2012-04-22)}{
  \subsection{added}{
    \itemize{
      \item koRpus now suggests the \code{"testthat"} package and uses it for automatic tests
      \item \code{treetag()} and \code{tokenize()} now also accept input from open connections
    }
  }
  \subsection{fixed}{
    \itemize{
      \item \code{treetag()} shouldn't fail on file names with spaces any more
    }
  }
}
\section{Changes in koRpus version 0.04-30 (2012-04-06)}{
  \itemize{
    \item added features:
    \item kRp.corp.freq class objects now include the columns 'lttr', 'lemma',
      'tag' and 'wclass'
    \item \code{query()} for corpus frequency objects now returns objects of the same
      class, to allow nested queries
    \item the 'query' parameter of \code{query()} can now be a list of lists, to
      facilitate nested requests more easily
    \item \code{query()} can now invoke \code{grepl()}, if 'var' is set to \code{"regexp"}; i.e., you
      can now filter words by regular expressions (inspired by suggestions after
      the koRpus talk at TeaP 2012)
  }
}
\section{Changes in koRpus version 0.04-29 (2012-04-05)}{
  \itemize{
    \item fixed bug in \code{summary()} for tagged objects without punctuation
    \item renamed \code{kRp.freq.analysis()} to \code{freq.analysis()} (with wrapper function for
      backwards compatibility)
    \item \code{readability.num()} can now directly digest objects of class kRp.readability
    \item data documentation hyph.XX is now a roxygen source file as well
    \item cleaned up \code{summary()} and \code{show()} docs
    \item adjustements to the roxygen2 docs (methods)
  }
}
\section{Changes in koRpus version 0.04-28 (2012-03-10)}{
  \itemize{
    \item code cleanup: initialized some variables by setting them NULL, to avoid
      needless NOTEs from R CMD check (\code{hyphen()}, and internal functions
      \code{frqcy.by.rel()}, \code{load.hyph.pattern()}, \code{tagged.txt.rm.classes()} and
      \code{text.freq.analysis()})
    \item re-formatted the ChangeLog so roxyPackage can translate it into a NEWS.Rd
      file
  }
}
\section{Changes in koRpus version 0.04-27 (2012-03-07)}{
  \itemize{
    \item prep for CRAN release:
    \item 0.04-26 was short-lived...
    \item really fixed plot docs
    \item removed usage section from hyph.XX data documentation
    \item renamed \code{text.features()} to \code{textFeatures()}
    \item encapsulated examples in \code{set.kRp.env()}/get.\code{kRp.env()} in \\dontrun\{\}
    \item re-encoded hyph.XX data objects to UTF-8
    \item replaces non-ASCII characters in code with unicode escapes
  }
}
\section{Changes in koRpus version 0.04-26 (2012-03-07)}{
  \itemize{
    \item fixed plot docs
    \item prep for inital CRAN release
  }
}
\section{Changes in koRpus version 0.04-25 (2012-03-05)}{
  \itemize{
    \item re-compressed all hyphenation pattern data files, using xz compression
    \item lifted the R dependency from 2.9 to 2.10
    \item compressed LCC tarballs are now detected automatically
    \item \code{kRp.freq.analysis()} now also lists the log10 value of word frequencies in
      the TT.res slot
    \item in the desc slot of kRp.txt.freq class objects, the rather misleading
      list elements \code{"freq"} and \code{"freq.wclass"} were more adequately renamed to
      \code{"freq.token"} and \code{"freq.types"}, respectively
    \item unmatched words in frequency analyses now get value 0, not NA
    \item fixed wrong signature for option \code{"tagger"} in \code{kRp.text.analysis()}
    \item fixed \code{kRp.cluster()} which still called some old slots
  }
}
\section{Changes in koRpus version 0.04-24 (2012-03-01)}{
  \itemize{
    \item fixed bug for attempts to calculate value distribution texts without any
      sentence endings
    \item all readability wrapper functions now also accept a list of text features
      for calculation
    \item class kRp.readability now inherits kRp.tagged
    \item \code{readability()} now checks for presence of a hyphen slot and re-uses it, if
      no new hyphen object was provided; this in addition to the previous
      change enables one to re-analyze a text more efficiently, as already calculated
      results are also preserved
    \item letter and character distribution in kRp.tagged desc slot now include
      columns with zero values if the respective values are missing (e.g., no words
      with five letters, but some with six, etc.)
    \item added summary method for class kRp.tagged, summarizing main information
      from the desc slot
    \item added plot method for class kRp.tagged
    \item show method for kRp.readability now lists unfamiliar words for
      Harris-Jacobson
    \item cleaned up code of \code{lex.div.num()} a bit
  }
}
\section{Changes in koRpus version 0.04-23 (2012-02-24)}{
  \itemize{
    \item added precise RGL formula option to FORCAST
    \item removed validation warnings from several indices, because results have
      been checked against those of other tools, and were comparable, so the
      implementations of these measures are assumed to be correct: - \code{lex.div()}: TTR,
      MSTTR, C, R, CTTR, U, Maas, HD-D, MTLD (thanks a lot to scott jarvis &
      phil mccarthy for calculating sample texts!) - \code{readability()}: ARI, ARI NRI,
      Bormuth, Coleman-Liau, Dale-Chall, Dale-Chall PSK, DRP,
      Farr-Jenkins-Paterson, Farr-Jenkins-Paterson PSK, Flesch, Flesch PSK, Flesch-Kincaid, FOG,
      FOG PSK, FORCAST, LIX, RIX, SMOG, Spache, Wheeler-Smith
    \item moved all calculation from \code{readability()} to an internal function
      \code{kRp.rdb.formulae()}. to make it easier to write a similar function to \code{lex.div.num()}
      for the readability fomulas as well
    \item added \code{readability.num()}
    \item adjusted exsyl calculation for ELF to the approach used in other
      measures, which also results in a change of its default \code{"syll"} parameter from 1 to
      2; also corrected a typo in the docs, the index was proposed by Fang, not
      Farr
    \item readability results now list letter distribution, not character
      distribution in desc slot
    \item the desc slot from readability calculations was enhanced so that it can
      directly be used as the txt.features parameter for \code{readability.num()}
    \item docs were polished
  }
}
\section{Changes in koRpus version 0.04-22 (2012-02-08)}{
  \itemize{
    \item further fixes to the Wheeler-Smith implementation. according to the
      original paper, polysyllabic words need to be counted, and the example given
      shows that this means words with more than one syllable, not three or more,
      as Bamberger & Vanecek (1984) suggested
    \item fixed HD-D, previous results are now labelled as ATTR in the HDD slot
    \item adjusted HD-D.char calculation for small number of tokens (probabilities
      are now set to 1, not NaN)
    \item added MATTR characteristics
    \item \code{show()} for \code{lex.div()} objects now also reports SD for characteristics
  }
}
\section{Changes in koRpus version 0.04-21 (2012-02-07)}{
  \itemize{
    \item MTLD now uses a slightly more efficient algorithm, inspired by the one
      used for MATTR
    \item MSTTR now also reports SD of TTRs
    \item differentiated the word class adposition into pre-, post- and
      circumposition in the language support for german and russian
    \item added both Tränke-Bailer formulae to \code{readability()}, incl. wrapper
      \code{traenkle.bailer()} and \code{show()}/summary() methods
    \item Coleman formulae now also count only prepositions as such
    \item fixed Wheeler-Smith (thanks to eleni miltsakaki)
  }
}
\section{Changes in koRpus version 0.04-20 (2012-02-06)}{
  \itemize{
    \item added Moving Average TTR (MATTR) to \code{lex.div()}, incl. wrapper  \code{MATTR()} and
      \code{show()}/summary() methods
    \item added \code{"rand.sample"} and \code{"window"} to the parameters returned by \code{lex.div()}
    \item further re-arranged the code of \code{readability()} and \code{lex.div()} to make it
      easier to maintain
    \item summary(flat=TRUE) for readability objects is now a numeric vector
  }
}
\section{Changes in koRpus version 0.04-19 (2012-02-02)}{
  \itemize{
    \item added five harris-jacobson readability formulae, incl. wrapper
      \code{harris.jacobson()} and \code{show()}/summary() methods
    \item updated vignette
    \item MTLD characteristics are now twice as fast
    \item classes \code{"kRp.txt.freq"} and \code{"kRp.txt.trans"} now simply extend
      \code{"kRp.tagged"}, and \code{"kRp.analysis"} extends \code{"kRp.txt.freq"}
    \item removed internal function \code{check.kRp.object()} (globally replaced by
      \code{inherits()})
    \item fixed letter count issue in \code{readability()}
    \item fixed bugs in loading word lists in \code{readability()}
    \item fixed crash if index=\code{"all"} in \code{readability()}
    \item reordered default kRp.readabilty slot order alphabetically, as well as
      \code{show()} and \code{summary()} for readability results
    \item renamed results of the Neue Wiener Sachtextformeln from WSTF* to nWS* in
      readability object methods \code{show()} and \code{summary()} for consistency
    \item renamed \code{WSFT()} to \code{nWS()} for the same reason
    \item cleaned up roxygen comments for more roxygen2 compliance
  }
}
\section{Changes in koRpus version 0.04-18 (2012-01-22)}{
  \itemize{
    \item added missing word exclusion to Gunning FOG measure
    \item added sentence length, word length, distribution of characters and
      letters to \code{"desc"} slot of class kRp.tagged and \code{readability()} results, where
      missing
    \item both syllable (\code{hyphen()}) and character distributions gained inversed
      cummulation for absolute numbers and percentages, so this one table now makes
      it easy to see how many words with more/equal/less characters/syllables
      there are in a text
    \item changed internals of \code{kRp.freq.analysis()} and \code{readability()} to re-use
      descriptives of tagged text objects
    \item NOTE: this also changed the names of some result elements in their \code{"desc"}
      slots for overall consistency (\code{"avg.sent.len"} is now \code{"avg.sentc.length"},
      \code{"avg.word.len"} became \code{"avg.word.length"}, and instances of \code{"num.words"},
      \code{"num.chars"} etc. lost the \code{"num."} prefix). in case you accessed these
      directly, check if you need to adopt these changes. this is a first round of
      changes towards 0.05, see the notes to 0.04-17 below!
  }
}
\section{Changes in koRpus version 0.04-17 (2012-01-17)}{
  \itemize{
    \item replaced the english hyphenation parameter set with a new one, which was
      made with PatGen2 especially for koRpus
    \item \code{tokenize()} will now interpret single letters followed by a dot as an
      abbreviation (e.g., of a name), not a sentence ending, if heuristics include
      \code{"abbr"}
    \item fixed bug which caused \code{hyphen()} to drop syllables if only one pattern
      match was found
    \item added cache support to the correct method of class kRp.hyphen
    \item added number of words and sentences to \code{"desc"} slot of class kRp.tagged
    \item elaborated \code{treetag()} error message if no TreeTagger command was specified
    \item NOTE: koRpus 0.05 will likely merge some object classes similar to
      kRp.tagged, i.e. kRp.txt.freq and kRp.txt.trans, into one class for tokenized
      text, either replacing or inheriting those classes
  }
}
\section{Changes in koRpus version 0.04-16 (2012-01-15)}{
  \itemize{
    \item added slot \code{"desc"} to class kRp.tagged, to have descriptive statistics
      directly available in the object
    \item added support for descriptive statistics to \code{tokenize()} and \code{treetag()}
    \item added function \code{text.features()} to extract a 9-features set from texts for
      authorship detection (inspired by a talk at the 28C3)
    \item \code{hyphen()} can now cache results on a per session basis, making it
      noticeably faster
  }
}
\section{Changes in koRpus version 0.04-15 (2012-01-04)}{
  \itemize{
    \item \code{manage.hyph.pat()} is now an exported function
    \item added initial support for italian (thanks to alberto mirisola)
    \item added italian hyphenation patterns
    \item changed min.length from 4 to 3 in \code{hyphen()} and \code{manage.hyph.pat()}
    \item hyphen now considers hyphenating before last letters of a word
    \item tuned hyph.en (with contributions by laura hauser)
    \item fixed check for existing tokenizer, tagger and parameter file in \code{treetag()}
    \item fixed MTLD calculation for texts which don't make even one factor
  }
}
\section{Changes in koRpus version 0.04-14 (2011-12-22)}{
  \itemize{
    \item added new internal function \code{manage.hyph.pat()} to add/replace/remove
      pattern entries for hyphenation
    \item added number of tokens per factor and standard deviation to MTLD results
      (thx to aris xanthos for the suggestion)
  }
}
\section{Changes in koRpus version 0.04-13 (2011-11-22)}{
  \itemize{
    \item added column \code{"token"} to slots MTLD$all.forw and MTLD$all.back of
      \code{lex.div()} results, so you can verify the results more easily
    \item slot HDD$type.probs of \code{lex.div()} results is now sorted (decreasing)
    \item removed warnings of missing encoding, since \code{enc2utf()} seems to do a
      pretty good job
  }
}
\section{Changes in koRpus version 0.04-12 (2011-11-21)}{
  \itemize{
    \item added support for the newer LCC .tar archive format
    \item changed vignette accordingly
    \item for consistency, changed \code{"words"} and \code{"dist.words"} into \code{"tokens"} and
      \code{"types"} in class kRp.corp.freq, slot desc
    \item added lgeV0 and the relative vocabulary growth measures suggested by Maas
      to \code{lex.div()}; furthermore, a is now reported instead of a^2
    \item added lgV0 and lgeV0 to \code{lex.div.num()}
    \item show method for class kRp.TTR now excludes Inf values from
      charasteristics values
  }
}
\section{Changes in koRpus version 0.04-11 (2011-11-20)}{
  \itemize{
    \item added function \code{lex.div.num()}, calculates TTR family measures by numbers
      of tokens and types directly
    \item cleaned up \code{lex.div()} code a little
  }
}
\section{Changes in koRpus version 0.04-10 (2011-11-19)}{
  \itemize{
    \item fixed missing 'input.enc' information if \code{treetag()} option 'treetagger' is
      not \code{"manual"} but a script
    \item enhanced encoding handling internally if none was specified
    \item changed default value of 'case.sens' to FALSE in \code{lex.div()}, as this seems
      to be more common
    \item changed default value of 'fileEncoding' from "UTF-8" to NULL and use
      \code{enc2utf()} internally if no encoding was defined
  }
}
\section{Changes in koRpus version 0.04-9 (2011-10-27)}{
  \itemize{
    \item \code{tokenize()} now converts all input to UTF-8 internally, to prevent
      conflicts later on (\code{treetag()} does that since 0.04-7 already)
    \item added an experimental feature to \code{treetag()} to replace TreeTagger's
      tokenizer with \code{tokenize()}
  }
}
\section{Changes in koRpus version 0.04-8 (2011-09-21)}{
  \itemize{
    \item fixed bugs in \code{treetag()}: \code{"debug"} now works without \code{"manual"} config as
      well, and global TT.options are now found if no preset was selected
  }
}
\section{Changes in koRpus version 0.04-7 (2011-09-16)}{
  \itemize{
    \item added \code{"encoding"} option to \code{treetag()} and defaults to the language presets
    \item fixed some option check and file path issues in \code{treetag()}
  }
}
\section{Changes in koRpus version 0.04-6 (2011-09-11)}{
  \itemize{
    \item fixed package description for R 2.14
  }
}
\section{Changes in koRpus version 0.04-5 (2011-09-01)}{
  \itemize{
    \item fixed dozends of small glitches in the docs which caused warnings during
      package checks
  }
}
\section{Changes in koRpus version 0.04-4 (2011-08-23)}{
  \itemize{
    \item fixed bug in getting the right preset: mixed \code{"lang"} and \code{"preset"} during
      the modularization
  }
}
\section{Changes in koRpus version 0.04-3 (2011-08-19)}{
  \itemize{
    \item modularized language support by the internal function \code{set.lang.support()},
      this should make it much easier to add new languages in the future,
      because it means to add only one R file. \code{hyphen()}, \code{kRp.POS.tags()} and \code{treetag()}
      now use this new method
    \item added CITATION file
  }
}
\section{Changes in koRpus version 0.04-2 (2011-08-18)}{
  \itemize{
    \item fixed duplicate \code{"PREP"} definition in spanish POS tags, which caused
      \code{treetag()} to consume lots of RAM
    \item fixed superfluous \code{"es"} definitions in \code{treetag()}
  }
}
\section{Changes in koRpus version 0.04-1 (2011-08-16)}{
  \itemize{
    \item added support for spanish (thanks to earl brown)
    \item docs can be created from source by roxygen2 (but all class docs are
      static, until '@slot' works again)
  }
}
\section{Changes in koRpus version 0.03-4 (2011-08-09)}{
  \itemize{
    \item added support for autodetection of headlines and paragraphs in \code{tokenize()}
    \item added support to revert autodetected headlines and paragraphs in
      \code{kRp.text.paste()}
    \item updated RKWard plugin to use \code{tokenize()}
  }
}
\section{Changes in koRpus version 0.03-3 (2011-08-08)}{
  \itemize{
    \item added parameters for formula C and simplified formula to SMOG
    \item enhanced readability formulas (like adding age levels to Flesch.Kincaid,
      grade levels to LIX)
    \item removed the duplicate Amstad index (is now just Flesch.de)
  }
}
\section{Changes in koRpus version 0.03-2 (2011-08-03)}{
  \itemize{
    \item added the full RKWard plugin as inst/rkward, so both get updated
      simultanously
    \item added experimental internal functions to import result logs from
      Readability Studio and TextQuest
  }
}
\section{Changes in koRpus version 0.03-1 (2011-07-29)}{
  \itemize{
    \item integrated internal tags to \code{kRp.POS.tags()}, so \code{tokenize()} can return
      valid kRp.tagged class objects, i.e. substitute TreeTagger if it's not
      available
    \item consequently renamed 'treetagger' option into 'tagger' in \code{readability()},
      \code{kRp.freq.analysis()} and \code{kRp.text.analysis()}
    \item lots of small fixes
  }
}
\section{Changes in koRpus version 0.02-9 (2011-07-17)}{
  \itemize{
    \item added a simple \code{tokenize()} function
    \item first working version of \code{read.corp.custom()}
    \item added \code{"..."} option to readability, kRp.freq.analysis and
      kRp.text.analysis, to configure \code{treetag()}
    \item added TT.options to the get/set environment functions
    \item changed default values for \code{treetag()} (for readability)
    \item fixed bug in internal \code{check.file()} function (mode=\code{"exec"} returned TRUE
      too soon)
    \item added warning messages to \code{readability()} and \code{lex.div()} to make people
      aware these implemetations are not yet fully validatied
    \item introduced release dates in this ChangeLog ;-) (reconstructed them for
      earlier releases from the time stamps on the server)
  }
}
\section{Changes in koRpus version 0.02-8 (2011-07-03)}{
  \itemize{
    \item added \code{"desc"} slot with some statistics to class kRp.hyphen and \code{hyphen()}
    \item added grading information for Flesch and RIX measures
    \item fixed grading for Wheeler-Smith formula
    \item introduced \code{"quiet"} options for \code{hyphen()}, \code{lex.div()} and \code{readability()}
    \item further improved the vignette, elaborated on the examples
  }
}
\section{Changes in koRpus version 0.02-7 (2011-06-29)}{
  \itemize{
    \item fixed typo in kRp.POS.tags(\code{"ru"}): "Vmis-sfa-e" tags no longer a \code{"vern"},
      but a \code{"verb"}
    \item removed XML package dependency again, by writing a small parser (there
      was no windows binary for the XML package, which was obviously a problem...)
    \item fixed \code{"quiet"} option in \code{guess.lang()}
  }
}
\section{Changes in koRpus version 0.02-6 (2011-06-26)}{
  \itemize{
    \item fixed bug in calculation of sentence lengths in \code{kRp.freq.analysis()}
      (counted punctuation as words)
    \item tweaked hyph.en patterns to get better results
    \item solved a small charset issue in \code{treetag()}
    \item fixed \code{hyphen()} output if doubled hyphenation marks appeared
  }
}
\section{Changes in koRpus version 0.02-5 (2011-06-25)}{
  \itemize{
    \item elaborated the vignette a little (including some references)
    \item added support for zipped LCC database archives to \code{read.corp.LCC()}
    \item improved handling of unknown POS tags: now causes an error dump for
      debugging
    \item added \code{query()} method to search in objects of class kRp.tagged
  }
}
\section{Changes in koRpus version 0.02-4 (2011-06-18)}{
  \itemize{
    \item de-factorized \code{treetag()} output
    \item fixed hyphenation problems (remove all non-characters for \code{hyphen()})
  }
}
\section{Changes in koRpus version 0.02-3 (2011-06-11)}{
  \itemize{
    \item fixed missing "''" and "$" POS tags in kRp.POS.tags(\code{"en"})
  }
}
\section{Changes in koRpus version 0.02-2 (2011-06-06)}{
  \itemize{
    \item renamed \code{kRp.guess.lang()} to \code{guess.lang()}
    \item \code{guess.lang()} now gzips only in memory by default, saves about 1/8 of
      processing time - added option \code{"in.mem"} to switch back to previous behavious
      (temporary files)
    \item added internal function \code{is.supported.lang()} as a possible wrapper for
      guessed ULIs
    \item added internal functions \code{roxy.description()} and \code{roxy.package()} to ease
      development
  }
}
\section{Changes in koRpus version 0.02-1 (2011-06-04)}{
  \itemize{
    \item added support for automatic language determination: - changed internal
      function \code{compression.ratio()} to \code{txt.compress()} - added internal function
      \code{read.udhr()} - added \code{kRp.guess.lang()} and class kRp.lang
  }
}
\section{Changes in koRpus version 0.01-8 (2011-05-30)}{
  \itemize{
    \item added class kRp.txt.trans for results of \code{kRp.text.transform()}
    \item enhanced function \code{kRp.text.transform()}, most notably calculate differences
  }
}
\section{Changes in koRpus version 0.01-7 (2011-05-28)}{
  \itemize{
    \item added function \code{kRp.text.paste()}
    \item added function \code{kRp.text.transform()}
  }
}
\section{Changes in koRpus version 0.01-6 (2011-05-27)}{
  \itemize{
    \item fixed \code{hyphen()} bug (leading dots in words caused functions to fail)
    \item added \code{kRp.filter.wclass()}
    \item added TODO list to the sources
  }
}
\section{Changes in koRpus version 0.01-5 (2011-05-16)}{
  \itemize{
    \item fixed another bug in frequency analysis with corpus data (superfluous
      class definition)
    \item fixed missing POS tags: refinement of english tags (extra tags for "to
      be" and "to have")
    \item added more to the vignette
    \item added .Rinstignore file to clean up the doc folder
  }
}
\section{Changes in koRpus version 0.01-4 (2011-05-12)}{
  \itemize{
    \item began to write a vignette
    \item fixed \code{treetag()} failing on windows machines (hopefully...)
  }
}
\section{Changes in koRpus version 0.01-3 (2011-05-10)}{
  \itemize{
    \item added TRI readability index
    \item fixed bug in frequency analysis with corpus data (wrong class definition)
    \item fixed bug in Bormuth implementation (didn't fetch parameters)
    \item fixed missing Flesch indices in summary method
    \item corrected display of FOG indices in summary method (grade instead of raw)
    \item added \code{compression.ratio()} to internal functions
  }
}
\section{Changes in koRpus version 0.01-2 (2011-05-03)}{
  \itemize{
    \item enhanced \code{query()} methods
    \item fixed some typos and smaller bugs
  }
}
\section{Changes in koRpus version 0.01-1 (2011-04-24)}{
  \itemize{
    \item initial public release (via reaktanz.de)
  }
}
