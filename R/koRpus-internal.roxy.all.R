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


# internal package description
pckg.dscrptn <- data.frame(
    Package="koRpus",
    Type="Package",
    Title="An R Package for Text Analysis",
    Author="m.eik michalke, with contributions from Earl Brown,
      Alberto Mirisola, Alexandre Brulet, and Laura Hauser",
    AuthorsR="c(person(given=\"m.eik\", family=\"michalke\", email=\"meik.michalke@hhu.de\",
        role=c(\"aut\", \"cre\")),
      person(given=\"Earl\", family=\"Brown\", email=\"eabrown@csumb.edu\",
        role=c(\"ctb\")),
      person(given=\"Alberto\", family=\"Mirisola\",
        role=c(\"ctb\")),
      person(given=\"Alexandre\", family=\"Brulet\",
        role=c(\"ctb\")),
      person(given=\"Laura\", family=\"Hauser\",
        role=c(\"ctb\")))",
    Maintainer="m.eik michalke <meik.michalke@hhu.de>",
    Depends="R (>= 2.10.0),methods",
    Enhances="rkward",
    Suggests="testthat,tm,SnowballC,shiny",
    Description="A set of tools to analyze texts. Includes, amongst others, functions for automatic language detection,
            hyphenation, several indices of lexical diversity (e.g., type token ratio, HD-D/vocd-D, MTLD)
            and readability (e.g., Flesch, SMOG, LIX, Dale-Chall). Basic import functions for language corpora
            are also provided, to enable frequency analyses (supports Celex and Leipzig Corpora Collection file formats).
            #'
            Note: For full functionality a local installation of TreeTagger is recommended. Also, due to some restrictions
            on CRAN, the full package sources are only available from the project homepage.
            Be encouraged to send feedback to the author(s)!",
    License="GPL (>= 3)",
    Encoding="UTF-8",
    LazyLoad="yes",
    URL="http://reaktanz.de/?c=hacking&s=koRpus",
    stringsAsFactors=FALSE)
