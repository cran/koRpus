# Copyright 2010-2019 Meik Michalke <meik.michalke@hhu.de>
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


#' Show methods for koRpus objects
#'
#' Show methods for S4 objects of classes
#' \code{\link[koRpus:kRp.lang-class]{kRp.lang}},
#' \code{\link[koRpus:kRp.readability-class]{kRp.readability}},
#' \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}} or
#' \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}.
#'
#' @param object An object of class \code{kRp.lang}, \code{kRp.readability},
#'    \code{kRp.corp.freq}, or \code{kRp.TTR}.
#' @aliases show,-methods show,kRp.lang-method
#' @seealso
#'    \code{\link[koRpus:kRp.lang-class]{kRp.lang}},
#'    \code{\link[koRpus:kRp.readability-class]{kRp.readability}},
#'    \code{\link[koRpus:kRp.corp.freq-class]{kRp.corp.freq}},
#'    \code{\link[koRpus:kRp.TTR-class]{kRp.TTR}}
#' @keywords methods
#' @examples
#' \dontrun{
#'   guess.lang("/home/user/data/some.txt", udhr.path="/home/user/data/udhr_txt/")
#' }
#' @export
#' @docType methods
#' @rdname show-methods
#' @include 01_class_04_kRp.lang.R
setMethod("show", signature(object="kRp.lang"), function(object){
  estim.lang <- slot(object, "lang.name")
  estim.lang.uli <- slot(object, "lang")
  estim.lang.udhr <- slot(object, "udhr")
  haveCountry <- "country" %in% colnames(estim.lang.udhr)
  if(isTRUE(haveCountry)){
    estim.lang.country <- estim.lang.udhr[1,"country"]
  } else {}
  estim.lang.region <- estim.lang.udhr[1,"region"]
  langs.available <- nrow(estim.lang.udhr)

  cat("\n  Estimated language: ", estim.lang,
     "\n          Identifier: ", estim.lang.uli,
    if(isTRUE(haveCountry)){
      paste0("\n             Country: ", estim.lang.country, " (", estim.lang.region,")\n")
    } else {
      paste0("\n              Region: ", estim.lang.region,"\n")
    },
     "\n", langs.available, " different languages were checked.\n\n",
     sep="")

})
