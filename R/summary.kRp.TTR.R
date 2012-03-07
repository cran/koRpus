#' Summary method for objects of class kRp.TTR
#'
#' Summary method for S4 objects of class \code{\link[koRpus]{kRp.TTR-class}}
#'
#' @param object An object of class \code{kRp.TTR}
#' @aliases summary,-methods summary,kRp.TTR-method
#' @author m.eik michalke \email{meik.michalke@@hhu.de}
#' @seealso \code{\link[koRpus]{kRp.TTR-class}}
#' @keywords methods
#' @examples
#' \dontrun{
#' summary(lex.div(tagged.txt))
#' }
#' @rdname summary-methods
setGeneric("summary")

#' @exportMethod summary
#' @rdname summary-methods
setMethod("summary", signature(object="kRp.TTR"), function(object){

	# function to add stuff to the matrix,
	# "adds" must me a named list
	add.to.sumtab <- function(table, adds){
		if("index" %in% names(adds) & !is.na(adds[["index"]])){
			add.index <- adds[["index"]]
		} else {
			add.index <- ""
		}
		if("value" %in% names(adds) & !is.na(adds[["value"]])){
			add.value <- round(adds[["value"]], digits=2)
		} else {
			add.value <- ""
		}
# 		if("interp" %in% names(adds)){
# 			add.interp <- adds[["interp"]]
# 		} else {
# 			add.interp <- ""
# 		}
		res.table <- rbind(table, list(add.index, add.value))
		return(res.table)
	}

# "TTR.char"
# "C.char"
# "R.char"
# "CTTR.char"
# "U.char"
# "S.char"
# "K.char"
# "Maas.char"
# "lgV0.char"
# "HDD.char"
# "MTLD.char"

	summary.table <- data.frame(index="", value="", stringsAsFactors=FALSE)

	if(!sum(!is.na(object@TTR)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="TTR",
				value=object@TTR
			))
	} else {}

	if(!sum(!is.na(object@MSTTR)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="MSTTR",
				value=object@MSTTR$MSTTR
			))
	} else {}

	if(!sum(!is.na(object@MATTR)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="MATTR",
				value=object@MATTR$MATTR
			))
	} else {}

	if(!sum(!is.na(object@C.ld)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Herdan's C",
				value=object@C.ld
			))
	} else {}

	if(!sum(!is.na(object@R.ld)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Root TTR",
				value=object@R.ld
			))
	} else {}

	if(!sum(!is.na(object@CTTR)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="CTTR",
				value=object@CTTR
			))
	} else {}

	if(!sum(!is.na(object@U.ld)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Uber index",
				value=object@U.ld
			))
	} else {}

	if(!sum(!is.na(object@S.ld)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Summer",
				value=object@S.ld
			))
	} else {}

	if(!sum(!is.na(object@K.ld)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Yule's K",
				value=object@K.ld
			))
	} else {}

	if(!sum(!is.na(object@Maas)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Maas a",
				value=object@Maas
			))
	} else {}

	if(!sum(!is.na(object@lgV0)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="Maas lgV0",
				value=object@lgV0
			))
	} else {}

	if(!sum(!is.na(object@HDD)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="HD-D (vocd-D)",
				value=object@HDD$HDD
			))
	} else {}

	if(!sum(!is.na(object@MTLD)) == 0){
		summary.table <- add.to.sumtab(summary.table, adds=list(
				index="MTLD",
				value=object@MTLD$MTLD
			))
	} else {}

	# remove the empty first row
	summary.table <- summary.table[-1,]
	dimnames(summary.table)[[1]] <- c(1:dim(summary.table)[[1]])

	return(summary.table)
})
