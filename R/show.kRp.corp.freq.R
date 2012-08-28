#' @aliases show,kRp.corp.freq-method
#' @include show.kRp.lang.R
#' @rdname show-methods
setMethod("show", signature(object="kRp.corp.freq"), function(object){
	show(slot(object, "words"))
})
