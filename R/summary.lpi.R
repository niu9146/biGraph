summary.lpi <-
function(object, ...){
	no_lp <- length(object$Linkage.Patterns)
	lp_sizes_summary <- summary((unlist(lapply(strsplit(as.character(object$Linkage.Patterns),split=';'),length))), ...)
	occurrences_summary <- summary(object$Occurrences, ...)
	source_sizes_summary <- summary((unlist(lapply(strsplit(as.character(object$Sources),split=';'),length))), ...)
	res <- list(nlp=no_lp,lp_sum=lp_sizes_summary,occ_sum=occurrences_summary,source_sum=source_sizes_summary)
	class(res) <- 'summary.lpi'
	res
}

