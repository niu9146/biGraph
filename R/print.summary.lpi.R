print.summary.lpi <-
function(x,...){
	cat("\nNumber of linkage patterns: ",x$nlp)
	cat("\n\nLinkage Pattern Sizes\n\n")
	print(x$lp_sum, ...)
	cat("\n\nOccurences\n\n")
	print(x$occ_sum, ...)
	cat("\n\nSources\n\n")
	print(x$source_sum, ...)
}

