print.lpi <-
function(x, ...){
	cat('\nLinkage.Patterns\n')
	print(x$Linkage.Patterns,max.levels=0, ...)
	cat('\nOccurrences\n', ...)
	print(x$Occurrences,max.levels=0, ...)
	cat('\nSources\n')
	print(x$Sources,max.levels=0, ...)
}

