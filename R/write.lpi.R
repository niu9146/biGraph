write.lpi <-
function(input,filename){
	if (class(input)=='lpi'){
		class(input) <- 'data.frame'
		if (length(input)==3) write.table(input,filename,sep='\t',quote=FALSE,row.names=FALSE,col.names=c('Linkage.Patterns','Occurrences','Sources'))
		else	write.table(input,filename,sep='\t',quote=FALSE,row.names=FALSE,col.names=c('Linkage.Patterns','Occurrences'))
	}
	else {
		cat("Input object 'input' is not of class lpi", input)
	}
}

