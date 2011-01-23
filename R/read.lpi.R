read.lpi <- 
function(filename,directed_edges=FALSE,prefix='D'){
	lpi <- read.table(filename,header=TRUE)
	class(lpi) <- 'lpi'
	## transform linkage pattern information into bipartite graph
	elc <- c()
	vc <- 0
	## 'basic' and 'minimal' lp information, create generic names for omitted vertex set
	if (length(lpi)==2){
		for (entry in 1:length(lpi$Occurrences)){
			for (s in 1:lpi$Occurrences[entry]){
				vc <- vc + s
				for (b in strsplit(as.character(lpi$Linkage.Patterns[entry]),';')[[1]]){
					elc <- c(elc,paste(prefix,vc,sep=''),b)	## add one edge to edgelist
				}
			}
		}
	}
	## 'full'information given
	else if (length(lpi)==3){
		for (entry in 1:length(lpi$Occurrences)){
			sids <- strsplit(as.character(lpi$Sources[entry]),';')[[1]]
			for (s in 1:lpi$Occurrences[entry]){
				## add edges to edgelist
				for (b in strsplit(as.character(lpi$Linkage.Patterns[entry]),';')[[1]]){
					elc <- c(elc,sids[s],b)
				}
			}
		}
	}
	return(list(lpi=lpi,graph=graph.edgelist(matrix(elc,nc=2,byrow=TRUE),directed=directed_edges)))
 }