lpi <-
function(g,vType=FALSE,mode='full'){
	bp <- is.bipartite(g)
	if (bp$res==TRUE){
		V(g)$type <- bp$type
		for (v in V(g)[type==vType]){
			V(g)[v]$lp <- paste(sort(V(g)[neighbors(g,v)]$name),sep='',collapse=';')
		}
		Linkage.Patterns <- unique(V(g)[type==vType]$lp)
		Sources <- c()
		if (mode=='minimal'){
			lpi_df <- data.frame(Linkage.Patterns,rep(1,length(Linkage.Patterns)))
			
		}
		else{
			Occurrences <- c()
			for (p in Linkage.Patterns){
				Sources <- c(Sources,paste(sort(V(g)[lp==p]$name),sep='',collapse=';'))
				Occurrences <- c(Occurrences,length(V(g)[(lp==p)&(!is.na(lp))]))
			}
			if (mode=='full'){
				lpi_df <- data.frame(Linkage.Patterns,Occurrences,Sources)
			}
			else if (mode=='basic'){
				lpi_df <- data.frame(Linkage.Patterns,Occurrences)
			}
		}
		class(lpi_df) <- 'lpi'
		return(lpi_df)
	}
	else {
		cat("Input graph is not bipartite")
	}
}

