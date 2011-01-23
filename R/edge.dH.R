edge.dH <-
function(g,matrix,mlps=0,logB=exp(1),parall=FALSE,preschedule=FALSE,cores=1,verbose=FALSE) {
	## Calculate uncertainty before projection
	for (e in 0:(length(E(g))-1)) {
		v1 <- get.edge(g,e)[[1]]
		v2 <- get.edge(g,e)[[2]]
		t <- c()	
		for (c in 1:ncol(matrix)) {
			if (matrix[v1+1,c]==1) {
				if (matrix[v2+1,c]==1) {
					t <- c(t,paste(matrix[,c],collapse=""))
				}
			}
		}	
		tfreq <- table(t) / length(t)
		E(g)[e]$H.before <- -sum(tfreq*log(tfreq,base=logB))
	}

	## calculate number of cliques per edge and from this the uncertainty after projection
	if (parall){
		if (verbose) cat(' in parallel mode\n')
		Hafter <- unlist(mclapply(0:(length(E(g))-1),get.Hafter.edge,g,mlps,logB,parall,mc.preschedule=preschedule,mc.cores=cores))
	}
	else{
		if (verbose) cat(' in sequential mode\n')
		Hafter <- unlist(lapply(0:(length(E(g))-1),get.Hafter.edge,g,mlps,logB))
		
	}
	for (k in 1:length(E(g))) {
		E(g)[k-1]$H.after <- Hafter[k]
	}

	## Uncertainty after projection
	E(g)[H.after==(-Inf)]$H.after <- NA
	E(g)[H.after==(Inf)]$H.after <- NA

	## Change in uncertainty
	E(g)$H.delta <- E(g)$H.after - E(g)$H.before

	## Return
	return(g)
}

