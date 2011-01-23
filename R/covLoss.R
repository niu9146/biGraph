covLoss <-
function(g,matrix,parall=FALSE,preschedule=FALSE,cores=1,verbose=FALSE) {
	if (!parall){
		## Sequential calculation
		if (verbose) cat(" in sequential mode\n")
		for (e in 0:(length(E(g))-1)) {
			v1 <- get.edge(g,e)[[1]]
			v2 <- get.edge(g,e)[[2]]
			t <- 0
			count <- 0
			for (c in 1:ncol(matrix)) {
				if (matrix[v1+1,c]==1) {
					if (matrix[v2+1,c]==1) {
						t <- sum(t,2/colSums(matrix)[[c]])
						count <- sum(count,1)
					}
				}
			}	
			E(g)[e]$covLoss <- 1-(t/count)
		}
	}
	else{
		## Parallel calculation
		if (verbose) cat(" in parallel mode\n")
		E(g)$covLoss <- unlist(mclapply(0:(length(E(g))-1),covLoss.single.edge,g,matrix,mc.preschedule=preschedule,mc.cores=cores))
	}
	return(g)
}

