get.Hafter.vertex <-
function(k,g,mlps=0,logB=exp(1)){
	numberOfCliques <- as.bigz(0)
	subgraph <- graph.neighborhood(g,order=1,nodes=k-1)[[1]]
	subgraph_maxCliques <- maximal.cliques(subgraph)
	maxCliques_done <- set()
	v <- V(subgraph)[id==V(g)[k-1]$id][[1]]
	for (i in 1:length(subgraph_maxCliques)) {
		## picking next maximal clique
		if (length(as.set(subgraph_maxCliques[[i]]))>1){
			## determine and add number of cliques for maximal clique
			numberOfCliques <- sum.bigz(numberOfCliques, as.bigz(number.subcliques(length(as.set(subgraph_maxCliques[[i]])),mode='vertex')))
			## subtracting number of redundant cliques from total number of cliques
			numberOfCliques <- sub.bigz(numberOfCliques,redundant.cliques(as.set(subgraph_maxCliques[[i]]),maxCliques_done,mode='vertex'))
			## Check set for following maximal cliques
			maxCliques_done <- set_union(maxCliques_done,set(as.set(subgraph_maxCliques[[i]])))
		}
	}
	## adding additional cliques for loops or unlinked vertices
	if (mlps==0) {
		if (numberOfCliques==0) {
			numberOfCliques <- sum.bigz(numberOfCliques,2)
		}
		else {
			numberOfCliques <- sum.bigz(numberOfCliques,1)
		}
	}
	if (mlps==1) {
		numberOfCliques <- sum.bigz(numberOfCliques,1)
	}
	after <- log.bigz(numberOfCliques,base=logB)

	return(after)
}

