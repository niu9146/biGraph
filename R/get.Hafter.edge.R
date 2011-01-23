get.Hafter.edge <- 
function(e,g,mlps=0,logB=exp(1)){
	numberOfCliques <- as.bigz(0)
	
	k <- get.edge(g,e)[1]
	kk <- get.edge(g,e)[2]
	
	subgraph <- subgraph(g,V(g)[nei(k:kk)])
	subgraph_maxCliques <- maximal.cliques(subgraph)
	v1 <- V(subgraph)[id==V(g)[k]$id][[1]]
	v2 <- V(subgraph)[id==V(g)[kk]$id][[1]]
	maxCliques_done <-set()
	for (i in 1:length(subgraph_maxCliques)) {
		if ((length(set_intersection(as.set(subgraph_maxCliques[[i]]),set(v1,v2)))>1)&&(length(as.set(subgraph_maxCliques[[i]]))>1)) {
			## add number of cliques associated to current maximal clique
			numberOfCliques <- sum.bigz(numberOfCliques,as.bigz(number.subcliques(length(as.set(subgraph_maxCliques[[i]])),mode='edge')))	
			## subtract multiple counts with respect to predecessing maximal clqiues
			numberOfCliques <- sub.bigz(numberOfCliques,redundant.cliques(as.set(subgraph_maxCliques[[i]]),maxCliques_done,mode='edge'))
			maxCliques_done <- set_union(maxCliques_done,set(as.set(subgraph_maxCliques[[i]])))
		}
	}
	after <- log.bigz(numberOfCliques,base=logB)
	return(after)
}

