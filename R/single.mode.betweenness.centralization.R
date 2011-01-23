single.mode.betweenness.centralization <-
function(g){
	if (!is.null(V(g)$type)){
		# determine denominators for both vertex subsets
		if (length(V(g)[type==FALSE])<length(V(g)[type==TRUE])){
			denom_TRUE <- 2*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1)
		}
		else{
			denom_TRUE <- (length(V(g)[type==TRUE])-1) * ( 0.5*length(V(g)[type==FALSE])*(length(V(g)[type==FALSE])-1) + 0.5*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==TRUE])-2) + (length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1) )
		}
		if (length(V(g)[type==TRUE])<length(V(g)[type==FALSE])){
			denom_FALSE <- 2*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-1)
		}
		else{
			denom_FALSE <- (length(V(g)[type==FALSE])-1) * ( 0.5*length(V(g)[type==TRUE])*(length(V(g)[type==TRUE])-1) + 0.5*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==FALSE])-2) + (length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-1) )
		}
		
		# determine raw betweenness centrality scores from igraph
		V(g)$betweenness.raw <- betweenness(g,directed=FALSE)
		# get maximum scores
		max_c_TRUE <- max(V(g)[type==TRUE]$betweenness.raw)
		max_c_FALSE <- max(V(g)[type==FALSE]$betweenness.raw)

		# determine centralization for TRUE vertex subset
		g$single.mode.betweenness.centralization.true <-sum(max_c_TRUE - V(g)[type==TRUE]$betweenness.raw)/denom_TRUE
		# determine centralization for FALSE vertex subset
		g$single.mode.betweenness.centralization.false <-sum(max_c_FALSE - V(g)[type==FALSE]$betweenness.raw)/denom_FALSE
		# return both values as list
		return(list("Single.Mode.Betweenness.Centralization.TRUE"=g$single.mode.betweenness.centralization.true,"Single.Mode.Betweenness.Centralization.FALSE"=g$single.mode.betweenness.centralization.false))
	}
	else {
		# boolean vertex attribute 'type' is required
		cat("vertex attribute <type> is missing")
	}
}