single.mode.closeness.centralization <-
function(g){
	if (!is.null(V(g)$type)){
		# determine bipartite closeness centrality scores
		V(g)$Bipartite.Closeness.Centrality <- bipartite.closeness.centrality(g)[[1]]

		# get maximum scores
		max_c_TRUE <- max(V(g)[type==TRUE]$Bipartite.Closeness.Centrality)
		max_c_FALSE <- max(V(g)[type==FALSE]$Bipartite.Closeness.Centrality)

		# determine denominators for both vertex subsets
		if (length(V(g)[type==FALSE])<length(V(g)[type==TRUE])){
			denom_TRUE <- ((length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-2))/(2*length(V(g)[type==TRUE])-3) + ((length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-length(V(g)[type==FALSE])))/(length(V(g)[type==TRUE])+length(V(g)[type==FALSE])-2)
		}
		else{
			denom_TRUE <- ((length(V(g)[type==TRUE])-2)*(length(V(g)[type==TRUE])-1))/(2*length(V(g)[type==TRUE])-3)
		}
		if (length(V(g)[type==TRUE])<length(V(g)[type==FALSE])){
			denom_FALSE <- ((length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-2))/(2*length(V(g)[type==FALSE])-3) + ((length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-length(V(g)[type==TRUE])))/(length(V(g)[type==FALSE])+length(V(g)[type==TRUE])-2)
		}
		else{
			denom_FALSE <- ((length(V(g)[type==FALSE])-2)*(length(V(g)[type==FALSE])-1))/(2*length(V(g)[type==FALSE])-3)
		}
	
		# determine centralization for TRUE vertex subset
		g$single.mode.closeness.centralization.true <-sum(max_c_TRUE - V(g)[type==TRUE]$Bipartite.Closeness.Centrality)/denom_TRUE
		# determine centralization for FALSE vertex subset
		g$single.mode.closeness.centralization.false <-sum(max_c_FALSE - V(g)[type==FALSE]$Bipartite.Closeness.Centrality)/denom_FALSE
		# return both values as list
		return(list("Single.Mode.Closeness.Centralization.TRUE"=g$single.mode.closeness.centralization.true,"Single.Mode.Closeness.Centralization.FALSE"=g$single.mode.closeness.centralization.false))
	}
	else {
		# boolean vertex attribute 'type' is required
		cat("vertex attribute <type> is missing")
	}
}