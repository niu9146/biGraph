single.mode.degree.centralization <-
function(g){
	if (!is.null(V(g)$type)){
		V(g)$degree <- degree(g)
		# determine maximum degrees for each vertex set
		max_d_TRUE <- max(V(g)[type==TRUE]$degree)
		max_d_FALSE <- max(V(g)[type==FALSE]$degree)
		# determine centralization for TRUE vertex subset
		g$single.mode.degree.centralization.true <-sum(max_d_TRUE - V(g)[type==TRUE]$degree)/((length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1))
		# determine centralization for FALSE vertex subset
		g$single.mode.degree.centralization.false <-sum(max_d_FALSE - V(g)[type==FALSE]$degree)/((length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1))
		# return both values as list
		return(list("Single.Mode.Degree.Centralization.TRUE"=g$single.mode.degree.centralization.true,"Single.Mode.Degree.Centralization.FALSE"=g$single.mode.degree.centralization.false))
	}
	else {
		# boolean vertex attribute 'type' is required
		cat("vertex attribute <type> is missing")
	}
}