bipartite.graph.density <-
function(g){
	if (!is.null(V(g)$type)){
		# return value as list item
		if (!is.directed(g)){
			return(list("Density"=length(E(g))/(length(which(V(g)$type))*length(which(!V(g)$type)))))
		}
		else{
			return(list("Density"=length(E(g))/(2*length(which(V(g)$type))*length(which(!V(g)$type)))))
		}
	}
	else {
		# boolean vertex attribute 'type' is required
		cat("vertex attribute <type> is missing")
	}
}