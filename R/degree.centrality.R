degree.centrality <-
function(g,loops=FALSE){
	# if boolean vertex attribute <type> is present, calculate bipartite degree centrality, otherwise monopartite degree centrality
	if (!is.null(V(g)$type)){
		for (i in V(g)){
			V(g)[i]$degree.centrality <- degree(g,v=i)/length(V(g)[type==!V(g)[i]$type])
		}
		# return value vector as list item
		return(list("Bipartite.Degree.Centrality"=V(g)$degree.centrality))
	}
	else {
		for (i in V(g)){
			if (!loops){
				V(g)[i]$degree.centrality <- degree(g,v=i,loops=FALSE)/(length(V(g))-1)
			}
			else{
				V(g)[i]$degree.centrality <- degree(g,v=i,loops=TRUE)/(length(V(g)))
			}
		}
		# return value vector as list item
		return(list("Monopartite.Degree.Centrality"=V(g)$degree.centrality))
	}
}