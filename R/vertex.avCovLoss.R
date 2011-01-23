vertex.avCovLoss <-
function(g) {
	for (v in V(g)){
		g <- set.vertex.attribute(g,'avCovLoss',index=v,sum(E(g)[E(g)[to(v)]]$covLoss)/length(E(g)[E(g)[to(v)]]$covLoss))
	}
	## vertices with no edges have no Cov Loss Information, and are not counted
	V(g)[V(g)$avCovLoss==Inf]$avCovLoss <- NA
	return(g)
}

