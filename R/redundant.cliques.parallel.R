redundant.cliques.parallel <-
function(index,mcliques,mode='vertex'){
	if (length(mcliques[[index]])>1){
		a <- as.set(mcliques[[index]])
		b <- set()
		if (index>1){
			for (item in lapply(mcliques[1:index-1],as.set)){b <- set_union(b,set(item))}
			return(redundant.cliques(a,b,mode=mode))
		}
	}
	return(as.bigz(0))
}

