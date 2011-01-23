covLoss.single.edge <-
function(edge,g,matrix){
	v1 <- get.edge(g,edge)[[1]]
	v2 <- get.edge(g,edge)[[2]]
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
	return(1-(t/count))
}

