get.weighted.projection <-
function(g,vType=FALSE,mode='shared-neighbours', m=NULL){
	bp <- is.bipartite(g)
	if (bp$res){
		V(g)$type <- bp$type
		if (vType) proj <- bipartite.projection(g)[[2]]
		else proj <- bipartite.projection(g)[[1]]
		V(proj)$name <- V(g)[type==vType]$name
		## number of shared vertices as edge weights 
		if (mode=='shared-neighbours'){
			if (is.null(m))	incidenceMatrix <- get.incidence(g)	## TODO: might need some more testing if type and vType match
			else incidenceMatrix <- m
			if (!vType){
				## off-diagonal elements represent edge weights, diagonal elements vertex weights (currently unused)
				m <- incidenceMatrix %*% t(incidenceMatrix)
			}
			else {
				m <- t(incidenceMatrix) %*% incidenceMatrix
			}
			## assign edge weights from matrix m
			if (length(E(proj))>0){
				for (i in 0:(length(E(proj))-1)){
					E(proj)[i]$weight <- m[V(proj)[get.edge(proj,i)[[1]]]$name, V(proj)[get.edge(proj,i)[[2]]]$name]
				}
			}
			return(proj)
		}
		## Newman2001
		if (mode=='newman'){
			if (is.null(m))	incidenceMatrix <- get.incidence(g)	## TODO: might need some more testing if type and vType match
			else incidenceMatrix <- m
			if (!vType){
				## off-diagonal elements represent edge weights, diagonal elements vertex weights (currently unused)
				incidenceMatrix <- t(incidenceMatrix)
				if (length(c(which(rowSums(incidenceMatrix)==1)))!=0){
					m2 <- incidenceMatrix[c(which(rowSums(incidenceMatrix)==1))*(-1),]
				}
				else{
					m2 <- incidenceMatrix
				}
				m <- t(m2) %*% (m2*(1/(rowSums(m2)-1)))
			}
			else {	
				if (length(c(which(rowSums(incidenceMatrix)==1)))!=0){
					m2 <- incidenceMatrix[c(which(rowSums(incidenceMatrix)==1))*(-1),]
				}
				else{
					m2 <- incidenceMatrix
				}
				m <- t(m2) %*% (m2*(1/(rowSums(m2)-1)))
			}
			## assign edge weights from matrix m
			if (length(E(proj))>0){
				for (i in 0:(length(E(proj))-1)){
					E(proj)[i]$weight <- m[V(proj)[get.edge(proj,i)[[1]]]$name, V(proj)[get.edge(proj,i)[[2]]]$name]
				}
			}
			return(proj)
		}
		## generic, just take provided matrix as weight
		else if (mode=='pass-through'){
			if (is.null(m)){
				cat('Weight matrix is missing')
				return(NULL)
			}
			else {
				## assign edge weights from provided matrix
				if (length(E(proj))>0){
					for (i in 0:(length(E(proj))-1)){
						E(proj)[i]$weight <- m[V(proj)[get.edge(proj,i)[[1]]]$name, V(proj)[get.edge(proj,i)[[2]]]$name]
					}
				}
				return(proj)
			}
		}
	}
	else cat('Graph is not bipartite !')
}

