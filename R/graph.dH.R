graph.dH <-
function(g,matrix,mlps=0,logB=exp(1),parall=FALSE,preschedule=TRUE,cores=1,verbose=FALSE) {
	## ToDo: preschedule=FALSE produces "Error in unserialize(r) : unknown input format" error
	t <- c()
	cat('\n')
	## determine uncertainty before projection
	for (i in 1:length(colnames(matrix))) {t <- c(t,paste(matrix[,i],collapse=""))}
	tfreq <- table(t) / length(t)
	g$H.before <- -sum(tfreq*log(tfreq,logB))

	## determine number of cliques and then uncertainty after projection
	numberOfCliques <- as.bigz(0)

	## get maximal cliques
	if (verbose) cat('\t-> determining maximal cliques ... ')
	maxCliques <- maximal.cliques(g)
	g$Clique.number <- max(unlist(lapply(maxCliques,length)))
	if (verbose) cat(length(maxCliques),'maximal cliques, clique number: ',g$Clique.number)
	## calculate number of cliques within all maximal cliques, remove redundancies
	if (verbose) cat('\n\t-> resolving overlap')
	if (parall){
		if (verbose) cat(' ... in parallel\n')
		## calculate number of subcliques for all maximal cliques
		noc_list <- mclapply(sapply(maxCliques,length),number.subcliques,mode='clique',mc.preschedule=TRUE,mc.cores=cores)
		## calculate number of redundant subcliques for all maximal cliques
		norc_list <- mclapply(1:length(maxCliques),redundant.cliques.parallel,maxCliques,mode='clique',mc.preschedule=TRUE,mc.cores=cores)
		## subtract redundant subcliques
		noc_list <- mclapply(1:length(noc_list), function(i) sub.bigz(noc_list[[i]],norc_list[[i]]),mc.preschedule=TRUE,mc.cores=cores)
	}
	else{
		if (verbose) cat(' ... sequentually\n')
		## calculate number of subcliques for all maximal cliques
		noc_list <- lapply(sapply(maxCliques,length),number.subcliques,mode='clique')
		## calculate number of redundant subcliques for all maximal cliques
		norc_list <- lapply(1:length(maxCliques),redundant.cliques.parallel,maxCliques,mode='clique')
		## subtract redundant subcliques
		noc_list <- lapply(1:length(noc_list), function(i) sub.bigz(noc_list[[i]],norc_list[[i]]))
	}
	
	## collapse number of non-redundant subcliques
	numberOfCliques <- collapse.bigzList(noc_list)

	## add special subcliques
	if (mlps==0) {
		## count vertices that are not connected in the bipartite graph if allowed size of linkage profiles is 0
		numberOfCliques <- sum.bigz(numberOfCliques,length(which(degree(g)==0)))
	}
	if (mlps<2) {
		## add number of linkage profiles of size 1 if allowed size of linkage profiles is 0 or 1
		numberOfCliques <- sum.bigz(numberOfCliques,length(V(g)))
	}
	g$H.after <- log.bigz(as.character(numberOfCliques),base=logB)
	if (g$H.after==(-Inf)) {
		g$H.after <- 0
	}
	g$H.delta <- g$H.after - g$H.before
	return(g)
}

