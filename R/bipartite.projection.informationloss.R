bipartite.projection.informationloss <-
function(input_graph,vType=c(FALSE,TRUE),measures=c("graph.dH","vertex.dH","edge.dH","covLoss"),mlps=0,logB=exp(1),parall=FALSE,preschedule=FALSE,cores=1,verbose=FALSE) {
	## check if input graph is bipartite
	bipartite <- is.bipartite(input_graph)
	if (bipartite[[1]]==TRUE){
		V(input_graph)$type <- bipartite$type
		## get incidence matrix for input graph
		incMatrix <- get.incidence(input_graph,type=bipartite$type)
		## if there is no vertex attribute 'id' in the input graph, take the numbering given by igraph object
		if (length(set_intersection(as.set(list.vertex.attributes(input_graph)),as.set("id")))==0){
			V(input_graph)$id <- V(input_graph)
		}
		## projecting input graph for each selected vertex set
		projected_graphs <- list()
		for (vtype in vType) {
			if (verbose) cat("\nVertex set: ",vtype,"\n")
			if (vtype==FALSE){
				bpp <- bipartite.projection(input_graph)$proj1
			}
			else {
				if (vtype==TRUE){
					bpp <- bipartite.projection(input_graph)$proj2
				}
			}
			if (verbose) cat(" ",length(V(bpp))," Vertices\n ",length(E(bpp))," Edges\n")
			## copy 'id' attrbutes from input graph to projected graph for remaining vertex set
			V(bpp)$id <- V(input_graph)[type==vtype]$id
			V(bpp)$name <- V(input_graph)[type==vtype]$name
			## calculate information loss according to selected measures
			for (measure in measures) {
				## change in uncertainty for entire graph
				if ((measure=="complete")|(measure=="graph.dH")) {
					if (verbose) cat('\n >> Calculating graph-based information loss')
					if (vtype==TRUE) {
						bpp <- graph.dH(bpp,t(incMatrix),mlps=mlps,logB=logB,parall=parall,preschedule=preschedule,cores=cores,verbose=verbose)
					}
					else {
						bpp <- graph.dH(bpp,incMatrix,mlps=mlps,logB=logB,parall=parall,preschedule=preschedule,cores=cores,verbose=verbose)
					}
				}
				## change in uncertainty for each vertex
				if ((measure=="complete")|(measure=="vertex.dH")) {
					if (verbose) cat('\n >> Calculating vertex-based information loss')
					if (vtype==TRUE) {
						bpp <- vertex.dH(bpp,t(incMatrix),mlps,logB,parall=parall,preschedule=preschedule,cores=cores,verbose=verbose)
					}
					else {
						bpp <- vertex.dH(bpp,incMatrix,mlps,logB,parall=parall,preschedule=preschedule,cores=cores,verbose=verbose)
					}
				}
				## change in uncertainty for each edge
				if ((measure=="complete")|(measure=="edge.dH")) {
					if (verbose) cat('\n >> Calculating edge-based information loss')
					if (vtype==TRUE) {
						bpp <- edge.dH(bpp,t(incMatrix),mlps,logB,parall=parall,preschedule=preschedule,cores=cores,verbose=verbose)
					}
					else {
						bpp <- edge.dH(bpp,incMatrix,mlps,logB,parall=parall,preschedule=preschedule,cores=cores,verbose=verbose)
					}
				}
				## loss of coverage for each edge
				if ((measure=="complete")|(measure=="covLoss")) {
					if (verbose) cat('\n >> Calculating edge-based loss of coverage')
					if (vtype==TRUE) {
						bpp <- covLoss(bpp,t(incMatrix),parall=parall,verbose=verbose)
					}
					else {
						bpp <- covLoss(bpp,incMatrix,parall=parall,verbose=verbose)
					}
					if (verbose) cat('\n >> Calculating average loss of coverage for vertices\n\n')
					bpp <- vertex.avCovLoss(bpp)
				}
			}
			projected_graphs[[length(projected_graphs)+1]] <- bpp
		}
		return(projected_graphs)
	}
	else {
		cat("Input graph is not bipartite")
	}
}

