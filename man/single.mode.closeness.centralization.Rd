\name{single.mode.closeness.centralization}
\alias{single.mode.closeness.centralization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single mode closeness centralization for bipartite graphs
}
\description{
Measures the closeness centralization for each vertex subset in a bipartite graph.
}
\usage{
single.mode.closeness.centralization(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph, undirected
}
}
\details{
Graph centralization measures the extent to which a graph has a highly central vertex around which highly peripheral vertices gather.  Single mode centralization for bipartite graphs measures the extent to which vertices in one vertex subset are central relative only to other vertices in the same subset.  This function is based on the closeness centrality of vertices.  The boolean vertex attribute \code{type} has to be provided.  \code{\link[igraph]{is.bipartite}} can be used to generate it, although care has to be taken if graph consists of multiple components.
}
\value{
List with centralization scores for both vertex subsets.

   \item{\code{Single.Mode.Closeness.Centralization.TRUE} }{single mode centralization for vertices in subset \code{TRUE}}
   \item{\code{Single.Mode.Closeness.Centralization.FALSE} }{single mode centralization for vertices in subset \code{FALSE}}
}
\references{
Borgatti, S. P. and Everett, M. G. (1997) Network analysis of 2--mode data. Social Networks \bold{19}, 243--269.
}
\author{
Ingo Vogt <\email{bigraph.vogt@gmail.com}>
}
\note{
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[biGraph]{bipartite.betweenness.centrality}}, \code{\link[biGraph]{bipartite.closeness.centrality}}, \code{\link[biGraph]{degree.centrality}}, \code{\link[biGraph]{single.mode.betweenness.centralization}}, \code{\link[biGraph]{single.mode.closeness.centralization}}, \code{\link[biGraph]{single.mode.degree.centralization}}, \code{\link[igraph]{is.bipartite}}, \code{\link[igraph]{betweenness}}
}
\examples{
data(g)

## Compare single mode centralization measures
single.mode.betweenness.centralization(g)
single.mode.closeness.centralization(g)
single.mode.degree.centralization(g)


## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ biGraph }
\keyword{ centrality }
\keyword{ centralization }
\keyword{ bipartite }

