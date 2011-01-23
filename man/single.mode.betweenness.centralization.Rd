\name{single.mode.betweenness.centralization}
\alias{single.mode.betweenness.centralization}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Single mode betweenness centralization for bipartite graphs
}
\description{
Measures the betweenness centralization for each vertex subset in a bipartite graph.
}
\usage{
single.mode.betweenness.centralization(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph, undirected
}
}
\details{
Graph centralization measures the extent to which a graph has a highly central vertex around which highly peripheral vertices gather.  Single mode centralization for bipartite graphs measures the extent to which vertices in one vertex subset are central relative only to other vertices in the same subset.  This function is based on the betweenness centrality of vertices.  The boolean vertex attribute \code{type} has to be provided.  \code{\link[igraph]{is.bipartite}} can be used to generate it, although care has to be taken if graph consists of multiple components.
}
\value{
List with centralization scores for both vertex subsets.

   \item{\code{Single.Mode.Betweenness.Centralization.TRUE} }{single mode centralization for vertices in subset \code{TRUE}}
   \item{\code{Single.Mode.Betweenness.Centralization.FALSE} }{single mode centralization for vertices in subset \code{FALSE}}
}
\references{
Borgatti, S. P. and Everett, M. G. (1997) Network analysis of 2--mode data. Social Networks \bold{19}, 243--269.
}
\author{
Ingo Vogt <\email{bigraph.vogt@gmail.com}>
}
\note{
Currently, all edges are treated as undirected.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[biGraph]{bipartite.betweenness.centrality}}, \code{\link[biGraph]{bipartite.closeness.centrality}}, \code{\link[biGraph]{degree.centrality}}, \code{\link[biGraph]{single.mode.betweenness.centralization}}, \code{\link[biGraph]{single.mode.closeness.centralization}}, \code{\link[biGraph]{single.mode.degree.centralization}},  \code{\link[igraph]{is.bipartite}},  \code{\link[igraph]{betweenness}}
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
		# determine denominators for both vertex subsets
		if (length(V(g)[type==FALSE])<length(V(g)[type==TRUE])){
			denom_TRUE <- 2*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1)
		}
		else{
			denom_TRUE <- (length(V(g)[type==TRUE])-1) * ( 0.5*length(V(g)[type==FALSE])*(length(V(g)[type==FALSE])-1) + 0.5*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==TRUE])-2) + (length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1) )
		}
		if (length(V(g)[type==TRUE])<length(V(g)[type==FALSE])){
			denom_FALSE <- 2*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-1)
		}
		else{
			denom_FALSE <- (length(V(g)[type==FALSE])-1) * ( 0.5*length(V(g)[type==TRUE])*(length(V(g)[type==TRUE])-1) + 0.5*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==FALSE])-2) + (length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-1) )
		}
		
		# determine raw betweenness centrality scores from igraph
		V(g)$betweenness.raw <- betweenness(g,directed=FALSE)
		# get maximum scores
		max_c_TRUE <- max(V(g)[type==TRUE]$betweenness.raw)
		max_c_FALSE <- max(V(g)[type==FALSE]$betweenness.raw)

		# determine centralization for TRUE vertex subset
		g$single.mode.betweenness.centralization.true <-sum(max_c_TRUE - V(g)[type==TRUE]$betweenness.raw)/denom_TRUE
		# determine centralization for FALSE vertex subset
		g$single.mode.betweenness.centralization.false <-sum(max_c_FALSE - V(g)[type==FALSE]$betweenness.raw)/denom_FALSE
		# return both values as list
		return(list("Single.Mode.Betweenness.Centralization.TRUE"=g$single.mode.betweenness.centralization.true,"Single.Mode.Betweenness.Centralization.FALSE"=g$single.mode.betweenness.centralization.false))
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

