\name{bipartite.closeness.centrality}
\alias{bipartite.closeness.centrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Closeness centrality for bipartite graphs
}
\description{
Calculates closeness centrality scores for vertices in bipartite graphs.
}
\usage{
bipartite.closeness.centrality(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph
}
}
\details{
The closeness centrality of a vertex is inversely proportional to the total geodesic distance to all other vertices.  This function is the adaption to bipartite graphs as presented in Borgatti and Everett (1997).  The boolean vertex attribute \code{type} has to be provided.  \code{\link[igraph]{is.bipartite}} can be used to generate it, although care has to be taken if graph consists of multiple components.
}
\value{
List with score vector as only item.

   \item{\code{Bipartite.Closeness.Centrality} }{centrality scores for vertices}
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
\code{\link[biGraph]{degree.centrality}}, \code{\link[biGraph]{bipartite.betweenness.centrality}}, \code{\link[biGraph]{single.mode.closeness.centralization}}, \code{\link[igraph]{is.bipartite}}, \code{\link[igraph]{closeness}}
}
\examples{
data(g)

## Calculate closeness centrality for bipartite graph
V(g)$bipartite.closeness.centrality <- bipartite.closeness.centrality(g)[[1]]
V(g)$bipartite.closeness.centrality
V(g)[bipartite.closeness.centrality==max(bipartite.closeness.centrality)]

## Calculate closeness centrality for monopartite gene projection
gene_projection <- bipartite.projection.informationloss(g,vType=FALSE,measures='')[[1]]
V(gene_projection)$closeness.centrality <- closeness(gene_projection)
plot(V(g)[type==FALSE]$bipartite.closeness.centrality,V(gene_projection)$closeness.centrality,xlim=c(0,0.9),ylim=c(0,0.9),xlab='bipartite graph',ylab='monopartite projection',main='Closeness centrality of genes')
\dontshow{lines(seq(0,0.9,0.1),seq(0,0.9,0.1))}

## The function is currently defined as
function(g){
	if (!is.null(V(g)$type)){
		# determine maximal raw scores for both vertex subsets
		mrs_TRUE <- length(V(g)[type==FALSE]) + 2*length(V(g)[type==TRUE]) - 2
		mrs_FALSE <- length(V(g)[type==TRUE]) + 2*length(V(g)[type==FALSE]) - 2
		# get sum of all geodesic paths for each vertex
		rowsums_shortest_paths <- rowSums(shortest.paths(g))
		# "bipartite" normalization of scores
		for (i in V(g)){
			if (V(g)[i]$type==TRUE){
				V(g)[i]$closeness.centrality <- mrs_TRUE/rowsums_shortest_paths[i+1]
			}
			else{
				V(g)[i]$closeness.centrality <- mrs_FALSE/rowsums_shortest_paths[i+1]
			}
		}
		# return value as list
		return(list("Bipartite.Closeness.Centrality"=V(g)$closeness.centrality))
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
\keyword{ bipartite }

