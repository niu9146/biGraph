\name{bipartite.betweenness.centrality}
\alias{bipartite.betweenness.centrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Betweenness centrality for bipartite graphs
}
\description{
Calculates betweenness centrality scores for vertices in bipartite graphs.
}
\usage{
bipartite.betweenness.centrality(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph
}
}
\details{
The betweenness centrality of a vertex  may be roughly defined as the number of geodesic paths that pass through a given vertex, weighted inversely by the total number of equivalent paths between the same two vertices, including those that do not pass through the given vertex.  This function is the adaption to bipartite graphs as presented in Borgatti and Everett (1997).  The boolean vertex attribute \code{type} has to be provided.  \code{\link[igraph]{is.bipartite}} can be used to generate it, although care has to be taken if graph consists of multiple components.
}
\value{
List with score vector as only item.

   \item{\code{Bipartite.Betweenness.Centrality} }{centrality scores for vertices}
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
\code{\link[biGraph]{degree.centrality}}, \code{\link[biGraph]{bipartite.closeness.centrality}}, \code{\link[biGraph]{single.mode.betweenness.centralization}}, \code{\link[igraph]{is.bipartite}},  \code{\link[igraph]{betweenness}}
}
\examples{
data(g)

## Calculate betweenness centrality for bipartite graph
V(g)$bipartite.betweenness.centrality <- bipartite.betweenness.centrality(g)[[1]]
V(g)$bipartite.betweenness.centrality
V(g)[bipartite.betweenness.centrality==max(bipartite.betweenness.centrality)]

## Calculate normalized betweenness centrality for monopartite gene projection
gene_projection <- bipartite.projection.informationloss(g,vType=FALSE,measures='')[[1]]
V(gene_projection)$betweenness.centrality <- (2*betweenness(gene_projection))/(length(V(gene_projection))*length(V(gene_projection))-3*length(V(gene_projection))+2)
plot(V(g)[type==FALSE]$bipartite.betweenness.centrality,V(gene_projection)$betweenness.centrality,xlim=c(0,0.7),ylim=c(0,0.7),xlab='bipartite graph',ylab='monopartite projection',main='betweenness centrality of genes')
\dontshow{lines(seq(0,0.7,0.1),seq(0,0.7,0.1))}

## The function is currently defined as
function(g){
	if (!is.null(V(g)$type)){
		# determine maximal raw scores for both vertex subsets
		if (length(V(g)[type==FALSE])<length(V(g)[type==TRUE])){
			mrs_TRUE <- 2*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-1)
		}
		else{
			mrs_TRUE <- 0.5*length(V(g)[type==FALSE])*(length(V(g)[type==FALSE])-1)+0.5*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==TRUE])-2)+(length(V(g)[type==FALSE])-1)*(length(V(g)[type==TRUE])-1)
		}
		if (length(V(g)[type==TRUE])<length(V(g)[type==FALSE])){
			mrs_FALSE <- 0.5*length(V(g)[type==TRUE])*(length(V(g)[type==TRUE])-1)+0.5*(length(V(g)[type==FALSE])-1)*(length(V(g)[type==FALSE])-2)+(length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1)
			
		}
		else{
			mrs_FALSE <- 2*(length(V(g)[type==TRUE])-1)*(length(V(g)[type==FALSE])-1)
		}
		
		# get raw betweenness centrality scores from igraph
		betweenness_rs <- betweenness(g,directed=FALSE)
		# "bipartite" normalization of scores
		for (i in V(g)){
			if (V(g)[i]$type==TRUE){
				V(g)[i]$betweenness.centrality <- betweenness_rs[i+1]/mrs_TRUE
			}
			else{
				V(g)[i]$betweenness.centrality <- betweenness_rs[i+1]/mrs_FALSE
			}
		}
		# return value as list
		return(list("Bipartite.Betweenness.Centrality"=V(g)$betweenness.centrality))
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

