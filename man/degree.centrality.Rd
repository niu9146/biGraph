\name{degree.centrality}
\alias{degree.centrality}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Degree centrality scores
}
\description{
Calculates degree centrality scores for vertices in mono- and bipartite graphs.
}
\usage{
degree.centrality(g,loops=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
graph, either mono- or bipartite
}
  \item{loops}{
boolean, whether or not self--loops are considered in monopartite graphs. Defaults to \code{FALSE}.
}
}
\details{
The degree centrality of a vertex can be defined as fraction of incident edges over the number of all possible edges.  This function is the adaption to bipartite graphs as presented in Borgatti and Everett (1997).  If graph vertices have boolean attribute \code{type}, centrality scores are normalized with respect to number of vertices in the other vertex subset.  If not, centrality scores are normalized with respect to all vertices.   \code{\link[igraph]{is.bipartite}} can be used to generate \code{type} attribute, although care has to be taken if graph consists of multiple components.
}
\value{
List with score vector as only item.

   \item{\code{Bipartite.Degree.Centrality} }{centrality scores if graph is bipartite}
   \item{\code{Monopartite.Degree.Centrality} }{centrality scores if graph is monopartite}
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
\code{\link[biGraph]{bipartite.closeness.centrality}}, \code{\link[biGraph]{bipartite.betweenness.centrality}}, \code{\link[biGraph]{single.mode.degree.centralization}}, \code{\link[igraph]{is.bipartite}}
}
\examples{
data(g)

## Calculate degree centrality for bipartite graph
V(g)$bipartite.degree.centrality <- degree.centrality(g)[[1]]
V(g)$bipartite.degree.centrality
V(g)[bipartite.degree.centrality==max(bipartite.degree.centrality)]

## Calculate degree centrality for monopartite disease projection
disease_projection <- bipartite.projection.informationloss(g,vType=TRUE,measures='')[[1]]
V(disease_projection)$degree.centrality <- degree.centrality(disease_projection)[[1]]
plot(V(g)[type==TRUE]$bipartite.degree.centrality,V(disease_projection)$degree.centrality,xlim=c(0,0.7),ylim=c(0,0.7),xlab='bipartite graph',ylab='monopartite projection',main='Degree centrality of diseases')
\dontshow{lines(seq(0,0.7,0.1),seq(0,0.7,0.1))}


## The function is currently defined as
function(g,loops=FALSE){
	# if boolean vertex attribute <type> is present, calculate bipartite degree centrality, otherwise monopartite degree centrality
	if (!is.null(V(g)$type)){
		for (i in V(g)){
			V(g)[i]$degree.centrality <- degree(g,v=i)/length(V(g)[type==!V(g)[i]$type])
		}
		# return value vector as list item
		return(list("Bipartite.Degree.Centrality"=V(g)$degree.centrality))
	}
	else {
		for (i in V(g)){
			if (!loops){
				V(g)[i]$degree.centrality <- degree(g,v=i,loops=FALSE)/(length(V(g))-1)
			}
			else{
				V(g)[i]$degree.centrality <- degree(g,v=i,loops=TRUE)/(length(V(g)))
			}
		}
		# return value vector as list item
		return(list("Monopartite.Degree.Centrality"=V(g)$degree.centrality))
	}
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ biGraph }
\keyword{ centrality }
\keyword{ bipartite }
\keyword{ monopartite }

