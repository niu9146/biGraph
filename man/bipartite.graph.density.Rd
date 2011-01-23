\name{bipartite.graph.density}
\alias{bipartite.graph.density}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bipartite graph density measure
}
\description{
Measures density for bipartite graphs.
}
\usage{
bipartite.graph.density(g)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph, directed or undirected
}
}
\details{
The density captures the fraction of actual present edges over the number of all possible edges, given that multiple edges are not allowed.  This function is the adaption to bipartite graphs as presented in Borgatti and Everett (1997).  The boolean vertex attribute \code{type} has to be provided.  \code{\link[igraph]{is.bipartite}} can be used to generate \code{type} attribute, although care has to be taken if graph consists of multiple components.
}
\value{
List with density value as only item.

   \item{\code{Density} }{density value}
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
\code{\link[igraph]{is.bipartite}}, \code{\link[igraph]{graph.density}}
}
\examples{
data(g)

## Calculate density
bipartite.graph.density(g)


## The function is currently defined as
function(g){
	if (!is.null(V(g)$type)){
		# return value as list item
		if (!is.directed(g)){
			return(list("Density"=length(E(g))/(length(which(V(g)$type))*length(which(!V(g)$type)))))
		}
		else{
			return(list("Density"=length(E(g))/(2*length(which(V(g)$type))*length(which(!V(g)$type)))))
		}
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
\keyword{ density }
\keyword{ bipartite }


