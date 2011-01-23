\name{get.weighted.projection}
\alias{get.weighted.projection}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assign weights to projected graph
}
\description{
Provides several methods for the assignment of weights to projected monopartite graph.
}
\usage{
get.weighted.projection(g,vType=FALSE,mode='shared-neighbours', m=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph
}
  \item{vType}{
logical, indicating according to which vertex subset the projection and subsequent calculations should be done.  See also function \code{bipartite.projection} from the \pkg{igraph}--package.
}
  \item{mode}{
string, currently one of \verb{"shared-neighbours"}, \verb{"newman"}, or \verb{'pass-through'}
}
  \item{m}{
weight matrix, optional, row and column names have to be those of the retained vertices  
}
}
\details{
Weighting scheme has to be chosen by setting \code{mode}. \code{"shared-neighbours"} adds the number of shared neighbours in the bipartite graph to the edge linking two neighbours in the monopartite projection. \code{"newman"} adopts the weighting scheme presented by Newman (2001), that weighs the contributions of shared neighbours by the size of their linkage profiles minus one.  Choosing \verb{'pass-through'} allows to provide arbitrary weighting matrices as argument.
}
\value{
Weights are added as attributes to graph entities, individual weight ids are employed to enable the simultaneous presence of several different weights.

Edge weight attribute
   \item{\code{weight} }{edge weight according to selected mode}
}
\references{
Newman M (2001) Scientific collaboration networks. II. Shortest paths, weighted networks, and centrality. Physical Review E 64.
}
\author{
Ingo Vogt <\email{bigraph.vogt@gmail.com}>
}
\note{
Currently only three approaches are implemented, extension of this function will be part of future development.  Suggestions sent to the author are appreciated.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[igraph]{bipartite.projection}}, \code{\link[igraph]{get.incidence}}
}
\examples{
data(g)

## Calculate monopartite projection and apply all information loss measures
proj_weighted <- get.weighted.projection(g, vType = TRUE, mode = "shared-neighbours")
plot(proj_weighted,layout=layout.fruchterman.reingold,vertex.label=gsub('_','\n',V(proj_weighted)$name),vertex.label.color='black',vertex.label.dist=0.5,edge.label=E(proj_weighted)$weight_sharedNeighbours,vertex.label.family='Helvetica',edge.label.family='Helvetica')


## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ biGraph }
\keyword{ bipartite projection }

