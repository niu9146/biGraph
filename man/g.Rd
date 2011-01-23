\name{g}
\alias{g}
\docType{data}
\title{
Subset of the bipartite diseasome network
}
\description{
A bipartite graph consisting of 38 vertices and 48 edges.  The vertices form two equally sized subsets, representing diseases and genes.  Edges between disease and gene vertices indicate evidence of correlation between gene alteration and disease.
}
\usage{data(g)}
\format{
  An igraph object.
}
\details{
This graph corresponds to the subnetwork shown in Figure 1 in the reference.
}
\source{
Goh, K. et al. (2007) The human disease network. PNAS \bold{104}, 8685--8690.
}
\references{
}
\examples{
data("g")
summary(g)
plot(g,layout=layout.fruchterman.reingold,vertex.label=gsub('_','\n',V(g)$dname),vertex.color=V(g)$col,vertex.label.color='black',vertex.label.dist=0.5,vertex.label.family='Helvetica')
## maybe str(g) ; plot(g) ...
}
\keyword{ datasets }
\keyword{ bipartite }
