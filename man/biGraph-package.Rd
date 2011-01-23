\name{biGraph-package}
\alias{biGraph-package}
\alias{biGraph}
\docType{package}
\title{
Toolbox for Bipartite Graphs
}
\description{
This package enhances the igraph--package by providing additional methods for the analysis of bipartite graphs and their monopartite projections.  The current main functionality is focused on measuring the loss of information associated to the projection of bipartite graphs.  In addition, a set of tools is provided that supports the appropiate analysis of bipartite graphs.
}
\details{
\tabular{ll}{
Package: \tab biGraph\cr
Type: \tab Package\cr
Version: \tab 0.9-3\cr
Date: \tab 2011-01-23\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}
bipartite.projection.informationloss(graph, vType=c(FALSE,TRUE), measures=c("graph.dH","vertex.dH","edge.dH","covLoss"), mlps=0, logB=exp(1), parall=FALSE, preschedule=FALSE, cores=1, verbose=FALSE)
lpi(graph, vset=FALSE, mode=c('minimal','basic','full'))
write.lpi(lpi, file)
read.lpi(file)
get.weighted.projection(graph, mode=c('shared-neighbours','pass-through'))
bipartite.graph.density(graph)
degree.centrality(graph)
bipartite.betweenness.centrality(graph)
bipartite.closeness.centrality(graph)
single.mode.degree.centralization(graph)
single.mode.betweenness.centralization(graph)
single.mode.closeness.centralization(graph)
}
\author{
Ingo Vogt

Maintainer: \email{bigraph.vogt@gmail.com}
}
\references{
Vogt, I. and Mestres, J. (in preparation) Assessing information loss associated with bipartite network projection.
}
\keyword{ package }
\keyword{ graph }
\keyword{ igraph }
\keyword{ biGraph }
\keyword{ network }
\seealso{
\code{\link[igraph]{igraph}}, \code{\link[multicore]{multicore}}, \code{\link[gmp]{bigz}}
}
\examples{
data(g)

## Calculate monopartite projection and apply all information loss measures
proj <- bipartite.projection.informationloss(g, vType=TRUE, measures="complete", verbose=TRUE)

## Graph measurements
proj[[1]]$H.delta

## Vertex measurements
summary(V(proj[[1]])$H.delta)
summary(V(proj[[1]])$avCovLoss)

## Edge measurements
summary(E(proj[[1]])$H.delta)
summary(E(proj[[1]])$covLoss)

## Extract linkage patterns
linkagePatterns <- lpi(g,vType=FALSE,mode='full')
linkagePatterns
summary(linkagePatterns)

## Calculate density of bipartite graph
bipartite.graph.density(g)

## Calculate centrality scores for vertices in bipartite graph
bipartite.betweenness.centrality(g)
bipartite.closeness.centrality(g)
degree.centrality(g)

## Calculate centrality scores for vertices in monopartite graphs
degree.centrality(proj[[1]])

## Calculate vertex centralization scores for each subset
single.mode.betweenness.centralization(g)
single.mode.closeness.centralization(g)
single.mode.degree.centralization(g)

}
