\name{lpi}
\alias{lpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract linkage pattern information from bipartite graph
}
\description{
Returns linkage pattern information associated to selected vertex subset.
}
\usage{
lpi(g, vType = FALSE, mode = "full")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
bipartite graph
}
  \item{vType}{
logical, indicating according to which vertex subset the linkage pattern information shall be extracted.  See also function \code{bipartite.projection} from the \pkg{igraph}--package.  Defaults to \verb{FALSE}.
}
  \item{mode}{
string, one of \verb{"full"}, \verb{"basic"}, or \verb{"minimal"}, determining the level of detail, defaults to \verb{"full"}
}
}
\details{
Setting argument \code{mode} determines the level of detail of the linkage pattern information.
\itemize{
	\item{mode="full" }{Include names of vertices selected by \code{vType}, include number of occurrences for each linkage pattern, resulting lpi object carries complete information on bipartite graph (except additional attributes)}
	\item{mode="basic" }{Include number of occurrences for each linkage pattern, allows reconstruction of bipartite graph isomorphic to the original graph, yet without the correct IDs of the vertices selected by \code{vType}}
	\item{mode="minimal" }{Set number of occurrences for each linkage pattern to \verb{1}, allows reconstruction of a \emph{minimal} bipartite graph, that contains one vertex for each linkage pattern}
}
}
\value{
A lpi object
}
\references{
Vogt, I. and Mestres, J. (in preparation) Assessing information loss associated with bipartite network projection.
}
\author{
Ingo Vogt <\email{bigraph.vogt@gmail.com}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[biGraph]{write.lpi}}, \code{\link[biGraph]{summary.lpi}}, \code{\link[biGraph]{read.lpi}}
}
\examples{
data(g)

## Extract linkage patterns
linkagePatterns <- lpi(g,vType=FALSE,mode='full')
linkagePatterns
summary(linkagePatterns)

## Write minimal linkage pattern information to file
linkagePatterns <- lpi(g,vType=FALSE,mode='minimal')
write.lpi(linkagePatterns, 'foomin.lpi')

## Read linkage patterns back to lpi object
linkagePatterns_b <- read.lpi('foomin.lpi')

## Read linkage patterns and construct graph from it
g_min <- (read.lpi('foomin.lpi'))$graph

## Compare original and reconstructed graph
def.par <- par(no.readonly = TRUE) # save default, for resetting...
par(mfrow=c(1,2),family='Helvetica')
plot(g,layout=layout.fruchterman.reingold,vertex.label=gsub('_','\n',V(g)$dname),vertex.color=V(g)$col,vertex.label.color='black',vertex.label.dist=0.5,vertex.label.family='Helvetica')
plot(g_min,layout=layout.fruchterman.reingold,vertex.label=gsub('_','\n',V(g_min)$name),vertex.label.family='Helvetica')
par(def.par)


## The function is currently defined as
function(g,vType=FALSE,mode='full'){
	bp <- is.bipartite(g)
	if (bp$res==TRUE){
		V(g)$type <- bp$type
		for (v in V(g)[type==vType]){
			V(g)[v]$lp <- paste(sort(V(g)[neighbors(g,v)]$name),sep='',collapse=';')
		}
		Linkage.Patterns <- unique(V(g)[type==vType]$lp)
		Sources <- c()
		if (mode=='minimal'){
			lpi_df <- data.frame(Linkage.Patterns,rep(1,length(Linkage.Patterns)))
			
		}
		else{
			Occurrences <- c()
			for (p in Linkage.Patterns){
				Sources <- c(Sources,paste(sort(V(g)[lp==p]$name),sep='',collapse=';'))
				Occurrences <- c(Occurrences,length(V(g)[(lp==p)&(!is.na(lp))]))
			}
			if (mode=='full'){
				lpi_df <- data.frame(Linkage.Patterns,Occurrences,Sources)
			}
			else if (mode=='basic'){
				lpi_df <- data.frame(Linkage.Patterns,Occurrences)
			}
		}
		class(lpi_df) <- 'lpi'
		return(lpi_df)
	}
	else {
		cat("Input graph is not bipartite")
	}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lpi }
\keyword{ biGraph }
\keyword{ bipartite }
