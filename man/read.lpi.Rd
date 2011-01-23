\name{read.lpi}
\alias{read.lpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Read linkage pattern information from file
}
\description{
Reads linkage pattern information from specified file and returns \verb{lpi}--object and the bipartite graph contructed from the given information as list.
}
\usage{
\method{read}{lpi}(filename, directed_edges=FALSE, prefix="D")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
Name of file containing linkage pattern information as written by \verb{write.lpi}.
}
  \item{directed_edges}{
logical, if edges in constructed graph should be directed or not.  Defaults to \verb{FALSE}.
}
  \item{prefix}{
string, used for the generation of missing vertex names, if not provided in the input file.  Defaults to \verb{D}
}
}
\details{
}
\value{
A list
  \item{\code{lpi} }{linkage pattern information read from file}
  \item{\code{graph} }{bipartite graph of class \verb{igraph} constructed from read linkage pattern information}
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
\code{\link[biGraph]{lpi}}, \code{\link[biGraph]{write.lpi}}, \code{\link[biGraph]{summary.lpi}}
}
\examples{
data(g)

## Extract linkage patterns
linkagePatterns <- lpi(g,vType=FALSE,mode='full')
linkagePatterns
summary(linkagePatterns)

## Write minimal linkage pattern information to file
linkagePatterns <- lpi(g,vType=FALSE,mode='full')
write.lpi(linkagePatterns, 'foomin.lpi')

## Read linkage patterns back to lpi object
#linkagePatterns_b <- read.lpi('foomin.lpi')

## Read linkage patterns and construct graph from it
g_min <- (read.lpi('foomin.lpi'))$graph

## Compare original and reconstructed graph
def.par <- par(no.readonly = TRUE) # save default, for resetting...
par(mfrow=c(1,2),family='Helvetica')
plot(g,layout=layout.fruchterman.reingold,vertex.label=gsub('_','\n',V(g)$dname),vertex.color=V(g)$col,vertex.label.color='black',vertex.label.dist=0.5,vertex.label.family='Helvetica')
plot(g_min,layout=layout.fruchterman.reingold,vertex.label=gsub('_','\n',V(g_min)$name),vertex.label.family='Helvetica')
par(def.par)


## The function is currently defined as
function(filename,directed_edges=FALSE,prefix='D'){
	lpi <- read.table(filename,header=TRUE)
	class(lpi) <- 'lpi'
	## transform linkage pattern information into bipartite graph
	elc <- c()
	vc <- 0
	## 'basic' and 'minimal' lp information, create generic names for omitted vertex set
	if (length(lpi)==2){
		for (entry in 1:length(lpi$Occurrences)){
			for (s in 1:lpi$Occurrences[entry]){
				vc <- vc + s
				for (b in strsplit(as.character(lpi$Linkage.Patterns[entry]),';')[[1]]){
					elc <- c(elc,paste(prefix,vc,sep='_'),b)	## add one edge to edgelist
				}
			}
		}
	}
	## 'full'information given
	else if (length(lpi)==3){
		for (entry in 1:length(lpi$Occurrences)){
			sids <- strsplit(as.character(lpi$Sources[entry]),';')[[1]]
			for (s in 1:lpi$Occurrences[entry]){
				## add edges to edgelist
				for (b in strsplit(as.character(lpi$Linkage.Patterns[entry]),';')[[1]]){
					elc <- c(elc,sids[s],b)
				}
			}
		}
	}
	return(list(lpi=lpi,graph=graph.edgelist(matrix(elc,nc=2,byrow=TRUE),directed=directed_edges)))
 }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lpi }
