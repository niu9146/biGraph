\name{print.lpi}
\alias{print.lpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print linkage pattern information
}
\description{
Prints linkage pattern information generated from bipartite graph.
}
\usage{
\method{print}{lpi}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
lpi, a linkage pattern information object
}
\item{...}{
additional arguments
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(g)

## Extract linkage patterns
linkagePatterns <- lpi(g,vType=FALSE,mode='full')
linkagePatterns

## The function is currently defined as
function(x, ...){
	cat('\nLinkage patterns\n')
	print(x$Linkage.Patterns,max.levels=0, ...)
	cat('\nOccurrences\n')
	print(x$Occurrences,max.levels=0, ...)
	cat('\nSources\n')
	print(x$Sources,max.levels=0, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lpi }
