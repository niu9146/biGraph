\name{print.summary.lpi}
\alias{print.summary.lpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print linkage pattern information summary
}
\description{
Prints summary on linkage pattern information object.
}
\usage{
\method{print.summary}{lpi}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
a lpi object
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
\code{\link[biGraph]{summary.lpi}}, \code{\link[biGraph]{lpi}}, \code{\link[biGraph]{read.lpi}}, \code{\link[biGraph]{write.lpi}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(x, ...){
	cat("\nNumber of linkage patterns: ",x$nlp)
	cat("\n\nLinkage Pattern Sizes\n\n")
	print(x$lp_sum, ...)
	cat("\n\nOccurences\n\n")
	print(x$occ_sum, ...)
	cat("\n\nSources\n\n")
	print(x$source_sum, ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lpi }
