\name{summary.lpi}
\alias{summary.lpi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Provide summary of lpi object
}
\description{
Specific summary function for linkage pattern object
}
\usage{
\method{summary}{lpi}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
lpi object, linkage pattern information generated from bipartite graph
}
\item{...}{
additional arguments affecting the summary produced
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list
  \item{nlp }{number of linkage patterns}
  \item{lp_sum }{summary of linkage pattern sizes}
  \item{occ_sum }{summary of number of occurrences}
  \item{source_sum }{summary of size of linkage pattern sources}
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
\code{\link[biGraph]{lpi}}, \code{\link[biGraph]{read.lpi}}, \code{\link[biGraph]{write.lpi}}
}
\examples{
data(g)

## Extract linkage patterns
linkagePatterns <- lpi(g,vType=TRUE,mode='full')
linkagePatterns
summary(linkagePatterns)

## The function is currently defined as
function(object, ...){
	no_lp <- length(object$Linkage.Patterns)
	lp_sizes_summary <- summary((unlist(lapply(strsplit(as.character(object$Linkage.Patterns),split=';'),length))), ...)
	occurrences_summary <- summary(object$Occurrences, ...)
	source_sizes_summary <- summary((unlist(lapply(strsplit(as.character(object$Sources),split=';'),length))), ...)
	res <- list(nlp=no_lp,lp_sum=lp_sizes_summary,occ_sum=occurrences_summary,source_sum=source_sizes_summary)
	class(res) <- 'summary.lpi'
	res
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lpi }
