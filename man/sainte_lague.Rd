\name{sainte_lague}
\alias{sainte_lague}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sainte_lague(votes, nseats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{votes}{
%%     ~~Describe \code{votes} here~~
}
  \item{nseats}{
%%     ~~Describe \code{nseats} here~~
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (votes, nseats) 
{
    nparties <- length(votes)
    denominators = 2 * (1:nseats) - 1
    quotients = outer(votes, denominators, "/")
    last = sort(quotients, decreasing = TRUE)[nseats]
    clear <- rowSums(quotients > last)
    borderline <- rowSums(quotients == last)
    borderline[sample(which(borderline > 0), sum(borderline) - 
        (nseats - sum(clear)))] <- 0
    rep(1:nparties, clear + borderline)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
