\name{hexclass}
\alias{hexclass}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hexclass(x, y, class, colours, nbins = 10, border = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{class}{
%%     ~~Describe \code{class} here~~
}
  \item{colours}{
%%     ~~Describe \code{colours} here~~
}
  \item{nbins}{
%%     ~~Describe \code{nbins} here~~
}
  \item{border}{
%%     ~~Describe \code{border} here~~
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
function (x, y, class, colours, nbins = 10, border = FALSE) 
{
    plot(x, y, type = "n")
    h <- hexbin(x, y, IDs = TRUE, xbins = nbins)
    centers <- hcell2xy(h)
    tab <- table(h@cID, class)
    allocations <- apply(tab, 1, sainte_lague1, 6)
    full_radius <- diff(h@xbnds)/h@xbins/sqrt(3)
    radii <- full_radius * sqrt(h@count/max(h@count))
    col_matrix <- matrix(colours[t(allocations)], nrow = ncol(allocations))
    hexen(centers$x, centers$y, radii, col_matrix, border = border)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
