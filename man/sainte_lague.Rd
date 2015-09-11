\name{sainte_lague}
\alias{sainte_lague}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Proportional representation by Sainte-Lague method
}
\description{
Originally an algorithm for proportional allocation of seats to parties in elections, used here to assign the six triangles in each hex to classes.
}
\usage{
sainte_lague(votes, nseats)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{votes}{
Vector of 'votes' for each party, non-negative numeric values
}
  \item{nseats}{
Single integer giving the number of seats to be allocated (6, here)
}
}

\value{
Numeric vector of length \code{nseats} giving the class membership for each seat, with an \code{error} attribute giving the rounding errors for each party.
}
\references{
\url{https://en.wikipedia.org/wiki/Sainte-Lagu\%C3\%AB_method}
}

\seealso{
\code{\link{hexclass}}
}
\examples{
sainte_lague(c(100,200,300,50),6)
}
