\name{hexclass}
\alias{hextri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hexagonal binning with classes
}
\description{
Displays a hexagonally-binned scatterplot with coloured subtriangles to
indicate number of observations in each class. 
}
\usage{
hextri(x, y, class, colours, nbins = 10, border = TRUE,
   style=c("alpha","size"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{
Coordinates of points
}
  \item{class}{
Factor giving class membership for points
}
  \item{colours}{
Vector of colors (number, name, or hashtag format) giving the colours for each class
}
  \item{nbins}{
Number of bins in the x-axis direction
}
  \item{border}{
Include a narrow transparent border around each triangle and hexagon
}
\item{style}{Represent data by hexagon size or by alpha-blending}
}
\details{
Uses the Sainte-Lague method to apportion counts to triangles, ensuring the counts
round to exactly six triangles per hexagon.
}
\value{
Used for its side-effect
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
Need to add error diffusion
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{sainte_lague}}
}
\examples{
xx<-rnorm(1000)
yy<-rnorm(1000)
cc<-cut(xx*yy,c(-Inf,-.4,0,.4,Inf))

plot(xx,yy,col=(1:4)[cc])
hextri(xx,yy,cc,1:4,nbins=20,border=TRUE,style="size")
hextri(xx,yy,cc,1:4,nbins=20,border=TRUE,style="alpha")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
