\name{hexclass}
\alias{hextri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Hexagonal binning with classes
}
\description{
Displays a hexagonally-binned scatterplot with coloured subtriangles to
indicate number of observations in each class. With a single class, gives a base-graphics version of the ordinary hexbin plot.
}
\usage{
hextri(x, y, class, colours, nbins = 10, border = TRUE,diffuse=FALSE,
   style=c("alpha","size"),weights=NULL,...)
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
\item{diffuse}{
	Pass on rounding error to nearest not-yet-drawn hexes so that rare classes get represented
}
\item{style}{Represent data by hexagon size or by alpha-blending}
\item{weights}{If not \code{NULL}, sampling weights or frequency
  weights. The hexbin plot will be based on the sum of weights in the
hex}
\item{\dots}{Passed to the \code{plot} call that sets up the plot region}
}
\details{
Uses the Sainte-Lague method to apportion counts to triangles, ensuring the counts
round to exactly six triangles per hexagon.

The binning/aspect ratio are adjusted to give regular hexagons on the figure region set by \code{plot(x,y)}. If you reshape the window you will need to redraw the plot. 
}
\value{
A list with components \code{x}, \code{y}, and \code{col} suitable as input to \code{polygon()}
}

\author{
Thomas Lumley
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
rval<-hextri(xx,yy,cc,1:4,nbins=20,border=TRUE,style="alpha")

## using the return value
plot(y~x,data=rval,type="n")
with(rval, polygon(x,y,col=col,border=NA))

## diffusion
xx<-runif(10000)
yy<-runif(10000)
cc<-rep(1:3,c(4750,4750,500))
hextri(xx,yy,cc,2:4,border=TRUE,diffuse=FALSE,style="size")
hextri(xx,yy,cc,2:4,border=TRUE,diffuse=TRUE,style="size")

hextri(xx,yy,cc,2:4,border=TRUE,diffuse=TRUE,style="size",weights=cc)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}
