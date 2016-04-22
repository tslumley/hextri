\name{panel.hextri}
\alias{panel.hextri}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A lattice panel function for hexagonal binning with classes
}
\description{
Displays a hexagonally-binned scatterplot with coloured subtriangles to indicate number of observations in each class. Uses the Sainte-Lague method to apportion counts to triangles, ensuring the counts round to exactly six triangles per hexagon.

}
\usage{
panel.hextri(x, y, groups, subscripts, colours, nbins = 10, border = TRUE, diffuse = FALSE, style = c("alpha", "size"), weights = NULL, sorted=!diffuse,shape = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x,y}{
point coordinates for the subset being plotted
}

  \item{groups}{
A factor giving the class identity for all points (will be filled in automatically by \code{xyplot})
}
  \item{subscripts}{
Vector selecting the elements of \code{groups} that are in the panel (will be filled in automatically by \code{xyplot})
}
  \item{colours}{
Vector of colors (number, name, or hashtag format) giving the colours for each class
}
  \item{nbins}{
Number of bins along the x axis
}
  \item{border}{
If \code{TRUE}, leave a transparent border around each element drawn
}
  \item{diffuse}{
If \code{TRUE} pass on rounding error to nearest not-yet-drawn hexes so that rare classes get represented
}
\item{style}{Represent data by hexagon size or by alpha-blending}
\item{weights}{If not \code{NULL}, sampling weights or frequency
  weights. The hexbin plot will be based on the sum of weights in the
  hex}
\item{sorted}{Sort the triangles into a consistent order within each hex?}
  \item{shape}{
Aspect ratio for each hex. 
}
  \item{\dots}{
because you have to.
}
}

\value{
A panel
}

\author{
Thomas Lumley
}

\seealso{
\code{\link{hextri}} is the base-graphics plot
}
\examples{
library(lattice)
xx<-rnorm(1000)
yy<-rnorm(1000)
cc<-cut(xx*yy,c(-Inf,-.4,0,.4,Inf))
zz<-cut(xx+yy,c(-Inf,-1,0,1,Inf))

plot(xx,yy,col=(1:4)[cc])
xyplot(yy~xx|zz, panel=panel.hextri,groups=cc,colours=1:4,style="size",nbins=10)

data(airquality)
airquality$o3group<-with(airquality, cut(Ozone, c(0,18,60,Inf)))

xyplot(Temp~Solar.R|equal.count(Wind,4), groups=o3group, panel=panel.hextri,
  data=na.omit(airquality),colours=c("royalblue","grey60","goldenrod"),
  strip=strip.custom(var.name="Wind Speed"),xlab="Solar Radiation (langley)",
  ylab="Temperature (F)")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{hplot}

