\name{PolyCol}
\alias{PolyCol}
\title{Create list of colors depending on attribute data.}
\description{The function provide list of colors, unique colors, levels of attribute,attribute breaks.}
\usage{PolyCol(attribute,
                     colPalette=NULL,
                     at=NULL)}
\arguments{\item{attribute}{vector of attribute data}
\item{colPalette}{colours to be used to fill features depending on attribute}
\item{at}{values at which colours will change}       
}
\value{The function provide list of colors (cols), unique colors (col.uniq), levels of attribute (att),attribute breaks (brks).}
\author{Milan Kilibarda \email{kili@grf.bg.ac.rs}}

\seealso{
\link{plotGoogleMaps},
\link{ellipseGoogleMaps},
\link{bubbleGoogleMaps}
}
\examples{
# Data preparation
data(meuse)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')

m<-plotGoogleMaps(meuse,zcol='zinc')
# see results in your working directory

# similar classic plot
cols=PolyCol(meuse$zinc)$cols
plot(meuse,col=cols)

str(PolyCol(meuse$zinc))

m<-plotGoogleMaps(meuse,zcol='zinc',at=c(0,300,600, max(meuse$zinc)), colPalette=rainbow(3))
# similar classic plot
cols=PolyCol(meuse$zinc,colPalette=rainbow(3), at=c(0,300,600, max(meuse$zinc)) )$cols
plot(meuse,col=cols)

# plotKML(meuse, colour='zinc', colour_scale =cols)
}