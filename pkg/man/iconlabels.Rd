\name{iconlabels}
\alias{iconlabels}
\title{Create icon markers depending on attribute data.}
\description{The function provide links to icon markers or icon labels ready to use for plotGoogleMaps.}
\usage{iconlabels(attribute,
                     colPalette=NULL,
                     at=NULL,
                     height=10,
                     icon=FALSE,
                     scale=0.6
                     
  )}
\arguments{\item{attribute}{vector of attribute data}
\item{colPalette}{colours to be used to fill marker symbols or labels}
\item{at}{values at which colours will change}
\item{height}{text height in pixels}
\item{icon}{if false create just labels, contrariwise create markers and labels in markers}
\item{scale}{scale of marker icon}        
}
\value{The function provide links to icon markers or icon labels ready to use for plotGoogleMaps.}
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

# zinc labels
ic=iconlabels(meuse$zinc, height=12)
m<-plotGoogleMaps(meuse,zcol='zinc', iconMarker=ic)

# landuse labels and markers
ic=iconlabels(meuse$landuse, height=12, colPalette=rainbow(15) )
m<-plotGoogleMaps(meuse,zcol='landuse',colPalette=rainbow(15), iconMarker=ic)

ic=iconlabels(meuse$landuse, height=12, colPalette='#9ECAE1', icon=TRUE)
m<-plotGoogleMaps(meuse,zcol='landuse',colPalette='#9ECAE1', iconMarker=ic)

}