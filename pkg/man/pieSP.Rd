\name{pieSP}
\alias{pieSP}
\title{Create pie charts data in form of SpatialPolygonsDataFrame.}
\description{Create pie charts data in form of SpatialPolygonsDataFrame depending on few attributes.}
\usage{pieSP(SPDF,
              zcol=1:length(SPDF@data),
              scalelist=TRUE, 
              max.radius=100,  
              do.sqrt = TRUE
             )}
\arguments{\item{SPDF}{object of \link[sp]{SpatialPointsDataFrame-class} with
          associated coordinate reference systems }
\item{zcol}{variable column names, or column numbers after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column}
\item{scalelist}{if TRUE circles are different in size, FALSE all circles have same radius}          
\item{max.radius}{value for maximum radius in meter}
\item{do.sqrt}{logical; if TRUE the plotting symbol area (sqrt(diameter)) is 
                 proportional to the value of the z-variable; if FALSE, 
                 the symbol size (diameter) is proportional to the z-variable}      
}
\value{The function provide SpatialPolygonsDataFrame depending on few attributes, ready to use for plotGoogleMaps or spplot.}
\author{Milan Kilibarda \email{kili@grf.bg.ac.rs}}

\seealso{
\link{pieSP},
\link{elSPDF},
\link{bubbleSP}
}
\examples{
# Data preparation
data(meuse)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')

pies<-pieSP(meuse,zcol=c('zinc','lead','copper'), max.radius=120)
pies$pie=rep(c('zinc','lead','copper'),155)

m=plotGoogleMaps(pies, zcol='pie')

pies$pie2=rep(1:3,155)
spplot(pies, 'pie2')

pies2<-pieSP(meuse,zcol=c('zinc','dist.m'), max.radius=70,scalelist=FALSE)
pies2$pie=rep(c('zinc','dist.m'),155)

m=plotGoogleMaps(pies2, zcol='pie')

# plotKML(pies, col='pie')
}
