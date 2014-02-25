\name{elSPDF}
\alias{elSPDF}
\title{Create ellipses in form of SpatialPolygonsDataFrame.}
\description{Create ellipses in form of SpatialPolygonsDataFrame based on attributes: semi-major axis,  semi-minor axis, and orientation in degrees.}
\usage{elSPDF(SPDF,
                  zcol=1:3,
                  scale_e=10)}
\arguments{\item{SPDF}{object of \link[sp]{SpatialPointsDataFrame-class} with
          associated coordinate reference systems and minimum three attributes: semi-major axis,  semi-minor axis, and orientation in degrees.}
\item{zcol}{variable column names, or column numbers after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column}
\item{scale_e}{scale factor}                
}
\value{The function provide SpatialPolygonsDataFrame of ellipses.}
\author{Milan Kilibarda \email{kili@grf.bg.ac.rs}}

\seealso{
\link{pieSP},
\link{elSPDF},
\link{bubbleGoogleMaps}
}
\examples{
# Data preparation
    ell<- data.frame(E=c(7456263,7456489,7456305),N=c(4954146,4952978,4952695),A=c( 2.96,4.55,7.10),B=c(2.35,2.11,2.29),teta=c(28.35242,41.04491,38.47216))
    coordinates(ell) <- ~E+N
    proj4string(ell) <- CRS("+proj=tmerc +lat_0=0 +lon_0=21 +k=0.9999 +x_0=7500000 +y_0=0 +ellps=bessel +towgs84=574.027,170.175,401.545,4.88786,-0.66524,-13.24673,0.99999311067 +units=m")
    
    ellpses<-elSPDF(ell)
    spplot(ellpses,"A")
}