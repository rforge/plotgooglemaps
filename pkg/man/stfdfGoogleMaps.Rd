\name{stfdfGoogleMaps}
\alias{stfdfGoogleMaps}
\title{Create a plot of spacetime (STFDF) data on Google Maps}
\description{Plot htm output with Google Maps API and a plot of spacetime (STFDF)
             data as a combination of users' data and Google Maps layers. Ready to use as local htm file or into your own website.}
\usage{
stfdfGoogleMaps(stfdf,
                 zcol=1,
                 filename='',
                 layerName="",
                 plotNames=row.names(stfdf@sp),
                 aggregateFUN='mean',
                 round.att=1,
                 plot.height=300,
                 plot.width=300,
                           ...) }
\arguments{
  \item{stfdf}{object of spacetime (STFDF-class) with associated coordinate reference systems}
\item{zcol}{variable column name, or column number after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column, it has to be numeric}
\item{filename}{the name of the output htm or html file. If filename is not defined the function creates temporary file.}
 \item{layerName}{the name of the layer. Layer name appears on the check box for switching layer of and on in output htm.}
\item{plotNames}{vector of of plot titles in infoWindow plots} 
\item{aggregateFUN}{a function to compute the summary statistics of time series}
\item{round.att}{integer indicating the number of decimal places for ploting attribute, if not a number no round} 
\item{plot.height}{the height of of plot in infoWindow plot in pixels (px)}
\item{plot.width}{the width of plot in infoWindow plot in pixels (px)}
\item{...}{arguments from plotGoogleMaps}
}
\value{This function returns a html(htm) file ready to use.}
\author{Milan Kilibarda <kili@grf.bg.ac.rs>}
\seealso{
\link{stplotGoogleMaps},
\link{plotGoogleMaps}
}
\examples{
## Data preparation
## STFDF data from spacetime vignette spacetime: Spatio-Temporal Data in R 
#library("maps")
# states.m = map('state', plot=FALSE, fill=TRUE)
# IDs <- sapply(strsplit(states.m$names, ":"), function(x) x[1])
# library("maptools")
#states = map2SpatialPolygons(states.m, IDs=IDs)
#yrs = 1970:1986
#time = as.POSIXct(paste(yrs, "-01-01", sep=""), tz = "GMT")
# library("plm")
#data("Produc")
#Produc.st = STFDF(states[-8], time, Produc[order(Produc[2], Produc[1]),])
#Produc.st@sp@proj4string=CRS('+proj=longlat +datum=WGS84')
# m <-  stfdfGoogleMaps(Produc.st, zcol= 'unemp')               
}
