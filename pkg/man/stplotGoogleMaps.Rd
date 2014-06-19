\name{stplotGoogleMaps}
\alias{stplotGoogleMaps}
\title{Create a plot of spacetime (STDIF,STFDF) data on Google Maps}
\description{Plot htm output with Google Maps API and a plot of spacetime (STDIF,STFDF)
             data as a combination of users' data and Google Maps layers. Ready to use as local htm file or into your own website.}
\usage{
stplotGoogleMaps(SPT,
                           zcol=1,
                           stfilename='spacetime.htm',
                           filename='file',
                           w="100\%",
                           h="49.5\%",
                           openMap=FALSE,
                           colPalette=NULL,
                           do.bubble=FALSE,
                           at=NULL,
                           bubble= list(max.radius=10000,
                                        key.entries = if(do.bubble){quantile(SPT@data[,zcol],(1:5)/5, 
                                        na.rm=T)}else{0},
                                         do.sqrt = TRUE),
                           iconMarker="",
                           ...) }
\arguments{
  \item{SPT}{object of spacetime (STFDF-class, STIDF-class) with
          associated coordinate reference systems}
\item{zcol}{variable column name, or column number after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column}
\item{filename}{string use as prefix to partial files, each time instance}
\item{stfilename}{the name of the output htm or html file}
\item{w}{the width of the map for each time instance. Can be in percent or in pixels (px)}
\item{h}{the height of the map for each time instance. Can be in percent or in pixels (px)}
\item{openMap}{if TRUE map is browesed by default browser for each time instance}
\item{colPalette}{colours to be used to fill polygon or colour for the line}
\item{do.bubble}{if TRUE creates bubble plots}
\item{at}{values at which colours will change}
\item{bubble}{list of parameters to be passed to bubble plot. The max.radius value for maximum radius in meter. The key.entries the values that will be plotted in the key; by default the five quantiles min, q.25, median q.75, max. The do.sqrt logical; if TRUE the plotting symbol area (sqrt(diameter)) is proportional to the value of the z-variable; if FALSE, the symbol size (diameter) is proportional to the z-variable }
  \item{iconMarker}{if it is empty string creates default marker icon, link to the image or local image to be used as marker image}
  \item{...}{arguments from plotGoogleMaps}
}
\value{This function returns a html(htm) file ready to use.}
\author{Milan Kilibarda <kili@grf.bg.ac.rs>}
\seealso{
\link{plotGoogleMaps},
\link{stfdfGoogleMaps}
}
\examples{
## Data preparation
## Point data
## data from plotKML package and plotKML tutorial
#library(plotKML)
#data(HRtemp08)
# HRtemp08$ctime <- as.POSIXct(HRtemp08$DATE, format="\%Y-\%m-\%dT\%H:\%M:\%SZ")
# library(spacetime)
# sp <- SpatialPoints(HRtemp08[,c("Lon","Lat")])
# proj4string(sp) <- CRS("+proj=longlat +datum=WGS84")
# HRtemp08.st <- STIDF(sp, time = HRtemp08$ctime, data = HRtemp08[,c("NAME","TEMP")])
# HRtemp08_jan <- HRtemp08.st[1:500]
#str(HRtemp08_jan)
# plot STDIF
# stplotGoogleMaps(HRtemp08_jan,zcol='TEMP', mapTypeId='ROADMAP',w='49\%',h='49\%')
# plot STDIF bubble 
# stplotGoogleMaps(HRtemp08_jan,zcol='TEMP',stfilename='HR_temp.html', mapTypeId='ROADMAP',w='49\%',h='49\%',  strokeOpacity = 0,
#                  do.bubble=T, bubble= list(max.radius=15000,
#                key.entries =quantile(HRtemp08_jan@data[,'TEMP'],(1:5)/5, na.rm=T),
#                  do.sqrt = F) )
#
############################################################################
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
#library(RColorBrewer)
#ee= stplotGoogleMaps(Produc.st,zcol='unemp',stfilename='USA.htm',colPalette=brewer.pal(9, "YlOrRd"), mapTypeId='ROADMAP',
#w='49\%',h='49\%', fillOpacity=0.85)
## without control
# ee= stplotGoogleMaps(Produc.st,zcol='unemp',stfilename='USA2.htm',colPalette=brewer.pal(9, "YlOrRd"), 
#mapTypeId='ROADMAP',w='33\%',h='25\%', fillOpacity=0.85, control.width=0)                
}
