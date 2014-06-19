\name{bubbleSP}
\alias{bubbleSP}
\title{Create bubble data in form of SpatialPolygonsDataFrame.}
\description{Create bubble data in form of SpatialPolygonsDataFrame based on one attribute.}
\usage{bubbleSP(SPDF,
           zcol=1,
           scale_e=1,
           max.radius=100,
           key.entries = quantile(SPDF@data[,zcol],(1:5)/5, na.rm=TRUE),
           do.sqrt = TRUE,
           radius.vector=NULL)}
\arguments{\item{SPDF}{object of \link[sp]{SpatialPointsDataFrame-class} with
          associated coordinate reference systems }
\item{zcol}{variable column names, or column numbers after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column}
\item{scale_e}{scale factor for radiuses}          
\item{max.radius}{value for maximum radius in meter}
\item{key.entries}{the values that will be plotted in the key; by default the five 
                    quantiles min, q.25, median q.75, max}
\item{do.sqrt}{logical; if TRUE the plotting symbol area (sqrt(diameter)) is 
                 proportional to the value of the z-variable; if FALSE, 
                 the symbol size (diameter) is proportional to the z-variable}
\item{radius.vector}{vector of predefined radius for each point}
       
}
\value{The function provide SpatialPolygonsDataFrame based on one attribute, ready to use for plotGoogleMaps or spplot.}
\author{Milan Kilibarda \email{kili@grf.bg.ac.rs}}

\seealso{
\link{pieSP},
\link{elSPDF},
\link{bubbleGoogleMaps}
}
\examples{
# Data preparation
wind<-data.frame( Station=c('Zlatibor' , 'Sjenica' , 'Pozega' , 
        'Kraljevo', 'Kopaonik', 'Kursumlija', 'Krusevac'),
                  Lon=c(19.708418 , 19.999311 , 20.029322, 
                  20.724012, 20.776111, 21.279513, 21.307998 ),
                  Lat=c(43.729755 , 43.267913	,43.84859	,
                  43.726812,43.2725,	43.137053	,43.574188),
                  Temp=c(7,  7,	5,	6,	1,	6,	9),
                  Wind_spd=c(3 , 6,	1	,1,	7	,1	,2),
                  Wind_dir=c(45, 162,  138	,265,	335	,180	,270) )

coordinates(wind)<- ~Lon+Lat
wind@proj4string=CRS('+proj=longlat +datum=WGS84')

b_temp=bubbleSP(wind,zcol=c('Temp'),max.radius=10000, do.sqrt=FALSE)

pal<-colorRampPalette(c( "#FFF7BC", "#FEC44F","#D95F0E"), space = "Lab")
spplot(b_temp,'Temp', col.regions=pal(5) )
m=plotGoogleMaps(b_temp,zcol='Temp', colPalette=pal(5),strokeWeight=0.5,strokeColor='black')

# plotKML(b_temp, col='Temp')
}