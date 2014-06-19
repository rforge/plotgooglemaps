\name{vectorsSP}
\alias{vectorsSP}
\title{Create radius vector data in form of SpatialLinesDataFrame.}
\description{Create radius vector data in form of SpatialLinesDataFrame based on radius and azimuth.}
\usage{vectorsSP(SPDF,
           zcol=1:2,
           maxlength=30000, 
           arrowSize=0.15, 
           arrAng=30  
          )}
\arguments{\item{SPDF}{object of \link[sp]{SpatialPointsDataFrame-class} with
          associated coordinate reference systems and minimum two attributes: radius in meter and azimuth in degrees. }
  \item{zcol}{variable column names, or column numbers after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column. Attribute names or numbers of
             columns contains radius in meter and azimuth in degrees}
\item{maxlength}{value for maximum vetor in meter}
\item{arrowSize}{size of arrow relative to vector size}
\item{arrAng}{angle of arrow in degrees, 0 no arrow}        
}
\value{The function provide SpatialLinesDataFrame based on radius and azimuth, ready to use for plotGoogleMaps or spplot.}
\author{Milan Kilibarda \email{kili@grf.bg.ac.rs}}

\seealso{
\link{pieSP},
\link{elSPDF},
\link{bubbleSP}
}
\examples{
# Data preparation
wind<-data.frame( Station=c('Zlatibor' , 'Sjenica' , 
    'Pozega' , 'Kraljevo', 'Kopaonik', 'Kursumlija', 'Krusevac'),
                  Lon=c(19.708418 , 19.999311 , 20.029322,
                  20.724012, 20.776111, 21.279513, 21.307998 ),
                  Lat=c(43.729755 , 43.267913	,43.84859	,
                  43.726812	,43.2725,	43.137053	,43.574188),
                  Temp=c(7,  7,	5,	6,	1,	6,	9),
                  Wind_spd=c(3 , 6,	1	,1,	7	,1	,2),
                  Wind_dir=c(45, 162,  138	,265,	335	,180	,270) )

coordinates(wind)<- ~Lon+Lat
wind@proj4string=CRS('+proj=longlat +datum=WGS84')

wind_vect=vectorsSP(wind,zcol=c('Wind_spd','Wind_dir'))

pal<-colorRampPalette(c( "#CB181D", "#A50F15","#67000D"), space = "Lab")
spplot(wind_vect,'Wind_spd', col.regions=pal(5) )
m=plotGoogleMaps(wind_vect,zcol='Wind_spd', 
      colPalette=pal(5), mapTypeId='ROADMAP',strokeWeight=3)

# plotKML(wind_vect)
}