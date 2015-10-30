\name{segmentGoogleMaps}
\alias{segmentGoogleMaps}
\title{Create a segment circles - pie charts plot of spatial data on Google Maps, 
       by using two or more related attributes.}
\description{Plot htm output with Google Maps API in form of pie charts plot form spatial
             data by using two or more related attributes. Ready to use as local htm file or into your own website.}
\usage{
segmentGoogleMaps(SP, 
                  zcol = 1:length(SP@data), 
                  filename = '', 
                  max.radius = 100, 
                  scalelist = TRUE, 
                  do.sqrt = FALSE, 
                  add = F, 
                  previousMap = NULL, 
                  colPalette = rainbow(ncol(SP@data[, zcol])),
                  strokeColor = '', 
                  strokeOpacity = 1, 
                  strokeWeight = 1, 
                  fillOpacity = 0.7, 
                  geodesic = TRUE, 
                  clickable = TRUE, 
                  zIndex = 'null', 
                  map.width="100\%",
                  map.height="100\%",
                  layerName="",
                  layerNameEnabled = TRUE,
                  layerGroupName = FALSE,
                  control.width="100\%",
                  control.height="100\%",
                  zoom = 15, 
                  fitBounds = TRUE, 
                  mapTypeId = 'HYBRID', 
                  disableDoubleClickZoom = FALSE, 
                  draggable = TRUE, 
                  keyboardShortcuts = TRUE, 
                  mapTypeControlOptions = 'DEFAULT', 
                  navigationControl = TRUE, 
                  navigationControlOptions = 'DEFAULT', 
                  scaleControlOptions = 'STANDARD', 
                  noClear = FALSE, 
                  scrollwheel = TRUE,
                  streetViewControl = FALSE,
                  legend=TRUE,
                  legendOpacityWeightEnabled = TRUE,
                  control=TRUE,
                  InfoWindowControl=list(map=map, event="click",position="event.latLng",
                                         disableAutoPan=FALSE, maxWidth=330,
                                         pixelOffset="null",zIndex="null"),
                  funcSetInfoWindowText = NULL,
                  map="map",
                  mapCanvas="map_canvas",
                  css = "",
                  api="https://maps.googleapis.com/maps/api/js?libraries=visualization",
                  openMap=TRUE,
                  ...)}
\arguments{\item{SP}{object of \link[sp]{SpatialPointsDataFrame-class} with
          associated coordinate reference systems}
  \item{filename}{the name of the output htm or html file. If filename is not defined
         the function creates temporary file.}
  \item{zcol}{variable column names, or column numbers after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column}
  \item{max.radius}{value for largest circle (the plotting symbols) in metre}
  \item{scalelist}{if TRUE circles are different in size, FALSE all circles have same radius}
  \item{do.sqrt}{logical; if TRUE the plotting symbol area (sqrt(diameter)) is 
                 proportional to the value of the z-variable; if FALSE, 
                 the symbol size (diameter) is proportional to the z-variable}
  \item{add}{logical; if TRUE the result of the function will be a list stored as variable 
             in the R. It is possible to combine more layers in the one plot, previously saved
             output from plotGoogleMaps should be given in the previousMap attribute.}
  \item{previousMap}{previously saved variable from plotGoogleMaps when attribute add is TRUE.
                     It is solution for combining more than one layers in the one plot.}
  \item{colPalette}{colours to be used to fill plotting symbols; numeric vector of same size
                     like key.entries}
  \item{strokeColor}{the color to draw the border of circle (the plotting symbols), NULL for no border}
  \item{strokeOpacity}{the stroke opacity between 0.0 and 1.0}
   \item{strokeWeight}{the stroke width in pixels}
   \item{fillOpacity}{the fill opacity between 0.0 and 1.0}
  \item{geodesic}{render each edge as a geodesic (a segment of a 'great circle')}
  \item{clickable}{indicates whether this plotting symbol handles click events}
  \item{zIndex}{the zIndex compared to other polygons}
  \item{map.width}{the width of the map. Can be in percent or in pixels (px)}
  \item{map.height}{the height of the map. Can be in percent or in pixels (px)}
  \item{layerName}{the name of the layer. Layer name appears on the check box for
                     switching layer of and on in output htm.}
  \item{layerNameEnabled}{controls whether the layer is enabled (shown) when the map is loaded. Default is TRUE}
  \item{layerGroupName}{character string to show above control in legend. Useful for identifying groups of map layers. Default is FALSE to indicate no grouping text to display}
  \item{control.width}{the width of the part of htm for controlling layer  (legend, opacity,...)
                           Can be in percent or in pixels (px).}
  \item{control.height}{the height of the part of htm for controlling layer  (legend, opacity,...)
                           Can be in percent or in pixels (px).}
  \item{zoom}{the initial Map zoom level.Zoom levels between 0 (the lowest zoom level,
             in which the entire world can be seen on one map)
              to 19 (the highest zoom level, down to individual buildings) are 
              possible within the normal maps view. Argument fitBounds should be FALSE.}
  \item{fitBounds}{sets the maps to fit to the boundary box values of sp object}
  \item{mapTypeId}{the initial Map mapTypeId. 
'HYBRID' - map type displays a transparent layer of major streets on satellite images. 
'ROADMAP' - map type displays a normal street map. 'SATELLITE' - map type displays satellite images. 
'TERRAIN' - map type displays maps with physical features such as terrain and vegetation.}
  \item{disableDoubleClickZoom}{enables or disables zoom and center on double click.}
  \item{draggable}{if FALSE, prevents the map from being dragged}
  \item{keyboardShortcuts}{if FALSE, prevents the map from being controlled by the keyboard.}
  \item{mapTypeControlOptions}{the initial display options for the Map type control.
                                'DEFAULT' - uses the default map type control. 'DROPDOWN_MENU' - 
                                a dropdown menu for the screen realestate conscious. 
                                'HORIZONTAL_BAR' - the standard horizontal radio buttons bar.}
  \item{navigationControl}{enabled or disabled state of the navigation control}
  \item{navigationControlOptions}{the initial display options for the navigation control. 
                                  'ANDROID' -	the small zoom control similar to the one used by the native Maps application on Android.
                                  'DEFAULT' -	the default navigation control. The control which DEFAULT maps to will vary according to map size and other factors. It may change in future versions of the API.
                                   'SMALL' -	the small, zoom only control.
                                 'ZOOM_PAN' -	the larger control, with the zoom slider and pan directional pad.}
  \item{scaleControlOptions}{the initial display options for the scale control.}
  \item{noClear}{if TRUE, do not clear the contents of the Map div.}
  \item{scrollwheel}{if FALSE, disables scrollwheel zooming on the map.}
  \item{streetViewControl}{the initial enabled or disabled state of the Street View pegman control}
  \item{legend}{if FALSE, disables legend.}
  \item{legendOpacityWeightEnabled}{if FALSE, Opacity and Line Weight controls are not shown in legend}
  \item{control}{if FALSE, disables control.}
  \item{InfoWindowControl}{see \url{https://developers.google.com/maps/documentation/javascript/reference#InfoWindowOptions}}
  \item{map}{name of map object in JavaScript}
  \item{funcSetInfoWindowText}{An R callback function that takes the spatial object supplied (SP) and returns a character vector of the same length as the length of SP which contains the text to show in the pop-up InfoWindow. The returned character vector can contain HTML code}
  \item{mapCanvas}{name of map DIV element}
  \item{css}{CSS for mapCanvas and cBoxes}
  \item{api}{see \url{https://developers.google.com/maps/documentation/javascript/tutorial}}
  \item{openMap}{if TRUE map is browesed by default browser}
  \item{...}{optional arguments}
}
\value{This function returns a list with HTML, JavaScript, Google Maps API key and
       else what is necessary for the final web map. The second output is htm file ready to use stored in working direcoty.}
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

m<-segmentGoogleMaps(meuse,zcol=c('zinc','lead','copper'))
}
