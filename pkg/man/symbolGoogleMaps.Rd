\name{symbolGoogleMaps}
\alias{symbolGoogleMaps}
\title{Create a plot of SpatialPointsDataFrame data using SVG symbols in Google Maps}
\description{Uses Google Maps to visualize data in a SpatialPointsDataFrame as a collection of customizable SVG symbols. See the following for more information: https://developers.google.com/maps/documentation/javascript/symbols}
\usage{
symbolGoogleMaps(SP, 
                 filename = '',
                 zcol = 1,
                 add = FALSE, 
                 previousMap = NULL,
                 symbolPath = 'google.maps.SymbolPath.CIRCLE',
                 minScale = 5,
                 zcolIndexBase = mean(sqrt(SP@data[,zcol]/pi)*2,na.rm = TRUE),
                 symbolFillColor = "red",
                 symbolFillOpacity = 0.7,
                 strokeColor = "white",
                 strokeOpacity = 1,
                 strokeWeight = 1,
                 clickable = TRUE,
                 draggableMarker = FALSE,
                 flat = TRUE,
                 visible = TRUE,
                 zIndex = "null",
                 map.width = "100\%",
                 map.height = "100\%",
                 layerName = "",
                 layerNameEnabled = TRUE,
                 layerGroupName = FALSE,
                 control.width = "100\%",
                 control.height = "100\%",
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
                 control = TRUE,
                 InfoWindowControl = list(map=map, event="click",position="event.latLng",
                                          disableAutoPan=FALSE, maxWidth=330,
                                          pixelOffset="null",zIndex="null"),
                 funcSetInfoWindowText=NULL,
                 funcSetMarkerTitleText=NULL,
                 map = "map",
                 mapCanvas = "map_canvas",
                 css = "",                                     
                 api = "https://maps.googleapis.com/maps/api/js?libraries=visualization",
                 openMap = TRUE,
                 trafficLayerEnabled = NULL,
                 trafficLayerName = "Traffic",
                 transitLayerEnabled = NULL,
                 transitLayerName = "Transit",
                 bicycleLayerEnabled = NULL,
                 bicycleLayerName = "Bicycle",
                 ...) }
\arguments{
  \item{SP}{object of \link[sp]{Spatial-class} with
          associated coordinate reference systems}
  \item{filename}{the name of the output htm or html file. If filename is not defined
         the function creates temporary file.}
  \item{zcol}{variable column name, or column number after removing spatial coordinates 
             from x@data: 1 refers to the first non-coordinate column}
  \item{add}{logical; if TRUE the result of the function will be a list stored as variable 
             in the R. It is possible to combine more layers in the one plot, previously saved
             output from plotGoogleMaps should be given in the previousMap attribute.}
  \item{previousMap}{previously saved variable from plotGoogleMaps or bubbleGoogleMaps when attribute add is TRUE.
                     It is solution for combining more than one layers in the one plot.}

  \item{symbolPath}{Google Maps symbol path. Default is google.maps.SymbolPath.CIRCLE. If 1 value is specified, it is used for all data points. Can also specify a vector of different symbol paths, one for each point}
  \item{minScale}{Number of pixels to use for the minimum value in zcol}
  \item{zcolIndexBase}{Denominator used to normalize zcol values. Default is mean(sqrt(SP@data[,zcol]/pi)*2,na.rm = TRUE)}
  \item{symbolFillColor}{Fill color for symbol interior. Default is 'red'. Can also supply a vector to specify a different color for each symbol}
  \item{symbolFillOpacity}{Opacity of symbols. Range is 0-1, default is 0.7.  Can also supply a vector to specify a different opacity for each symbol}
  \item{strokeColor}{Stroke color for symbol outlines. Default is 'white'. Can also supply a vector to specify a different stroke color for each symbol}
  \item{strokeOpacity}{Stroke opacity for symbol outlines. Range is 0-1, default is 1. Can also supply a vector to specify a different opacity for each symbol}
  \item{strokeWeight}{Stroke weight for symbol outlines. Default is 1. Can also supply a vector to specify a different stroke weight for each symbol}
  \item{clickable}{indicates whether this symbol (point) handles click events}
  \item{draggableMarker}{if TRUE, the symbol can be dragged}
  \item{flat}{if TRUE, the marker shadow will not be displayed}
  \item{visible}{if TRUE, the marker is visible}
  \item{zIndex}{the zIndex compared to other polygons. This can be specified as a vector of length(SP).}

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
  \item{streetViewControl}{The initial enabled or disabled state of the Street View pegman control}
  \item{control}{if FALSE, disables control.}
  \item{InfoWindowControl}{see \url{https://developers.google.com/maps/documentation/javascript/reference#InfoWindowOptions}}
  \item{funcSetInfoWindowText}{An R callback function that takes the spatial object supplied (SP) and returns a character vector of the same length as the length of SP which contains the text to show in the pop-up InfoWindow. The returned character vector can contain HTML code}
  \item{funcSetMarkerTitleText}{An R callback function that takes the spatial object supplied (SP) and returns a character vector of the same length as the length of SP which contains the text to show as tool-tip text for a marker. The returned character vector must only consist of text}
  \item{map}{name of map object in JavaScript}
  \item{mapCanvas}{name of map DIV element}
  \item{css}{CSS for mapCanvas and cBoxes}
  \item{api}{see \url{https://developers.google.com/maps/documentation/javascript/tutorial}}
  \item{openMap}{if TRUE map is opened by default browser}
  \item{trafficLayerEnabled}{If TRUE, display current traffic conditions. If NULL (default), do not add traffic map layer (for backwards compatibility)}
  \item{trafficLayerName}{Layer name to use for traffic layer in map legend. Default is "Traffic". Has no effect if trafficLayerEnabled is NULL}
  \item{transitLayerEnabled}{If TRUE, display transit map layer. If NULL (default), do not add transit map layer (for backwards compatibility)}
  \item{transitLayerName}{Layer name to use for transit layer in map legend. Default is "Transit". Has no effect if transitLayerEnabled is NULL}
  \item{bicycleLayerEnabled}{If TRUE, display bicycle map layer. If NULL (default), do not add bicycle map layer (for backwards compatibility)}
  \item{bicycleLayerName}{Layer name to use for bicycle layer in map legend. Default is "Bicycle". Has no effect if bicycleLayerEnabled is NULL}
  \item{...}{optional arguments}
}
\value{This function returns a list with HTML, JavaScript, Google Maps API key and
       else what is necessary for the final web map. The second output is html file ready to use.}
\author{Milan Kilibarda <kili@grf.bg.ac.rs>, Dan Cheng <dan.cheng@insightanalyticsinc.com>}
\seealso{
\link{bubbleGoogleMaps},
\link{ellipseGoogleMaps},
\link{heatmapGoogleMaps},
\link{plotGoogleMaps},
\link{segmentGoogleMaps}
}
\examples{
# Data preparation
# Point data
data(meuse)
require(maptools)
coordinates(meuse)<-~x+y
proj4string(meuse) <- CRS('+init=epsg:28992')

# Create symbol map of point data based upon the values 
# of cadmium measurements
m<-symbolGoogleMaps(meuse,zcol = "cadmium") 

}
