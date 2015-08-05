##  Modified plotGoogleMaps to use Google symbols as markers
##  DC 6/3/2015 Initial version uses circles for SpatialPointsDataFrames only

symbolGoogleMaps <-
  function(SP,
           filename="",
           zcol=1,
           add=FALSE,
           previousMap=NULL,
           symbolPath="google.maps.SymbolPath.CIRCLE",
           minScale=5,
           zcolIndexBase=mean(sqrt(SP@data[,zcol]/pi)*2,na.rm = TRUE),
           symbolFillColor="red",
           symbolFillOpacity=0.7,
           strokeColor="white",
           strokeOpacity=1,
           strokeWeight=1,
           clickable=TRUE,
           draggableMarker=FALSE,
           flat=TRUE,
           visible=TRUE,
           zIndex="null",
           map.width="100%",
           map.height="100%",
           layerName="",
           layerNameEnabled=TRUE,
           control.width="100%",
           control.height="100%",
           zoom=15,
           fitBounds=TRUE,
           mapTypeId = "HYBRID",
           disableDoubleClickZoom =FALSE,
           draggable= TRUE ,
           keyboardShortcuts=TRUE,
           mapTypeControlOptions='DEFAULT',
           navigationControl=TRUE,
           navigationControlOptions='DEFAULT',
           scaleControlOptions= 'STANDARD',
           noClear=FALSE,
           scrollwheel =TRUE     ,
           streetViewControl= FALSE,
           control=TRUE,
           InfoWindowControl=list(map=map, event="click",position="event.latLng",
                                  disableAutoPan=FALSE, maxWidth=330,pixelOffset="null",
                                  zIndex="null") ,
           funcSetInfoWindowText=NULL,
           funcSetMarkerTitleText=NULL,
           map="map",
           mapCanvas="map_canvas",
           css = "",
           api="https://maps.googleapis.com/maps/api/js?libraries=visualization",
           openMap= TRUE,
           ...) {
    
    ###############################################################################
    ############################### symbolGoogleMaps ##############################
    ###############################################################################
    
    #  wd=getwd()
    
    ## Check new arguments
    if(!is.logical(layerNameEnabled)) {
      warning("layerNameEnabled must be TRUE to show map layer on map load or FALSE to hide map layer. Using default of TRUE")
      layerNameEnabled <- TRUE
    }
    if(!is.null(funcSetInfoWindowText) && !is.function(funcSetInfoWindowText)) {
      warning("funcSetInfoWindowText must be NULL or a function that accepts a sp object and returns a character vector of InfoWindow marker strings. Using defaults.")
      funcSetInfoWindowText <- NULL
    }
    if(!is.null(funcSetMarkerTitleText) && !is.function(funcSetMarkerTitleText)) {
      warning("funcSetMarkerTitleText must be NULL or a function that accepts a sp object and returns a character vector of marker title strings. Using defaults.")
      funcSetMarkerTitleText <- NULL
    }
     
    nameOfSP<-sapply(as.list(substitute({SP})[-1]), deparse)
    nameOfSP<-gsub("\\s","", nameOfSP)
    nameOfSP<-gsub('[!,",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,_,^,`,|,~]', "x", nameOfSP)
    nameOfSP<-gsub('[[]', "X", nameOfSP)
    nameOfSP<-gsub('[]]', "X", nameOfSP)
    temporary = FALSE 
    ## 6/26/2015: Only use temporary folder (vs. current working directory) if filename is "" AND add is FALSE
    if(filename=="" && !add){
      filename <- tempfile("map", fileext = c(".html"))
      temporary = TRUE
    }
    
    if(class(SP)[1]=="RasterLayer"){
      SP<- projectRaster( SP , crs=CRS("+proj=longlat +datum=WGS84"))
      SP <- as(SP , 'SpatialGridDataFrame')
      SP.ll <- SP
    }
    if ((class(SP)[1]=="SpatialPixelsDataFrame" || class(SP)[1]=="SpatialGridDataFrame" ) ){
      r <- raster(SP, layer=zcol)
      SP<- projectRaster( r , crs=CRS("+proj=longlat +datum=WGS84"))
      SP <- as(SP , 'SpatialGridDataFrame')
      SP.ll <- SP
    } else{
      SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))
    }
    
    disableDefaultUI=FALSE
    Centar=c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
    sw<-c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
    ne<-c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])
    if(any('data'==slotNames(SP)) ){
      attribute=SP@data[,zcol] 
      for(i in 1:length(SP.ll@data)) {
        if( identical(attribute,SP.ll@data[,i])){
          attributeName<-names(SP.ll@data)[i]  }
      }
    }
    
    if(layerName==""){
      layerName=nameOfSP}
    
    if(strokeColor!=""){
      rgbc<-col2rgb(strokeColor)
      strokeColor<-rgb(rgbc[1],rgbc[2],rgbc[3],maxColorValue=255) }
    
    if (!is.list(previousMap)) {
      functions<-""
      
      # Creating functions for checkbox control, Show , Hide and Toggle control
      # Set of JavaScript functionalities
      funs <- createMapFunctions()
      
      functions <- paste(functions,funs,sep="")
      
      init <- createInitialization(SP.ll,
                                   add=T,
                                   name=map,
                                   divname=mapCanvas,
                                   zoom=zoom,
                                   fitBounds=fitBounds,
                                   mapTypeId = mapTypeId,
                                   disableDefaultUI=disableDefaultUI,
                                   disableDoubleClickZoom =disableDoubleClickZoom,
                                   draggable= draggable ,
                                   keyboardShortcuts=keyboardShortcuts,
                                   mapTypeControlOptions=mapTypeControlOptions,
                                   scaleControlOptions=scaleControlOptions,
                                   navigationControl=navigationControl,
                                   navigationControlOptions=navigationControlOptions,
                                   noClear=noClear,
                                   scrollwheel=scrollwheel,
                                   streetViewControl= streetViewControl)
      # Put all functions together
      functions<-paste( functions,init, sep="")  
      
    } else { 
      functions<- previousMap$functions
    }
    
    fjs=""
    
    # fjs<-paste(fjs,'\n USGSOverlay.prototype = new google.maps.OverlayView(); \n',sep="")
    # fjs<-paste(fjs,'function USGSOverlay(bounds, image, map) {\n      this.bounds_ = bounds;\n      this.image_ = image;\n      this.map_ = map;\n      this.div_ = null;\n      this.setMap(map); }\n',sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.onAdd = function() {\n      var div = document.createElement("DIV");\n      div.style.border = "none";\n      div.style.borderWidth = "0px";\n      div.style.position = "absolute";\n      var img = document.createElement("img");\n      img.src = this.image_;\n      img.style.width = "100%";\n      img.style.height = "100%";\n      div.appendChild(img);\n      this.div_ = div;\n      this.div_.style.opacity = ',fillOpacity,';\n      var panes = this.getPanes();\n      panes.overlayImage.appendChild(this.div_);}\n' ,sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.draw = function() {\n        var overlayProjection = this.getProjection();\n        var sw = overlayProjection.fromLatLngToDivPixel(this.bounds_.getSouthWest());\n        var ne = overlayProjection.fromLatLngToDivPixel(this.bounds_.getNorthEast());\n        var div = this.div_;\n        div.style.left = sw.x + "px";\n        div.style.top = ne.y + "px";\n        div.style.width = (ne.x - sw.x) + "px";\n        div.style.height = (sw.y - ne.y) + "px";} \n' ,sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.onRemove = function() { \n this.div_.parentNode.removeChild(this.div_);} \n' ,sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.hide = function() { if (this.div_) { this.div_.style.visibility = "hidden";} } \n' ,sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.show = function() {if (this.div_) {  this.div_.style.visibility = "visible";}} \n' ,sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.toggle = function() { \n if (this.div_) { \n  if (this.div_.style.visibility == "hidden") {  \n   this.show(); \n  } else { \n  this.hide(); } } } \n' ,sep="")
    # fjs<-paste(fjs,'USGSOverlay.prototype.toggleDOM = function() {\n          if (this.getMap()) {\n            this.setMap(null);\n          } else {\n            this.setMap(this.map_);}}\n' ,sep="")
    
    if(map.width!=control.width & css=="") {
      css= paste('\n #',mapCanvas,' { float: left;
 width:', map.width,';
 height:' , map.height,'; }
\n #cBoxes {float: left;
width:', control.width,';
height: ', control.height,';
overflow:auto} \n', sep='') 
    } else if (css=="") {
      css=paste(' #',mapCanvas,' {min-height: 100%;height:auto; } \n #cBoxes {position:absolute;right:5px; top:50px; background:white}',sep='')
    }
    
    
    starthtm=paste('<!DOCTYPE html> \n <html> \n <head> \n <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
 <meta charset="utf-8"> \n <style type="text/css">  \n html { height: 100% ; font-size: small} \n body { height: 100%; margin: 0px; padding: 0px }
',css,'
</style> \n
 <script type="text/javascript" src="',api,'"> </script>  \n
 <script language="javascript"> \n ',sep='')
    starthtm<-paste(starthtm, fjs)
    
    ################################################################################
    randNum = sample(1:10000, 1)
    
    ## symbolGoogleMaps() only applies to SpatialPointsDataFrame
    if(class(SP)[1]=="SpatialPointsDataFrame") {
      pointsName<-paste('markers',nameOfSP,randNum, sep="")
      # Create check box name for checkbox control
      boxname<-paste(pointsName,'box',sep="")
      att<-rep(NA,.5*length(slot(SP.ll,"coords")))
      att1=""
      
      if(!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<-paste(' var marker \n var ',map,' \n')
        # Create all markers and store them in markersArray - PointsName
      } else { 
        var<-previousMap$var
      }
      
      var<-paste(var,'var ',pointsName,'=[] ;')
      var1=""
      k = 1:length(names(SP.ll@data))
      
      ## att is a character vector for marker Titles. Handle embedded single quotes
      att <- paste(lapply(as.list(1:length(SP.ll@coords[,1])), 
                          function(i) paste(names(SP.ll@data)[k],": ",
                                            sapply(k, function(k) as.character(SP.ll@data[i,k])), collapse="\\r", sep="")))
      att <- gsub("'","\\\\'",att)
      
      ## If funcSetMarkerTitleText argument is specified, call it to specify marker title text
      if(is.function(funcSetMarkerTitleText)) {
        attCustom <- funcSetMarkerTitleText(SP.ll)
        ## Make sure that attCustom is a character vector of the same length as att
        if(!is.character(attCustom) || length(attCustom) != length(att)) {
          warning("funcSetMarkerTitleText function returned class of ",class(attCustom),", expected character vector of length ",length(att),". Using default text for marker titles")
        } else {
          ## Use custom values generated by funcSetInfoWindowText
          att <- gsub("'","\\\\'",attCustom)
        }
      }
      
      ## Generate code to create Symbols
      var1 <- paste(sapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) paste(var1,
                                             createSymbol(lonlat=SP.ll@coords[i,],
                                                          zcolData=SP@data[i,zcol],
                                                          title=paste(att[i],sep=""),
                                                          clickable=clickable,
                                                          draggable=draggableMarker,
                                                          flat=flat,
                                                          map=map,
                                                          visible=visible,
                                                          zIndex=ifelse(length(zIndex)==nrow(SP.ll@data),zIndex[i],zIndex[1]),
                                                          
                                                          symbolPath=symbolPath,
                                                          symbolFillColor=symbolFillColor,
                                                          symbolFillOpacity=symbolFillOpacity,
                                                          symbolScale=sqrt(SP@data[i,zcol]/pi)*2 / zcolIndexBase * minScale,
                                                          ## Add linear scale option?  SP@data[i,zcol]/zcolIndexBase * minScale,
                                                          strokeColor=strokeColor,
                                                          strokeWeight=strokeWeight,
                                                          strokeOpacity=strokeOpacity),
                                             '\n',sep="")),
                    pointsName,'.push(marker); \n',sep="",collapse='\n')
      
      ## att is a character vector of HTML strings to display in InfoWindow. Handle embedded single quotes
      att <- paste(lapply(as.list(1:length(SP.ll@coords[,1])), 
                          function(i) paste(names(SP.ll@data)[k],': ',
                                            sapply(k, function(k) as.character(SP.ll@data[i,k])), collapse=" <br>", sep="")))
      att <- gsub("'","\\\\'",att)
      
      ## If funcSetInfoWindowText argument is specified, call it to specify InfoWindow text
      if(is.function(funcSetInfoWindowText)) {
        attCustom <- funcSetInfoWindowText(SP.ll)
        ## Make sure that attCustom is a character vector of the same length as att
        if(!is.character(attCustom) || length(attCustom) != length(att)) {
          warning("funcSetInfoWindowText function returned class of ",class(attCustom),", expected character vector of length ",length(att),". Using default text for InfoWindow")
        } else {
          ## Use custom values generated by funcSetInfoWindowText
          att <- gsub("'","\\\\'",attCustom)
        }
      }
      
      var <- paste(var,var1)
      infW <- ""
      
      infW <- paste(lapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) {
                             paste(infW,
                                   createInfoWindowEventM(Marker=paste(pointsName,'[',i-1,'] ',sep=""),
                                                          content=att[i],
                                                          map=InfoWindowControl$map,
                                                          event=InfoWindowControl$event,
                                                          position= InfoWindowControl$position,
                                                          disableAutoPan = InfoWindowControl$disableAutoPan,
                                                          maxWidth=InfoWindowControl$maxWidth,
                                                          pixelOffset=InfoWindowControl$pixelOffset,
                                                          zIndex=InfoWindowControl$zIndex),' \n') }),collapse='\n')
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,infW,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         pointsName,',"',boxname,'",',map,');',sep="")
      
      if(!is.list(previousMap)) {
        endhtm<-paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')              
      } else { endhtm<- previousMap$endhtm }
      
      if(control) {
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName ,'<b> </td> </tr> </table>',sep="")
      }

    }
    
    ## SP object is not supported for plotting using symbols
    else {
      message("SP object must be SpatialPointsDataFrame class!") 
    }
    
    
    
    if(!add) {
      functions<- paste(functions,"\n google.maps.event.addListener( " ,map,", 'rightclick', function(event) {
    var lat = event.latLng.lat();
    var lng = event.latLng.lng();
    alert('Lat=' + lat + '; Lng=' + lng);}); " , " \n }" )
      endhtm<-paste(endhtm,'</div> \n </body>  \n  </html>')
      write(starthtm, filename,append=F)
      write(var, filename,append=TRUE)
      write(functions, filename,append=TRUE)
      write(endhtm, filename,append=TRUE)
      if(openMap) {browseURL(filename)}
    }
    
    return(list(starthtm=starthtm,var=var,functions=functions,endhtm=endhtm))
    
  }
