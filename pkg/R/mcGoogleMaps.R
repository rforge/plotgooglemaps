mcGoogleMaps <-
  function(SP,
           filename="",
           zcol=1,
           at=NULL,
           add=FALSE,
           previousMap=NULL,
           colPalette=NULL,
           geodesic=TRUE,
           clickable=TRUE,
           draggableMarker=FALSE,
           iconMarker="",
           flat=TRUE,
           visible=TRUE,
           zIndex="null",
           map.width="100%",
           map.height="100%",
           layerName="",
           layerNameEnabled=TRUE,
           layerGroupName=FALSE,
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
           legend=TRUE,
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
           apiMarkerClusterer='',
           openMap= TRUE,
           trafficLayerEnabled = NULL,
           trafficLayerName = "Traffic",
           transitLayerEnabled = NULL,
           transitLayerName = "Transit",
           bicycleLayerEnabled = NULL,
           bicycleLayerName = "Bicycle",
           ...) {
    
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
    if(is.logical(layerGroupName) && layerGroupName || !is.logical(layerGroupName) && !is.character(layerGroupName)) {
      warning("layerGroupName must be FALSE or a character string to use as the group name for this layer")
    }
    
    nameOfSP <- sapply(as.list(substitute({SP})[-1]), deparse)
    nameOfSP <- gsub("\\s","", nameOfSP)
    nameOfSP <- gsub('[!,",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,_,^,`,|,~]', "x", nameOfSP)
    nameOfSP <- gsub('[[]', "X", nameOfSP)
    nameOfSP <- gsub('[]]', "X", nameOfSP)
    temporary <- FALSE 
    ## 6/26/2015: Only use temporary folder (vs. current working directory) if filename is "" AND add is FALSE
    if(filename=="" && !add) {
      filename <- tempfile("map", fileext = c(".html"))
      temporary <- TRUE
    }
    if(apiMarkerClusterer=='') {
      apiMarkerClusterer='https://google-maps-utility-library-v3.googlecode.com/svn/tags/markerclusterer/1.0.2/src/markerclusterer_compiled.js'
    }
    
    if(!(class(SP)[1]=="SpatialPixelsDataFrame" || class(SP)[1]=="SpatialGridDataFrame" || class(SP)[1]=="RasterLayer")) {
      
      SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))
      ## If SP was already in the required projection, use the original SP to preserve @bbox settings
      ## spTranform() will reset @bbox to show entire object
      if(identicalCRS(SP,SP.ll)) {
        SP.ll <- SP
      }
      
    } else if(class(SP)[1]!="RasterLayer") {
      SP <- raster(SP,layer=zcol)
      
      SP <- projectRaster(SP,crs=CRS("+proj=longlat +datum=WGS84"))
      SP <- as(SP,'SpatialGridDataFrame')
      SP.ll <- SP
    }
    
    disableDefaultUI <- FALSE
    Centar <- c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
    sw <- c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
    ne <- c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])
    if(any('data'==slotNames(SP))) {
      attribute <- SP@data[,zcol] 
      for(i in 1:length(SP.ll@data)) {
        if(identical(attribute,SP.ll@data[,i])) {
          attributeName <- names(SP.ll@data)[i]  
        }
      }
    }
    
    if(class(SP.ll)[1]=="SpatialPointsDataFrame") {
      
      if(length(iconMarker)<length(SP.ll@coords[,1])) {
        iconMarker <- iconlabels(attribute,colPalette,at,height=10,icon=TRUE,scale=0.6) 
      } else {
        iconMarker <- iconMarker[1:length(SP.ll@coords[,1])] 
      }
      # warning("iconMarker length is exceeded number of points")
    }
    
    if(class(SP.ll)[1]=="SpatialPoints") {
      
      if(length(iconMarker)<length(SP.ll@coords[,1])) {
        iconMarker <- iconlabels(rep(iconMarker[1],length(SP.ll@coords[,1])),colPalette,at,height=10,icon=TRUE,scale=0.6) 
      } else {
        iconMarker <- iconMarker[1:length(SP.ll@coords[,1])] 
      }
      # warning("iconMarker length is exceeded number of points")
    }
    
    if(layerName=="") {
      layerName <- nameOfSP
    }
    
    if(!is.null(colPalette)) {
      rgbc <- col2rgb(colPalette)
      colPalette <- apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))
    }
    
    if(!is.list(previousMap)) {
      # Creates JavaScript functions for checkbox control, Show, Hide and Toggle control
      functions <- createMapFunctions()
      
      init <- createInitialization(SP.ll,
                                   add=TRUE,
                                   name=map,
                                   divname=mapCanvas,
                                   zoom=zoom,
                                   fitBounds=fitBounds,
                                   mapTypeId=mapTypeId,
                                   disableDefaultUI=disableDefaultUI,
                                   disableDoubleClickZoom=disableDoubleClickZoom,
                                   draggable=draggable,
                                   keyboardShortcuts=keyboardShortcuts,
                                   mapTypeControlOptions=mapTypeControlOptions,
                                   scaleControlOptions=scaleControlOptions,
                                   navigationControl=navigationControl,
                                   navigationControlOptions=navigationControlOptions,
                                   noClear=noClear,
                                   scrollwheel=scrollwheel,
                                   streetViewControl=streetViewControl)
      # Put all functions together
      functions <- paste( functions,init, sep="")  
    } else { 
      functions <- previousMap$functions
    }
    fillOpacity <- 0.5
    
    fjs <- ""
    fjs <- paste(fjs,'\n USGSOverlay.prototype = new google.maps.OverlayView(); \n',sep="")
    fjs <- paste(fjs,'function USGSOverlay(bounds, image, map) {\n      this.bounds_ = bounds;\n      this.image_ = image;\n      this.map_ = map;\n      this.div_ = null;\n      this.setMap(map); }\n',sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.onAdd = function() {\n      var div = document.createElement("DIV");\n      div.style.border = "none";\n      div.style.borderWidth = "0px";\n      div.style.position = "absolute";\n      var img = document.createElement("img");\n      img.src = this.image_;\n      img.style.width = "100%";\n      img.style.height = "100%";\n      div.appendChild(img);\n      this.div_ = div;\n      this.div_.style.opacity = ',fillOpacity,';\n      var panes = this.getPanes();\n      panes.overlayImage.appendChild(this.div_);}\n' ,sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.draw = function() {\n        var overlayProjection = this.getProjection();\n        var sw = overlayProjection.fromLatLngToDivPixel(this.bounds_.getSouthWest());\n        var ne = overlayProjection.fromLatLngToDivPixel(this.bounds_.getNorthEast());\n        var div = this.div_;\n        div.style.left = sw.x + "px";\n        div.style.top = ne.y + "px";\n        div.style.width = (ne.x - sw.x) + "px";\n        div.style.height = (sw.y - ne.y) + "px";} \n' ,sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.onRemove = function() { \n this.div_.parentNode.removeChild(this.div_);} \n' ,sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.hide = function() { if (this.div_) { this.div_.style.visibility = "hidden";} } \n' ,sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.show = function() {if (this.div_) {  this.div_.style.visibility = "visible";}} \n' ,sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.toggle = function() { \n if (this.div_) { \n  if (this.div_.style.visibility == "hidden") {  \n   this.show(); \n  } else { \n  this.hide(); } } } \n' ,sep="")
    fjs <- paste(fjs,'USGSOverlay.prototype.toggleDOM = function() {\n          if (this.getMap()) {\n            this.setMap(null);\n          } else {\n            this.setMap(this.map_);}}\n' ,sep="")
    fjs <- paste(fjs,'function setOpacR(Raster,textname) { \n  opac=0.01*parseInt(document.getElementById(textname).value) \n    Raster.div_.style.opacity= opac } \n' ,sep="")
    
    if(map.width!=control.width & css=="") {
      css <- paste('\n #',mapCanvas,' { float: left;
                 width:', map.width,';
                 height:' , map.height,'; }
                 \n #cBoxes {float: left;
                 width:', control.width,';
                 height: ', control.height,';
                 overflow:auto} \n', sep='') 
    } else if(css=="") {
      css <- paste(' #',mapCanvas,' {min-height: 100%;height:auto; } \n #cBoxes {position:absolute;right:5px; top:50px; background:white}',sep='')
    }
    
    starthtm <- paste('<!DOCTYPE html> \n <html> \n <head> \n <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
                   <meta charset="utf-8"> \n <style type="text/css">  \n html { height: 100% ; font-size: small} \n body { height: 100%; margin: 0px; padding: 0px }
                   ',css,'
                   </style> \n
                   <script type="text/javascript" src="',api,'"> </script>  \n
                   <script type="text/javascript" src="',apiMarkerClusterer,'"> </script>  \n
                   <script language="javascript"> \n ',sep='')
    starthtm <- paste(starthtm, fjs)
    
    ################################################################################
    randNum <- sample(1:10000, 1)
    
    if(class(SP)[1]=="SpatialPoints") {
      
      pointsName <- paste('markers',nameOfSP,randNum, sep="")
      # Create chechk box name for checkbox control
      boxname <- paste(pointsName,'box',sep="")
      
      if(!is.list(previousMap)) {
        # Declare JavaScript variables
        var <- createJsVars(map = map)
        # Declare JavaScript marker variable
        var <- paste0(var,"var marker;\n")
      } else { 
        var <- previousMap$var
      }
      
      var <- paste(var,'var ',pointsName,'=[];\n')
      
      var1 <- ""
      var1 <- paste(sapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) {
                             paste(var1,createMarker(SP.ll@coords[i,],
                                                     title=paste(nameOfSP,
                                                                 ' NO: ',as.character(i),sep=""),
                                                     clickable=clickable,
                                                     draggable=draggableMarker,
                                                     flat=flat,
                                                     visible=visible,
                                                     map=map,
                                                     icon=iconMarker[i],
                                                     zIndex=zIndex),'\n',sep="")
                           }),
                    pointsName,'.push(marker); \n',sep="",collapse='\n')
      
      # Put all variables together
      var <- paste(var,var1)
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         pointsName,',"',boxname,'",',map,');',sep="")
      
      ## Add traffic, transit and/or bicycling layer show/hide JavaScript function calls
      functions <- paste0(functions,
                          createMapLayerJsCalls(map = map,
                                                boxnamePrefix = boxname,
                                                trafficLayerEnabled = trafficLayerEnabled,
                                                transitLayerEnabled = transitLayerEnabled,
                                                bicycleLayerEnabled = bicycleLayerEnabled))      
      
      if(!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n 
                      <div id="',mapCanvas,'"></div>  \n
                      \n <div id="cBoxes"> \n', sep='')
      } else { 
        endhtm<- previousMap$endhtm 
      }
      
      if(control) {
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName,'</b> </td> </tr> </table> \n',sep="")
        
        ## Add optional traffic, transit and bicycle layers
        endhtm <- paste0(endhtm,
                         createMapLayerHtml(map = map,
                                            boxnamePrefix = boxname,
                                            trafficLayerEnabled = trafficLayerEnabled,trafficLayerName = trafficLayerName,
                                            transitLayerEnabled = transitLayerEnabled,transitLayerName = transitLayerName,
                                            bicycleLayerEnabled = bicycleLayerEnabled,bicycleLayerName = bicycleLayerName))
      }
      
    } else if(class(SP)[1]=="SpatialPointsDataFrame") {
      pointsName <- paste('markers',nameOfSP,randNum, sep="")
      # Create chechk box name for checkbox control
      boxname <- paste(pointsName,'box',sep="")
      att <- rep(NA,.5*length(slot(SP.ll,"coords")))
      att1 <- ""
      
      if(!is.list(previousMap)) {
        # Declare JavaScript variables
        var <- createJsVars(map = map)
        # Declare JavaScript marker variable
        var <- paste0(var,"var marker;\n")
      } else { 
        var <- previousMap$var
      }
      var <- paste(var,'var ',pointsName,'=[];\n')
      k <- 1:length(names(SP.ll@data))
      
      ## att is a character vector for marker Titles. Handle embedded single quotes
      att <- paste(lapply(as.list(1:length(SP.ll@coords[,1])), 
                          function(i) paste(names(SP.ll@data)[k],': ',
                                            sapply(k ,function(k) as.character(SP.ll@data[i,k])), collapse="\\r", sep="")))
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
      
      ## Generate code to create Markers
      var1 <- ""
      var1 <- paste(sapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) {
                             paste(var1,createMarker(SP.ll@coords[i,],
                                                     title=paste(att[i],sep=""),
                                                     clickable=clickable,
                                                     draggable=draggableMarker,
                                                     flat=flat,
                                                     map=map,
                                                     visible=visible,
                                                     icon=iconMarker[i],
                                                     zIndex=zIndex),'\n',sep="")
                           }),
                    pointsName,'.push(marker); \n',sep="",collapse='\n')
      
      ## att is a character vector for infowindows. Handle embedded single quotes
      att <- paste(lapply(as.list(1:length(SP.ll@coords[,1])), 
                          function(i) paste(names(SP.ll@data)[k],': ',
                                            sapply(k, function(k) as.character(SP.ll@data[i,k])), collapse=" <br>",sep="")))
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
                             paste(infW,createInfoWindowEventM(Marker=paste(pointsName,'[',i-1,'] ',sep=""),
                                                               content=att[i],
                                                               map=InfoWindowControl$map,
                                                               event=InfoWindowControl$event,
                                                               position= InfoWindowControl$position,
                                                               disableAutoPan = InfoWindowControl$disableAutoPan,
                                                               maxWidth=InfoWindowControl$maxWidth,
                                                               pixelOffset=InfoWindowControl$pixelOffset,
                                                               zIndex=InfoWindowControl$zIndex),' \n')
                           }),
                    collapse='\n') 
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,infW,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         pointsName,',"',boxname,'",',map,');',sep="")
      
      ## Add traffic, transit and/or bicycling layer show/hide JavaScript function calls
      functions <- paste0(functions,
                          createMapLayerJsCalls(map = map,
                                                boxnamePrefix = boxname,
                                                trafficLayerEnabled = trafficLayerEnabled,
                                                transitLayerEnabled = transitLayerEnabled,
                                                bicycleLayerEnabled = bicycleLayerEnabled))      
      
      if(!is.list(previousMap)) {
        endhtm<-paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                      \n <div id="cBoxes"> \n', sep='')              
      } else { 
        endhtm<- previousMap$endhtm 
      }
      
      if(control) {
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName,'</b> </td> </tr> </table> \n',sep="")
        
        if(legend) {
          divLegendImage <- tempfile("Legend")  
          divLegendImage <- substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
          legendboxname <- paste('box',divLegendImage,sep="")
          cxx <- PolyCol(attribute,colPalette,at=at)
          pp <- legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage, temp=temporary)
          
          endhtm <- paste(endhtm,' \n <table style="border-collapse:collapse; width:100%;"> <tr>  <td> <input type="checkbox" checked="checked" id="'
                          ,legendboxname,'" onClick=\'legendDisplay(this,"',
                          divLegendImage,'");\' /> LEGEND </td> </tr> \n <tr> <td>',
                          attributeName,'</td> </tr> \n <tr> <td> <div style="display:block;" id="',
                          divLegendImage,'"> <img src="',divLegendImage,
                          '.png" alt="Legend" height="70%"> </div> \n </td> </tr> </table> \n <hr> \n',sep="") 
        }
        
        ## Add optional traffic, transit and bicycle layers
        endhtm <- paste0(endhtm,
                         createMapLayerHtml(map = map,
                                            boxnamePrefix = boxname,
                                            trafficLayerEnabled = trafficLayerEnabled,trafficLayerName = trafficLayerName,
                                            transitLayerEnabled = transitLayerEnabled,transitLayerName = transitLayerName,
                                            bicycleLayerEnabled = bicycleLayerEnabled,bicycleLayerName = bicycleLayerName))
      }
    } else {
      message("SP object must be SpatialPoints class or SpatialPointsDataFrame!") 
    }
    
    if(!add) {
      functions <- paste(functions,"\n google.maps.event.addListener( " ,map,", 'rightclick', function(event) {
                                  var lat = event.latLng.lat();
                                  var lng = event.latLng.lng();
                                  alert('Lat=' + lat + '; Lng=' + lng);}); " ,"var markerCluster = new MarkerClusterer(",map,"," ,pointsName,"  );",
                         " \n }" )
      endhtm <- paste(endhtm,'</div> \n </body>  \n  </html>')
      write(starthtm, filename,append=FALSE)
      write(var, filename,append=TRUE)
      write(functions, filename,append=TRUE)
      write(endhtm, filename,append=TRUE)
      if(openMap){
        browseURL(filename)
      }
    }
    
    return(list(starthtm=starthtm,var=var,functions=functions,endhtm=endhtm))
  }
