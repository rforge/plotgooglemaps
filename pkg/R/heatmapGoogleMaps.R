##  Generate Google heatmaps from SpatialPointsDataFrames
##  DC 6/9/2015

heatmapGoogleMaps <-
  function(SP,
           filename="",
           zcol=1,
           add=FALSE,
           previousMap=NULL,
           weightedColumn=NULL,
           excludeZeroWeights=TRUE,
           heatmapDissipating=FALSE,
           heatmapRadius=.5,
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
           control=TRUE,
           map="map",
           mapCanvas="map_canvas",
           css = "",
           ## Call with visualization library
           api="https://maps.googleapis.com/maps/api/js?libraries=visualization",
           openMap= TRUE,
           trafficLayerEnabled = NULL,
           trafficLayerName = "Traffic",
           transitLayerEnabled = NULL,
           transitLayerName = "Transit",
           bicycleLayerEnabled = NULL,
           bicycleLayerName = "Bicycle",
           ...){
    
    ###############################################################################
    ################################ heatGoogleMaps ###############################
    ###############################################################################
    
    ## Check new arguments
    if(is.character(weightedColumn) && weightedColumn %in% "") weightedColumn <- NULL   ## Allow weightedColumn=="" to be equivalent to NULL
    if(!is.null(weightedColumn) && !is.numeric(SP@data[,weightedColumn])) stop("weightedColumn must be a numeric field in SP")
    if(!is.logical(excludeZeroWeights)) stop("excludeZeroWeights must be TRUE or FALSE")
    if(!is.logical(heatmapDissipating)) stop("heatmapDissipating must be TRUE or FALSE")
    if(!is.numeric(heatmapRadius)) stop("heatmapRadius must be a numeric value in pixels")
    
    if(!is.logical(layerNameEnabled)) {
      warning("layerNameEnabled must be TRUE to show map layer on map load or FALSE to hide map layer. Using default of TRUE")
      layerNameEnabled <- TRUE
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
    if(filename=="" && !add){
      filename <- tempfile("map", fileext = c(".html"))
      temporary <- TRUE
    }
    
    ## Convert projection to WGS84
    SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))
    ## If SP was already in the required projection, use the original SP to preserve @bbox settings
    ## spTranform() will reset @bbox to show entire object
    if(identicalCRS(SP,SP.ll)) {
      SP.ll <- SP
    }
    
    disableDefaultUI <- FALSE
    Centar <- c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
    sw <- c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
    ne <- c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])
    if(any('data'==slotNames(SP)) ) {
      attribute=SP@data[,zcol] 
      for(i in 1:length(SP.ll@data)) {
        if( identical(attribute,SP.ll@data[,i])) {
          attributeName <- names(SP.ll@data)[i]  
        }
      }
    }
    
    if(layerName=="") {
      layerName <- nameOfSP
    }
    
    if(!is.list(previousMap)) {
      # Creates JavaScript functions for checkbox control, Show, Hide and Toggle control
      functions <- createMapFunctions()
      
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
      functions <- paste( functions,init, sep="")  
    } else { 
      functions <- previousMap$functions
    }
    
    fjs <- ""
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
      css <- paste('\n #',mapCanvas,' { float: left; width:', map.width,'; height:' , map.height,'; }',
                   '\n #cBoxes {float: left; width:', control.width,';height: ', control.height,';overflow:auto} \n', sep='') 
    } else if(css=="") {
      css <- paste(' #',mapCanvas,' {min-height: 100%;height:auto; } \n #cBoxes {position:absolute;right:5px; top:50px; background:white}',sep='')
    }
    
    starthtm <- paste('<!DOCTYPE html> \n <html> \n <head> \n <meta name="viewport" content="initial-scale=1.0, user-scalable=no" /> ',
                      '<meta charset="utf-8"> \n <style type="text/css">  \n html { height: 100% ; font-size: small} \n body { height: 100%; margin: 0px; padding: 0px }',css,
                      '</style> \n ',
                      '<script type="text/javascript" src="',api,'"> </script>  \n ',
                      '<script language="javascript"> \n ',sep='')
    starthtm <- paste(starthtm, fjs)
    
    ################################################################################
    randNum <- sample(1:10000, 1)
    
    if(class(SP)[1]=="SpatialPoints") {
      
      pointsName<-paste('markers',nameOfSP,randNum, sep="")
      # Create check box name for checkbox control
      boxname<-paste(pointsName,'box',sep="")
      
      if(!is.list(previousMap)) {
        # Declare JavaScript variables
        var <- createJsVars(map = map)
        # Declare JavaScript marker variable
        var <- paste0(var,"var marker;\n")
      } else {
        var <- previousMap$var
      }
      
      ## Define data array for heatmap
      var <- paste(var,"\nvar ",pointsName,"Data = [ \n",sep="")
      var1 <- paste(sapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) {
                             paste("new google.maps.LatLng(",
                                   SP.ll@coords[i,2],",",
                                   SP.ll@coords[i,1],")",
                                   sep="")
                           }), 
                    collapse=', \n')
      
      # Add closing array delimiter ("];")
      var <- paste(var,var1,"]; \n",sep="")
      
      ## Create heatmap layer with any options
      var <- paste(var,"\n",
                   "var ",pointsName," = new google.maps.visualization.HeatmapLayer({ \n",
                   "  data: ",pointsName,"Data, \n",
                   "  dissipating: ",tolower(as.character(heatmapDissipating)),",\n",
                   "  radius: ",heatmapRadius,"\n",
                   "}); \n",sep="")
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,
                         ifelse(layerNameEnabled,'\n showHeatmap(','\n hideHeatmap('),
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
        endhtm <- previousMap$endhtm 
      }
      
      if(control) {
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclickHeatmap(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName,'</b> </td> </tr> </table> ',sep="")
        
        ## Add optional traffic, transit and bicycle layers
        endhtm <- paste0(endhtm,
                         createMapLayerHtml(map = map,
                                            boxnamePrefix = boxname,
                                            trafficLayerEnabled = trafficLayerEnabled,trafficLayerName = trafficLayerName,
                                            transitLayerEnabled = transitLayerEnabled,transitLayerName = transitLayerName,
                                            bicycleLayerEnabled = bicycleLayerEnabled,bicycleLayerName = bicycleLayerName))
      }
      
    } else if(class(SP)[1]=="SpatialPointsDataFrame") {
      
      ##############################################################################################
      ## Handle SpatialPointsDataFrame
      ##############################################################################################
      pointsName <- paste('markers',nameOfSP,randNum, sep="")
      # Create check box name for checkbox control
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
      
      ## Start heatmap array definition
      var <- paste(var,"\nvar ",pointsName,"Data = [ \n",sep="")
      
      ## Define data array for heatmap
      ## If weightedColumn is not NULL, create weighted data points
      ## If excludeZeroWeights is TRUE, don't include points where weighted column value is zero
      ## If excludeZeroWeights is FALSE, include points (without weights)      
      var1 <- paste(unlist(sapply(as.list(1:length(SP.ll@coords[,1])), 
                                  function(i) {
                                    if(is.null(weightedColumn)) {
                                      return(paste("new google.maps.LatLng(",
                                                   SP.ll@coords[i,2],",",
                                                   SP.ll@coords[i,1],")",
                                                   sep=""))
                                    } else if(is.na(SP.ll@data[i,weightedColumn]) ||
                                              (excludeZeroWeights && SP.ll@data[i,weightedColumn]==0)) {
                                      return(character())
                                    } else {
                                      return(paste("{location: new google.maps.LatLng(",
                                                   SP.ll@coords[i,2],",",
                                                   SP.ll@coords[i,1],"), ",
                                                   "weight: ",SP.ll@data[i,weightedColumn],"}",
                                                   sep=""))
                                    }
                                  })), 
                    collapse=', \n',sep="")
      
      # Add closing array delimiter ("];")
      var <- paste(var,var1,"]; \n",sep="")
      
      ## Create heatmap layer with any options
      var <- paste(var,"\n",
                   "var ",pointsName," = new google.maps.visualization.HeatmapLayer({ \n",
                   "  data: ",pointsName,"Data, \n",
                   "  dissipating: ",tolower(as.character(heatmapDissipating)),",\n",
                   "  radius: ",heatmapRadius,"\n",
                   "}); \n",sep="")
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,
                         ifelse(layerNameEnabled,'\n showHeatmap(','\n hideHeatmap('),
                         pointsName,',"',boxname,'",',map,');',sep="")
      
      ## Add traffic, transit and/or bicycling layer show/hide JavaScript function calls
      functions <- paste0(functions,
                          createMapLayerJsCalls(map = map,
                                                boxnamePrefix = boxname,
                                                trafficLayerEnabled = trafficLayerEnabled,
                                                transitLayerEnabled = transitLayerEnabled,
                                                bicycleLayerEnabled = bicycleLayerEnabled))      
      
      if(!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')              
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control) {
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclickHeatmap(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName ,'</b> </td> </tr> </table> ',sep="")
        
        ## Add optional traffic, transit and bicycle layers
        endhtm <- paste0(endhtm,
                         createMapLayerHtml(map = map,
                                            boxnamePrefix = boxname,
                                            trafficLayerEnabled = trafficLayerEnabled,trafficLayerName = trafficLayerName,
                                            transitLayerEnabled = transitLayerEnabled,transitLayerName = transitLayerName,
                                            bicycleLayerEnabled = bicycleLayerEnabled,bicycleLayerName = bicycleLayerName))
      }
      
    }
    else  {
      message("SP object must be Spatial class!") 
    }
    
    if(!add) {
      functions <- paste(functions,"\n google.maps.event.addListener( " ,map,", 'rightclick', function(event) {",
                         "var lat = event.latLng.lat();",
                         "var lng = event.latLng.lng();",
                         "alert('Lat=' + lat + '; Lng=' + lng);}); " , " \n }")
      
      endhtm <- paste(endhtm,'</div> \n </body>  \n  </html>')
      write(starthtm, filename,append=FALSE)
      write(var, filename,append=TRUE)
      write(functions, filename,append=TRUE)
      write(endhtm, filename,append=TRUE)
      if(openMap) { 
        browseURL(filename)
      }
    }
    
    return(list(starthtm=starthtm,var=var,functions=functions,endhtm=endhtm))
  }
