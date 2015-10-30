plotGoogleMaps <-
  function(SP,
           filename="",
           zcol=1,
           at=NULL,
           add=FALSE,
           previousMap=NULL,
           colPalette=NULL,
           strokeColor="",
           strokeOpacity=1,
           fillOpacity=0.7,
           strokeWeight=1,
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
           legendOpacityWeightEnabled=TRUE,
           control=TRUE,
           InfoWindowControl=list(map=map, event="click",position="event.latLng",
                                  disableAutoPan=FALSE, maxWidth=330,
                                  pixelOffset="null", zIndex="null"),
           funcSetInfoWindowText=NULL,
           funcSetMarkerTitleText=NULL,
           map="map",
           mapCanvas="map_canvas",
           css = "",
           api="https://maps.googleapis.com/maps/api/js?libraries=visualization",
           openMap= TRUE,
           ...){

    ###############################################################################
    ################################ plotGoogleMaps ###############################
    ###############################################################################
    
    ## Check new arguments
    if(!is.logical(layerNameEnabled)) {
      warning("layerNameEnabled must be TRUE to show map layer on map load or FALSE to hide map layer. Using default of TRUE")
      layerNameEnabled <- TRUE
    }
    if(!is.logical(legendOpacityWeightEnabled)) {
      warning("legendOpacityWeightEnabled must be TRUE to show Opacity and Line Weight controls or FALSE to hide. Using default of TRUE")
      legendOpacityWeightEnabled <- TRUE
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
    
    
    if(class(SP.ll)[1]=="SpatialPointsDataFrame"){
      
      if(length(iconMarker)<length(SP.ll@coords[,1]) )  {
        iconMarker=iconlabels(attribute,colPalette,at,height=10,icon=TRUE,scale=0.6) }else{
          iconMarker=iconMarker[1:length(SP.ll@coords[,1])] }
      # warning("iconMarker length is exceeded number of points")
    }
    
    if(class(SP.ll)[1]=="SpatialPoints"){
      
      if(length(iconMarker)<length(SP.ll@coords[,1]) )  {
        iconMarker=iconlabels(rep(iconMarker[1],length(SP.ll@coords[,1]) ),colPalette,at,height=10,icon=TRUE,scale=0.6) }else{
          iconMarker=iconMarker[1:length(SP.ll@coords[,1])] }
      # warning("iconMarker length is exceeded number of points")
    }
    
    if(layerName==""){
      layerName=nameOfSP}
    
    if(strokeColor!=""){
      rgbc<-col2rgb(strokeColor)
      strokeColor<-rgb(rgbc[1],rgbc[2],rgbc[3],maxColorValue=255) }
    
    if(!is.null(colPalette)){
      rgbc<-col2rgb(colPalette)
      colPalette<-apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))}
    
    if (!is.list(previousMap)) {
      functions<-""
      
      # Creating functions for checkbox control, Show , Hide and Toggle control
      # Set of JavaScript functionalities
      funs <- createMapFunctions()
      
      functions<-paste(functions,funs,sep="")
      
      init<-createInitialization(SP.ll,
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
                                 scrollwheel =scrollwheel    ,
                                 streetViewControl= streetViewControl)
      # Put all functions together
      functions<-paste( functions,init, sep="")  
      
    } else { 
      functions<- previousMap$functions
    }
    
    fjs=""
    
    fjs<-paste(fjs,'\n USGSOverlay.prototype = new google.maps.OverlayView(); \n',sep="")
    fjs<-paste(fjs,'function USGSOverlay(bounds, image, map) {\n      this.bounds_ = bounds;\n      this.image_ = image;\n      this.map_ = map;\n      this.div_ = null;\n      this.setMap(map); }\n',sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.onAdd = function() {\n      var div = document.createElement("DIV");\n      div.style.border = "none";\n      div.style.borderWidth = "0px";\n      div.style.position = "absolute";\n      var img = document.createElement("img");\n      img.src = this.image_;\n      img.style.width = "100%";\n      img.style.height = "100%";\n      div.appendChild(img);\n      this.div_ = div;\n      this.div_.style.opacity = ',fillOpacity,';\n      var panes = this.getPanes();\n      panes.overlayImage.appendChild(this.div_);}\n' ,sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.draw = function() {\n        var overlayProjection = this.getProjection();\n        var sw = overlayProjection.fromLatLngToDivPixel(this.bounds_.getSouthWest());\n        var ne = overlayProjection.fromLatLngToDivPixel(this.bounds_.getNorthEast());\n        var div = this.div_;\n        div.style.left = sw.x + "px";\n        div.style.top = ne.y + "px";\n        div.style.width = (ne.x - sw.x) + "px";\n        div.style.height = (sw.y - ne.y) + "px";} \n' ,sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.onRemove = function() { \n this.div_.parentNode.removeChild(this.div_);} \n' ,sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.hide = function() { if (this.div_) { this.div_.style.visibility = "hidden";} } \n' ,sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.show = function() {if (this.div_) {  this.div_.style.visibility = "visible";}} \n' ,sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.toggle = function() { \n if (this.div_) { \n  if (this.div_.style.visibility == "hidden") {  \n   this.show(); \n  } else { \n  this.hide(); } } } \n' ,sep="")
    fjs<-paste(fjs,'USGSOverlay.prototype.toggleDOM = function() {\n          if (this.getMap()) {\n            this.setMap(null);\n          } else {\n            this.setMap(this.map_);}}\n' ,sep="")
    
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
    
    if (class(SP)[1]=="SpatialPoints"){
      
      ################################## SpatialPoints ######################################
      
      pointsName<-paste('markers',nameOfSP,randNum, sep="")
      # Create check box name for checkbox control
      boxname<-paste(pointsName,'box',sep="")
      
      if (!is.list(previousMap)) {
        var <- ""
        # Declare variables in JavaScript marker and map
        var <- paste(' var marker \n var ', map,' \n')
        # Create all markers and store them in markersArray - PointsName
      } else { 
        var <- previousMap$var
      }
      
      var <- paste(var,'var ',pointsName,'=[]; \n')
      var1 <- ""
      
      ## Generate code to create Markers
      var1 <- paste(sapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) paste(var1,createMarker(SP.ll@coords[i,],
                                                               title=paste(nameOfSP,
                                                                           ' NO: ',as.character(i),sep=""),
                                                               clickable=clickable,
                                                               draggable=draggableMarker,
                                                               flat=flat,
                                                               visible=visible,
                                                               map=map,
                                                               icon=iconMarker[i],
                                                               zIndex=zIndex),'\n',sep="")), 
                    pointsName,'.push(marker); \n',sep="",collapse='\n')
      
      # Put all variables together
      var<-paste(var,var1)
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         pointsName,',"',boxname,'",',map,');',sep="")
      
      if (!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control){
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName ,'</b> </td> </tr> </table> \n ',sep="")
      }
      
    }
    else if   (class(SP)[1]=="SpatialPointsDataFrame") {
      
      ################################## SpatialPointsDataFrame ######################################
      
      pointsName<-paste('markers',nameOfSP,randNum, sep="")
      # Create check box name for checkbox control
      boxname<-paste(pointsName,'box',sep="")
      att<-rep(NA,.5*length(slot(SP.ll,"coords")))
      att1=""
      
      if (!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<-paste(' var marker \n var ',map,' \n')
        # Create all markers and store them in markersArray - PointsName
      } else { 
        var<-previousMap$var
      }
      var<-paste(var,'var ',pointsName,'=[]; \n')
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
      
      ## Generate code to create Markers
      var1 <- paste(sapply(as.list(1:length(SP.ll@coords[,1])), 
                           function(i) paste(var1,createMarker(SP.ll@coords[i,],
                                                               title=paste(att[i],sep=""),
                                                               clickable=clickable,
                                                               draggable=draggableMarker,
                                                               flat=flat,
                                                               map=map,
                                                               visible=visible,
                                                               icon=iconMarker[i],
                                                               zIndex=zIndex),'\n',sep="")),
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
      
      if (!is.list(previousMap)) {
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
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',pointsName,',"',boxname,'",',map,');\' /> <b>', layerName ,'</b> </td> </tr> </table> \n',sep="")
        
        if(legend) {
          divLegendImage <- tempfile("Legend")  
          divLegendImage <- substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
          legendboxname <- paste('box',divLegendImage,sep="")
          cxx <- PolyCol(attribute,colPalette,at=at)
          pp <- legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage, temp=temporary)
          
          endhtm <- paste(endhtm,' \n <table> <tr> <td> <input type="checkbox" checked="checked" id="'
                          ,legendboxname,'" onClick=\'legendDisplay(this,"',
                          divLegendImage,'");\' /> LEGEND </td> </tr> \n <tr> <td>',
                          attributeName,'</td> </tr> \n <tr> <td> <div style="display:block;" id="',
                          divLegendImage,'"> <img src="',divLegendImage,
                          '.png" alt="Legend" height="70%"> </div> \n </td> </tr> \n </table> \n  <hr> \n',sep="") 
        }
      }
    }
    else if   (class(SP)[1]=="SpatialLines"){
      
      ################################## SpatialLines ######################################
      
      lineName<-paste('line',nameOfSP,randNum, sep="")
      boxname<-paste(lineName,'box',sep="")
      textname<- paste(lineName,'text',sep="")
      textnameW<-paste(lineName,'W',sep="")
      attribute=seq(1,length(SP.ll@lines))
      
      if (!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<-paste(' \n var', map, ' \n')
        # Create all markers and store them in markersArray - lineName
      } else { 
        var<-previousMap$var
      }
      
      var<-paste(var,'var ',lineName,'=[] ; \n')
      var1=""
      
      if(length(colPalette)==length(SP.ll@lines)){
        xx<- colPalette}else{
          cxx<-PolyCol(1:SP.ll@lines,colPalette,at=at)
          xx<- cxx$cols
        }
      
      var1 <- paste(lapply(as.list(1:length(SP.ll@lines)), 
                           function(i) paste(var1,createLine(SP.ll@lines[[i]],
                                                             strokeColor=xx[i],
                                                             strokeOpacity=strokeOpacity,
                                                             strokeWeight=strokeWeight,
                                                             geodesic=geodesic,
                                                             clickable=clickable,
                                                             map=map,
                                                             zIndex=zIndex),'\n',sep="")), 
                    lineName,'.push(line); \n',sep="",collapse='\n')
      
      # Put all variables together
      var<-paste(var,var1)
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         lineName,',"',boxname,'",',map,');',sep="")
      
      if (!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')                
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control){
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',lineName,',"',boxname,'",',map,');\' /> <b>', layerName,' </b> </td> </tr> \n',sep="")
        
        ## Show Opacity and Line Weight controls in legend?
        if(legendOpacityWeightEnabled) {
          endhtm <- paste(endhtm,' \n <tr> <td> \n <input type="text" id="',
                          textname,'" value="100" onChange=\'setOpacL(',
                          lineName,',"',textname,'")\' size=3 /> Opacity (0-100 %) </td> </tr> \n',sep="")
          endhtm <- paste(endhtm,' \n <tr> <td> \n <input type="text" id="',
                          textnameW,'" value="1" onChange=\'setLineWeight(',
                          lineName,',"',textnameW,'")\' size=3 /> Line weight (px) </td> </tr> </table> \n',sep="")
        } else
          endhtm <- paste(endhtm,'</table> \n',sep="")
      }
      
    }
    else if   (class(SP)[1]=="SpatialLinesDataFrame")     {
      
      ################################## SpatialLinesDataFrame ######################################
      
      lineName<-paste('line',nameOfSP,randNum, sep="")
      boxname<-paste(lineName,'box',sep="")
      textname<- paste(lineName,'text',sep="")
      textnameW<-paste(lineName,'W',sep="")
      att<-rep(NA,length(slot(SP.ll,"lines")))
      
      
      att1=""
      
      
      if (!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<-paste(' \n var ',map,' \n') 
        # Create all markers and store them in markersArray - PointsName
      } else {
        var<-previousMap$var
      }
      
      var<-paste(var,'var ',lineName,'=[] ; \n')
      var1=""
      
      cxx<-PolyCol(attribute,colPalette,at=at)
      xx<- cxx$cols
      
      if(length(strokeWeight)==length(attribute)){
        swxx=strokeWeight
      }else{swxx<-weightATR(attribute,strokeWeight)}
      
      if(length(attribute)==length(colPalette)){xx= colPalette}
      
      var1 <- paste(sapply(1:length(SP.ll@lines), 
                           function(i) paste(var1,createLine(SP.ll@lines[[i]],
                                                             strokeColor=xx[i]  ,
                                                             strokeOpacity=strokeOpacity,
                                                             strokeWeight=swxx[i],
                                                             geodesic=geodesic,
                                                             clickable=clickable,
                                                             zIndex=zIndex ),'\n',sep="")),
                    lineName,'.push(line); \n',sep="",collapse='\n')
      
      k = 1:length(names(SP.ll@data))
      
      ## att is a character vector of HTML strings to display in InfoWindow
      att <- paste(lapply(as.list(1:length(SP.ll@lines)), 
                          function(i) paste(names(SP.ll@data),': ',
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
      
      # Put all variables together
      var<-paste(var,var1)
      
      infW <- ""
      
      infW <- paste(lapply(as.list(1:length(SP.ll@lines)), 
                           function(i) paste(infW,createInfoWindowEvent(Line_or_Polygon=paste(lineName,'[',i-1,'] ',sep=""),
                                                                        content=att[i],
                                                                        map=InfoWindowControl$map,
                                                                        event=InfoWindowControl$event,
                                                                        position= InfoWindowControl$position,
                                                                        disableAutoPan = InfoWindowControl$disableAutoPan,
                                                                        maxWidth=InfoWindowControl$maxWidth,
                                                                        pixelOffset=InfoWindowControl$pixelOffset,
                                                                        zIndex=InfoWindowControl$zIndex),
                                             ' \n')), collapse='\n')
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,infW,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         lineName,',"',boxname,'",',map,');',sep="")
      
      if (!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')  
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control){
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '"onClick=\'boxclick(this,',lineName,',"',boxname,'",',map,');\' /> <b>', layerName, '</b> </td> </tr> \n',sep="")
        
        ## Show Opacity and Line Weight controls in legend?
        if(legendOpacityWeightEnabled) {
          endhtm <- paste(endhtm,' \n <tr> <td> <input type="text" id="',
                          textname,'" value="100" onChange=\'setOpacL(',
                          lineName,',"',textname,'")\' size=3 /> 
                           Opacity (0-100 %) </td> </tr> \n',sep="")
          endhtm <- paste(endhtm,'<tr> <td> <input type="text" id="',
                          textnameW,'" value="1" onChange=\'setLineWeight(',
                          lineName,',"',textnameW,'")\' size=3 /> 
                           Line weight (pixels) </td> </tr> \n',sep="")
        }
      }  
      if(legend){
        divLegendImage<-tempfile("Legend")  
        divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
        legendboxname<-paste('box',divLegendImage,sep="")
        pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage, temp=temporary)
        endhtm <- paste(endhtm,'<tr> <td> <input type="checkbox" checked="checked" id="',
                        legendboxname,'" onClick=\'legendDisplay(this,"',
                        divLegendImage,'");\' /> LEGEND </td> </tr> \n <tr> <td>',
                        attributeName,'</td> </tr> \n
                           <tr> <td> <div style="display:block;" id="',
                        divLegendImage,'"> <img src="',divLegendImage,
                        '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> </table> \n   <hr> \n',sep="") 
      } else {
        endhtm <- paste(endhtm, '</table> \n ',sep="")
      }
      
    }
    else if   (class(SP)[1]=="SpatialPolygons")             {
      
      ################################## SpatialPolygons ######################################
      
      polyName<-paste('poly',nameOfSP,randNum, sep="")
      boxname<-paste(polyName,'box',sep="")
      textname<- paste(polyName,'text',sep="")
      textnameW<-paste(polyName,'W',sep="")
      attribute=seq(1,length(SP.ll@polygons))
      
      
      if (!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<-   paste(' \n var', map, ' \n')
        # Create all markers and store them in markersArray - PointsName
      } else {
        var<-previousMap$var
      }
      
      var<-paste(var,'var ',polyName,'=[] ; \n')
      var1=""
      if(length(colPalette)==length(SP.ll@polygons)){
        xx<- colPalette}else{
          cxx<-PolyCol(1:length(SP.ll@polygons),colPalette,at=at)
          xx<- cxx$cols
        }
      
      if(length(strokeWeight)==length(SP.ll@polygons)){
        swxx=strokeWeight
      }else{swxx=rep(strokeWeight,length(SP.ll@polygons)) }
      
      var1 <- paste( lapply(as.list(1:length(SP.ll@polygons)), 
                            function(i) paste(var1,createPolygon(SP.ll@polygons[[i]],
                                                                 fillColor=xx[i],
                                                                 strokeColor=strokeColor,
                                                                 strokeOpacity=strokeOpacity,
                                                                 strokeWeight=swxx[i],
                                                                 geodesic=geodesic,
                                                                 map=map,
                                                                 clickable=clickable,
                                                                 fillOpacity=fillOpacity,
                                                                 zIndex=zIndex),'\n',sep="")), 
                     polyName,'.push(polygon); \n',sep="",collapse='\n')
      
      # Put all variables together
      var<-paste(var,var1)
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         polyName,',"',boxname,'",',map,'); \n',sep="")
      
      if (!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')  
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control){
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',polyName,',"',boxname,'",',map,');\' /> <b>', layerName,'</b> </td> </tr> \n',sep="")
        
        ## Show Opacity and Line Weight controls in legend?
        if(legendOpacityWeightEnabled) {
          endhtm <- paste(endhtm,' \n <tr> <td> <input type="text" id="',
                          textname,'" value="50" onChange=\'setOpac(',
                          polyName,',"',textname,'")\' size=3 /> 
                             Opacity (0-100 %) </td> </tr> \n',sep="")
          endhtm <- paste(endhtm,' \n <tr> <td> <input type="text" checked="checked" id="',
                          textnameW,'" value="1" onChange=\'setLineWeight(',
                          polyName,',"',textnameW,'")\' size=3 /> Line weight
                               (pixels) </td> </tr> </table>\n',sep="")
        } else {
          endhtm <- paste(endhtm,'</table> \n',sep="")
        }
      }
    }
    
    else if   (class(SP)[1]=="SpatialPolygonsDataFrame") {
      
      ################################## SpatialPolygonsDataFrame ######################################
      
      polyName<-paste('poly',nameOfSP,randNum, sep="")
      boxname<-paste(polyName,'box',sep="")
      textname<- paste(polyName,'text',sep="")
      textnameW<-paste(polyName,'W',sep="")
      
      att<-rep(NA,length(slot(SP.ll,"polygons")))
      att1=""
      
      if (!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<- paste(' \n var', map, ' \n')
        # Create all markers and store them in markersArray - PointsName
      } else {
        var<-previousMap$var
      }
      
      var<-paste(var,'var ',polyName,'=[] ; \n')
      var1=""
      
      cxx<-PolyCol(attribute,colPalette,at=at)
      xx<- cxx$cols
      
      
      if(length(attribute)==length(colPalette)){xx= colPalette}
      
      
      if(length(strokeWeight)==length(SP.ll@polygons)){
        swxx=strokeWeight
      }else{swxx=rep(strokeWeight,length(SP.ll@polygons)) }       
      
      var1 <- paste(lapply(as.list(1:length(SP.ll@polygons)), 
                           function(i) paste(var1,createPolygon(SP.ll@polygons[[i]],
                                                                fillColor=xx[i],
                                                                strokeColor=strokeColor,
                                                                strokeOpacity=strokeOpacity,
                                                                strokeWeight=swxx[i],
                                                                geodesic=geodesic,
                                                                map=map,
                                                                clickable=clickable,
                                                                fillOpacity=fillOpacity,
                                                                zIndex=zIndex),'\n',sep="")),
                    polyName,'.push(polygon); \n',sep="",collapse='\n')  
      
      
      k = 1:length(names(SP.ll@data))
      
      ## att is a character vector of HTML strings to display in InfoWindow
      att <- paste(lapply(as.list(1:length(SP.ll@polygons)), 
                          function(i) paste(names(SP.ll@data),': ',
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
      
      var<-paste(var,var1)
      
      infW <- ""
      infW <- paste(lapply(as.list(1:length(SP.ll@polygons)), 
                           function(i) paste(infW,createInfoWindowEvent(Line_or_Polygon=paste(polyName,'[',i-1,'] ',sep=""),
                                                                        content=att[i],
                                                                        map=InfoWindowControl$map,
                                                                        event=InfoWindowControl$event,
                                                                        position= SP.ll[i,]@polygons[[1]]@labpt,
                                                                        disableAutoPan = InfoWindowControl$disableAutoPan,
                                                                        maxWidth=InfoWindowControl$maxWidth,
                                                                        pixelOffset=InfoWindowControl$pixelOffset,
                                                                        zIndex=InfoWindowControl$zIndex),
                                             ' \n')), collapse='\n')  
      
      
      ## If layerNameEnabled is FALSE, hide this map layer on load
      functions <- paste(functions,infW,
                         ifelse(layerNameEnabled,'\n showO(','\n hideO('),
                         polyName,',"',boxname,'",',map,');',sep="")
      
      if (!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')  
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control){
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclick(this,',polyName,',"',boxname,'",',map,');\' /> <b>', layerName,'</b> </td> </tr> \n',sep="")
        
        ## Show Opacity and Line Weight controls in legend?
        if(legendOpacityWeightEnabled) {
          endhtm <- paste(endhtm,' \n <tr> <td> <input type="text" id="',
                          textname,'" value="50" onChange=\'setOpac(',
                          polyName,',"',textname,'")\' size=3 /> 
                                   Opacity (0-100 %) </td> </tr> \n',sep="")
          endhtm <- paste(endhtm,' \n <tr>  <td> <input type="text" 
                                   id="',textnameW,'" value="1" onChange=\'
                                   setLineWeight(',polyName,',"',textnameW,'")\' 
                                   size=3 /> Line weight (pixels) </td> </tr> \n ',sep="")
        } 
      }
      if(legend){
        divLegendImage<-tempfile("Legend")  
        divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
        legendboxname<-paste('box',divLegendImage,sep="")
        pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage, temp=temporary)
        
        endhtm <- paste(endhtm,' \n <tr> <td> <input type="checkbox"  checked="checked" id="'
                        ,legendboxname,'" onClick=\'legendDisplay(this,"',
                        divLegendImage,'");\' /> LEGEND </td> </tr> \n <tr> <td>',
                        attributeName,'</td> </tr> \n <tr> <td> <div style="display:block;" id="',
                        divLegendImage,'"> <img src="',divLegendImage,
                        '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n <hr> \n',sep="") 
      } else { 
        endhtm <- paste(endhtm, '</table> \n ',sep="") 
      }
    }
    
    else if ( class(SP)[1]=="SpatialGridDataFrame" ) {
      
      ################################## SpatialGridDataFrame ######################################
      
      rasterName <- tempfile("grid")
      rasterName <- substr(rasterName, start=regexpr("grid",rasterName),stop=nchar(rasterName))
      boxname <- paste(rasterName,'box',sep="")
      divLegendImage <- tempfile("Legend")  
      divLegendImage <- substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
      legendboxname <- paste('box',divLegendImage,sep="")
      textname <- paste(rasterName,'text',sep="")
      
      if (!is.list(previousMap)) {
        var<-""
        # Declare variables in JavaScript marker and map
        var<-paste(' \n var', map,' \n')
        # Create all markers and store them in markersArray - PointsName
      } else {
        var<-previousMap$var
      }
      var<-paste(var,'\n var ',rasterName,'imageBounds = new google.maps.LatLngBounds
                (new google.maps.LatLng(',sw[1],',',sw[2],'),
                new google.maps.LatLng(',ne[1],',',ne[2],')); \n',sep="")
      var<-paste(var,'var ', rasterName ,'= new USGSOverlay(',rasterName,'imageBounds',',"',rasterName,  '.png",', map,'); \n',sep="")
      
      if(is.null(colPalette)){
        pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
        xx<-colPalette<-as.character(substr(pal(12),1,7)) } else { xx<-colPalette<-as.character(substr(colPalette,1,7)) }
      
      if(is.factor(attribute)){
        
        if(length(colPalette)!=nlevels(attribute)) {
          pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
          xx<-colPalette<- as.character(substr(pal(nlevels(attribute)),1,7))    }
        
        #pp<-rasterLegend(attribute,colPalette=xx,legendName=divLegendImage)
        SP$arg1111<-as.numeric(SP[attributeName]@data[,1])
      } else {
        SP$arg1111<-SP[zcol]@data[,1]
      }
      
      cxx=PolyCol(SP[attributeName]@data[,1],colPalette,at)
      
      xx=cxx$cols
      pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage, temp=temporary)            
      
      SGqk <- GE_SpatialGrid(SP.ll)
      png(filename =ifelse(temporary,paste(tempdir(),'/',rasterName,'.png',sep=""),paste(rasterName,'.png',sep="") )
          , width=10*SGqk$width, height=10*SGqk$height, bg="transparent")
      par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
      image(as.image.SpatialGridDataFrame(SP["arg1111"]),breaks=cxx$brks,  col=cxx$col.uniq)
      
      dev.off()
      
      
      functions<-paste(functions,'showR(',rasterName,',"',boxname,'",',map,');',sep="")
      
      if (!is.list(previousMap)) {
        endhtm <- paste('</script> \n </head> \n <body onload="initialize()"> \n  <div id="',mapCanvas,'"></div>  \n
                           \n <div id="cBoxes"> \n', sep='')  
      } else { 
        endhtm <- previousMap$endhtm 
      }
      
      if(control){
        if(is.character(layerGroupName)) {
          endhtm <- paste(endhtm,'<table style="border-collapse:collapse; width:100%;"> <tr> <td> <b>',
                          paste(layerGroupName,collapse = " <br> "),'</b> </td> </tr> </table> \n',sep="")
        }
        
        endhtm <- paste(endhtm,'<table> <tr> <td> <input type="checkbox" id="',boxname,
                        '" onClick=\'boxclickR(this,',rasterName,',"',boxname,'",',map,');\' /> <b>', layerName,'</b> </td> </tr> \n',sep="")
        
        endhtm <- paste(endhtm,'  \n <tr> <td> <input type="text" id="',
                        textname,'" value="50" onChange=\'setOpacR(',
                        rasterName,',"',textname,'")\' size=3 />  Opacity (0-100 %)</td> </tr> \n',sep="")
      }
      if(legend){
        
        pp <-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage, temp=temporary)
        
        endhtm <- paste(endhtm,' \n <tr> <td> <input type="checkbox"  checked="checked" id="',
                        legendboxname,'" onClick=\'legendDisplay(this,"',
                        divLegendImage,'");\' /> LEGEND </td> </tr>  <tr> <td>',
                        attributeName,'</td> </tr> \n <tr> <td> <div style="display:block;" id="',
                        divLegendImage,'"> <img src="',divLegendImage,
                        '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n <hr> \n',sep="")
      } else { 
        endhtm <- paste(endhtm, '</table> \n ',sep="") 
      }
      
    }
    else  {
      message("SP object must be Spatial class!") }
    
    if (!add) {
      functions<- paste(functions,
                        "\n google.maps.event.addListener( " ,map,", 'rightclick', function(event) {
    var lat = event.latLng.lat();
    var lng = event.latLng.lng();
    alert('Lat=' + lat + '; Lng=' + lng);}); ",
                        " \n }" )
      
      endhtm <- paste(endhtm,'</div> \n  </body> \n   </html>')
      write(starthtm, filename,append=F)
      write(var, filename,append=TRUE)
      write(functions, filename,append=TRUE)
      write(endhtm, filename,append=TRUE)
      if(openMap) {browseURL(filename)}
    }
    
    return(list(starthtm=starthtm,var=var,functions=functions,endhtm=endhtm))
    
  }
