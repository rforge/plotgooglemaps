plotGoogleMaps <-
function(SP,
                            filename="",
                            zcol=1,
                            at=NULL,
                            add=FALSE,
                            previousMap=NULL,
                            colPalette=NULL,
                            strokeColor="#FFAA00",
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
                               openMap= TRUE
                               ){
 

    
 ################################ plotGoogleMaps ###############################
 ###############################################################################
 ###############################################################################
 
#  wd=getwd()
 

SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))

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
nameOfSP<-sapply(as.list(substitute({SP})[-1]), deparse)
nameOfSP<-gsub("\\s","", nameOfSP)
nameOfSP<-gsub('[!,",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,^,`,|,~]', "x", nameOfSP)
nameOfSP<-gsub('[[]', "X", nameOfSP)
nameOfSP<-gsub('[]]', "X", nameOfSP)
if(filename==""){
filename <- paste(nameOfSP,'.htm',sep="")}
 
 if(class(SP.ll)[1]=="SpatialPointsDataFrame"){
   
           if(length(iconMarker)<length(SP.ll@coords[,1]) )  {
                 iconMarker=iconlabels(attribute,colPalette,at,height=10,icon=TRUE,scale=0.6) }else{
                                     iconMarker=iconMarker[1:length(SP.ll@coords[,1])] }
      # warning("iconMarker length is exceeded number of points")
                                             }

if(layerName==""){
layerName=nameOfSP}
 

 
 legenda=T

if(strokeColor!=""){
rgbc<-col2rgb(strokeColor)
strokeColor<-rgb(rgbc[1],rgbc[2],rgbc[3],maxColorValue=255) }

if(!is.null(colPalette)){
rgbc<-col2rgb(colPalette)
colPalette<-apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))}

if (!is.list(previousMap)) {
functions<-""
# Creating functions for checkbox comtrol, Show , Hide and Toggle control
# Set of JavaScript functionalities
functions<-paste(functions,' function setOpacL(MLPArray,textname) {
opacity=0.01*parseInt(document.getElementById(textname).value) \n
for (var i = 0; i < MLPArray.length; i++) { MLPArray[i].setOptions
({strokeOpacity: opacity}); } } \n',sep="")
functions<-paste(functions,'function showO(MLPArray,boxname) { \n
for (var i = 0; i < MLPArray.length; i++) { \n MLPArray[i].setMap(map); } \n
 document.getElementById(boxname).checked = true; } \n ',sep="")
functions<-paste(functions,'function hideO(MLPArray,boxname) { \n
for (var i = 0; i < MLPArray.length; i++) { \n MLPArray[i].setMap(null);} \n
 document.getElementById(boxname).checked = false; } \n ',sep="")
functions<-paste(functions,'function boxclick(box,MLPArray,boxname)
{ \n if (box.checked) { \n showO(MLPArray,boxname); \n }
 else { \n hideO(MLPArray,boxname);} } \n',sep="")
functions<-paste(functions,' function setOpac(MLPArray,textname)
 {opacity=0.01*parseInt(document.getElementById(textname).value) \n for
  (var i = 0; i < MLPArray.length; i++) { MLPArray[i].setOptions({strokeOpacity:
  opacity, fillOpacity: opacity}); } } \n',sep="")
functions<-paste(functions,' function setLineWeight(MLPArray,textnameW)
 {weight=parseInt(document.getElementById(textnameW).value) \n
 for (var i = 0; i < MLPArray.length; i++)
  { MLPArray[i].setOptions({strokeWeight: weight}); } } \n',sep="")
functions<-paste(functions,'function legendDisplay(box,divLegendImage){ \n
element = document.getElementById(divLegendImage).style; \n if (box.checked)
 { element.display="block";} else {  element.display="none";}} \n',sep="")
functions<-paste(functions,'function showR(R,boxname) { R.setMap(map);
 \n document.getElementById(boxname).checked = true; } \n ',sep="")
functions<-paste(functions,' function hideR(R,boxname) { R.setMap(null);
 \n document.getElementById(boxname).checked = false; } \n ',sep="")
functions<-paste(functions,'function boxclickR(box,R,boxname) { \n if (box.checked)
{ \n showR(R,boxname);\n } else { \n hideR(R,boxname);} }  \n',sep="")
functions<-paste(functions,'function legendDisplay(box,divLegendImage){
\n element = document.getElementById(divLegendImage).style; \n if (box.checked)
 { element.display="block";} else {  element.display="none";}} \n',sep="")

 
init<-createInitialization(SP.ll,
                               add=T,
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
functions<-paste( functions,init, sep="")  }else{ functions<- previousMap$functions}

fjs=""

fjs<-paste(fjs,'\n USGSOverlay.prototype = new google.maps.OverlayView(); \n',sep="")
fjs<-paste(fjs,'function USGSOverlay(bounds, image, map) {\n      this.bounds_ = bounds;\n      this.image_ = image;\n      this.map_ = map;\n      this.div_ = null;\n      this.setMap(map); }\n',sep="")
fjs<-paste(fjs, 'USGSOverlay.prototype.onAdd = function() {\n      var div = document.createElement("DIV");\n      div.style.border = "none";\n      div.style.borderWidth = "0px";\n      div.style.position = "absolute";\n      var img = document.createElement("img");\n      img.src = this.image_;\n      img.style.width = "100%";\n      img.style.height = "100%";\n      div.appendChild(img);\n      this.div_ = div;\n      this.div_.style.opacity = ',fillOpacity,';\n      var panes = this.getPanes();\n      panes.overlayImage.appendChild(this.div_);}\n' ,sep="")
fjs<-paste(fjs,'USGSOverlay.prototype.draw = function() {\n        var overlayProjection = this.getProjection();\n        var sw = overlayProjection.fromLatLngToDivPixel(this.bounds_.getSouthWest());\n        var ne = overlayProjection.fromLatLngToDivPixel(this.bounds_.getNorthEast());\n        var div = this.div_;\n        div.style.left = sw.x + "px";\n        div.style.top = ne.y + "px";\n        div.style.width = (ne.x - sw.x) + "px";\n        div.style.height = (sw.y - ne.y) + "px";} \n' ,sep="")
fjs<-paste(fjs,'USGSOverlay.prototype.onRemove = function() { \n this.div_.parentNode.removeChild(this.div_);} \n' ,sep="")
fjs<-paste(fjs,'USGSOverlay.prototype.hide = function() { if (this.div_) { this.div_.style.visibility = "hidden";} } \n' ,sep="")
fjs<-paste(fjs,'USGSOverlay.prototype.show = function() {if (this.div_) {  this.div_.style.visibility = "visible";}} \n' ,sep="")
fjs<-paste(fjs,'       USGSOverlay.prototype.toggle = function() { \n if (this.div_) { \n  if (this.div_.style.visibility == "hidden") {  \n   this.show(); \n  } else { \n  this.hide(); } } } \n' ,sep="")
fjs<-paste(fjs,' USGSOverlay.prototype.toggleDOM = function() {\n          if (this.getMap()) {\n            this.setMap(null);\n          } else {\n            this.setMap(this.map_);}}\n' ,sep="")
fjs<-paste(fjs,' function setOpacR(Raster,textname) { \n  opac=0.01*parseInt(document.getElementById(textname).value) \n    Raster.div_.style.opacity= opac } \n' ,sep="")

if(map.width!=control.width){
  css= paste('\n #map_canvas { float: left;
 width:', map.width,';
 height:' , map.height,'; }
\n #cBoxes {float: left;
width:', control.width,';
height: ', control.height,';
overflow:auto} \n') 
}else{
  css=' #map_canvas {min-height: 100%;
height:auto; }
 
 #cBoxes {position:absolute;
right:5px;
top:50px;
background:white}'
}

 
starthtm=paste('<html> \n <head> \n <meta name="viewport" content="initial-scale=1.0, user-scalable=no" />
\n <style type="text/css">  \n html { height: 100% } \n body { height: 100%; margin: 0px; padding: 0px }
',css,'
</style> \n
 <script type="text/javascript" src="http://maps.google.com/maps/api/js?sensor=false"> </script>  \n
 <script language="javascript"> \n ')
starthtm<-paste(starthtm, fjs)

################################################################################

if (class(SP)[1]=="SpatialPoints"){

            pointsName<-paste('markers',nameOfSP,sep="")
            # Create chechk box name for checkbox control
            boxname<-paste(nameOfSP,'box',sep="")
            
            
            if (!is.list(previousMap)) {
            var<-""
            # Declare variables in JavaScript marker and map
            var<-c(' var marker \n var map \n')
            # Create all markers and store them in markersArray - PointsName
            }else{ var<-previousMap$var}
            
            var<-paste(var,'var ',pointsName,'=[] ;')
            var1=""

            
            var1<- paste( sapply(as.list(1:length(SP.ll@coords[,1])), function(i) paste(var1,createMarker(SP.ll@coords[i,],
                                                                                                          title=paste(nameOfSP,
                                                                                                                      ' NO: ',as.character(i),sep=""),
                                                                                                          clickable=clickable,
                                                                                                          draggable=draggableMarker,
                                                                                                          flat=flat,
                                                                                                          visible=visible,
                                                                                                          icon=iconMarker[i],
                                                                                                          zIndex=zIndex),'\n',sep="") 
                                   )
                          ,pointsName,'.push(marker); \n',sep="",collapse='\n')
            
            # Put all variables together
            var<-paste(var,var1)
            
            
            functions<-paste(functions,'showO(',pointsName,',"',boxname,'");',sep="")
            
            if (!is.list(previousMap)) {
             endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                           \n <div id="cBoxes"> \n')
            } else { endhtm<- previousMap$endhtm }
            
            endhtm<- paste(endhtm,'<input type="checkbox" id="',boxname,
            '" onClick=\'boxclick(this,',pointsName,',"',boxname,'");\' /> <b>', layerName ,'<b> <hr/>',sep="")
                                                          }
else if   (class(SP)[1]=="SpatialPointsDataFrame") {
              pointsName<-paste('markers',nameOfSP,sep="")
              # Create chechk box name for checkbox control
              boxname<-paste(nameOfSP,'box',sep="")
              att<-rep(NA,.5*length(slot(SP.ll,"coords")))
              att1=""
              
              
              if (!is.list(previousMap)) {
              var<-""
              # Declare variables in JavaScript marker and map
              var<-c(' var marker \n var map \n')
              # Create all markers and store them in markersArray - PointsName
              }else{ var<-previousMap$var}
              var<-paste(var,'var ',pointsName,'=[] ;')
              var1=""
              k = 1:length(names(SP.ll@data))
              
              att<- paste ( lapply(as.list(1:length(SP.ll@coords[,1])), function(i) paste(names(SP.ll@data)[k],':',sapply(k ,function(k) as.character(SP.ll@data[i,k])) ,'\\r',
                                                                                                                          collapse="")  )   )
              
              var1<- paste( sapply(as.list(1:length(SP.ll@coords[,1])), function(i) paste(var1,createMarker(SP.ll@coords[i,],
                                                                                                            title=paste(att[i],sep=""),
                                                                                                            clickable=clickable,
                                                                                                            draggable=draggableMarker,
                                                                                                            flat=flat,
                                                                                                            visible=visible,
                                                                                                            icon=iconMarker[i],
                                                                                                            zIndex=zIndex),'\n',sep="") 
                                    )
                            ,pointsName,'.push(marker); \n',sep="",collapse='\n')
              
           
              
              att<- paste ( lapply(as.list(1:length(SP.ll@coords[,1])), function(i) paste(names(SP.ll@data)[k],':',sapply(k ,function(k) as.character(SP.ll@data[i,k]))
                                                                                          ,'<br>', collapse="")  )   )
                                
              
              var<-paste(var,var1)
              infW<-""
              
              infW<- paste ( lapply(as.list(1:length(SP.ll@coords[,1])), function(i) 
                paste(infW,createInfoWindowEventM(Marker=paste(pointsName,'[',i-1,'] ',sep=""),content=att[i]),' \n')  )  ,collapse='\n' )                  
              

              
              functions<-paste(functions,infW,'showO(',pointsName,',"',boxname,'");',sep="")
              
              if (!is.list(previousMap)) {
                endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                           \n <div id="cBoxes"> \n')              
                } else { endhtm<- previousMap$endhtm }
              
              endhtm<- paste(endhtm,'<input type="checkbox" id="',boxname,'" onClick=\'boxclick(this,',pointsName,',"',boxname,'");\' /> <b>', layerName ,'<b> <hr />',sep="")

                divLegendImage<-tempfile("Legend")  
                divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
                legendboxname<-paste('box',divLegendImage,sep="")
              
              cxx<-PolyCol(attribute,colPalette,at=at)
              pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage)

                
              endhtm<- paste(endhtm,' \n <table> <tr>  <td> <input type="checkbox"  checked="checked" id="'
                             ,legendboxname,'" onClick=\'legendDisplay(this,"',
                             divLegendImage,'");\' /> LEGEND </td> </tr>  <tr> <td>',
                             attributeName,'</td> </tr>
                                    <tr> <td> <div style="display:block;" id="',
                                   divLegendImage,'"> <img src="',divLegendImage,
                             '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n  <hr> \n',sep="") 
              
                                                             }
else if   (class(SP)[1]=="SpatialLines"){
                lineName<-paste('line',nameOfSP,sep="")
                boxname<-paste(nameOfSP,'box',sep="")
                textname<- paste(nameOfSP,'text',sep="")
                textnameW<-paste(textname,'W',sep="")
                attribute=seq(1,length(SP.ll@lines))
                
                
                
                if (!is.list(previousMap)) {
                var<-""
                # Declare variables in JavaScript marker and map
                var<-c(' \n var map \n')
                # Create all markers and store them in markersArray - PointsName
                }else{ var<-previousMap$var}
                
                var<-paste(var,'var ',lineName,'=[] ; \n')
                var1=""
                
                if(length(colPalette)==length(SP.ll@lines)){
                  xx<- colPalette}else{
                    cxx<-PolyCol(1:SP.ll@lines,colPalette,at=at)
                    xx<- cxx$cols
                  }
                
               
                
                
                var1<- paste( lapply(as.list(1:length(SP.ll@lines)), function(i) paste(var1,createLine(SP.ll@lines[[i]],
                                                                                                       strokeColor=xx[i],
                                                                                                       strokeOpacity=strokeOpacity,
                                                                                                       strokeWeight=strokeWeight,
                                                                                                       geodesic=geodesic,
                                                                                                       clickable=clickable,
                                                                                                       zIndex=zIndex),'\n',sep="") 
                                     )
                              ,lineName,'.push(line); \n',sep="",collapse='\n')
                
                
                
                # Put all variables together
                var<-paste(var,var1)
                
                
                
                functions<-paste(functions,'showO(',lineName,',"',boxname,'");',sep="")
                
                if (!is.list(previousMap)) {
                  endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                           \n <div id="cBoxes"> \n')               
                  } else { endhtm<- previousMap$endhtm }
                
                endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input type="checkbox" id="',boxname,'" onClick=\'boxclick(this,',lineName,',"',boxname,'");\' /> ', layerName ,' </td> </tr>  \n',sep="")
                endhtm<- paste(endhtm,' \n <tr>  <td> \n <input type="text" id="',textname,'" value="100" onChange=\'setOpacL(',lineName,',"',textname,'")\' size=3 /> Opacity (0-100 %) </td> </tr> \n',sep="")
                endhtm<- paste(endhtm,' \n <tr> <td> \n <input type="text" id="',textnameW,'" value="1" onChange=\'setLineWeight(',lineName,',"',textnameW,'")\' size=3 /> Line weight (px) </td> </tr> </table> \n',sep="")
                
                }
else if   (class(SP)[1]=="SpatialLinesDataFrame")     {
            lineName<-paste('line',nameOfSP,sep="")
            boxname<-paste(nameOfSP,'box',sep="")
            textname<- paste(nameOfSP,'text',sep="")
            textnameW<-paste(textname,'W',sep="")
            att<-rep(NA,length(slot(SP.ll,"lines")))
            divLegendImage<-tempfile("Legend")  
            divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
            legendboxname<-paste('box',divLegendImage,sep="")

            att1=""
            
            
            if (!is.list(previousMap)) {
            var<-""
            # Declare variables in JavaScript marker and map
            var<-c(' \n var map \n')
            # Create all markers and store them in markersArray - PointsName
            }else{ var<-previousMap$var}
            
            var<-paste(var,'var ',lineName,'=[] ; \n')
            var1=""

                cxx<-PolyCol(attribute,colPalette,at=at)
                xx<- cxx$cols
            
            if(length(strokeWeight)==length(attribute)){
              swxx=strokeWeight
            }else{swxx<-weightATR(attribute,strokeWeight)}

            
            
            pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage)
            
            if(length(attribute)==length(colPalette)){xx= colPalette}
            
            
            
            var1<- paste( sapply(1:length(SP.ll@lines), function(i)          paste(var1,createLine(SP.ll@lines[[i]],
                                                                                                   strokeColor=xx[i]  ,
                                                                                                   strokeOpacity=strokeOpacity,
                                                                                                   strokeWeight=swxx[i],
                                                                                                   geodesic=geodesic,
                                                                                                   clickable=clickable,
                                                                                                   zIndex=zIndex ),'\n',sep="") 
            )
                          ,lineName,'.push(line); \n',sep="",collapse='\n')
            
            k = 1:length(names(SP.ll@data))
            
            att<- paste ( lapply(as.list(1:length(SP.ll@lines)), function(i) paste(names(SP.ll@data),':',
                                                                                   
                                             sapply(k ,function(k) as.character(SP.ll@data[i,k])),'<br>', collapse="")  )   )
            
 
            # Put all variables together
            var<-paste(var,var1)
            
            infW<-""
            
            infW<- paste ( lapply(as.list(1:length(SP.ll@lines)), function(i) 
              paste(infW,createInfoWindowEvent(Line_or_Polygon=paste(lineName,'[',i-1,'] ',sep=""),
                                               content=att[i]),' \n')  )  ,collapse='\n' )
            
            
            functions<-paste(functions,infW,'showO(',lineName,',"',boxname,'");',sep="")
            
            
            if (!is.list(previousMap)) {
              endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                           \n <div id="cBoxes"> \n')
            } else { endhtm<- previousMap$endhtm }
            
            endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> 
                           <input type="checkbox" id="',boxname,'" 
                           onClick=\'boxclick(this,',lineName,',"',
                           boxname,'");\' /> <b> </tr>', layerName ,
                           '<b> </td> \n',sep="")
            endhtm<- paste(endhtm,' \n <tr>  <td> \n <input type="text" id="',
                           textname,'" value="100" onChange=\'setOpacL(',
                           lineName,',"',textname,'")\' size=3 /> 
                           Opacity (0-100 %) </td>  </tr>\n',sep="")
            endhtm<- paste(endhtm,'<tr> \n  <td> \n <input type="text" id="',
                           textnameW,'" value="1" onChange=\'setLineWeight(',
                           lineName,',"',textnameW,'")\' size=3 /> 
                           Line weight (pixels) </td>  </tr> \n',sep="")
            if(legenda){
            endhtm<- paste(endhtm,' \n  <tr> <td> <input type="checkbox"  checked="checked" id="'
                           ,legendboxname,'" onClick=\'legendDisplay(this,"',
                           divLegendImage,'");\' /> LEGEND </td> </tr>  <tr> <td>',
                           attributeName,'</td> </tr> \n
                           <tr> <td> <div style="display:block;" id="',
                           divLegendImage,'"> <img src="',divLegendImage,
                           '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n   <hr> \n',sep="") }else{endhtm<- paste(endhtm, '</tr> \n </table> \n <hr>  \n')}
            
            
                                                                       }
else if   (class(SP)[1]=="SpatialPolygons")             {
              polyName<-paste('poly',nameOfSP,sep="")
              boxname<-paste(nameOfSP,'box',sep="")
              textname<- paste(nameOfSP,'text',sep="")
              textnameW<-paste(textname,'W',sep="")
              attribute=seq(1,length(SP.ll@polygons))
              
              
              if (!is.list(previousMap)) {
              var<-""
              # Declare variables in JavaScript marker and map
              var<-c(' \n var map \n')
              # Create all markers and store them in markersArray - PointsName
              }else{ var<-previousMap$var}
              
              var<-paste(var,'var ',polyName,'=[] ; \n')
              var1=""
              if(length(colPalette)==length(SP.ll@polygons)){
                xx<- colPalette}else{
                  cxx<-PolyCol(1:SP.ll@polygons,colPalette,at=at)
                  xx<- cxx$cols
                }
              
              if(length(strokeWeight)==length(SP.ll@polygons)){
                swxx=strokeWeight
              }else{swxx=rep(strokeWeight,length(SP.ll@polygons)) }
              
              var1<- paste( lapply(as.list(1:length(SP.ll@polygons)), function(i) paste(var1,createPolygon(SP.ll@polygons[[i]],
                                                                                                           fillColor=xx[i],
                                                                                                           strokeColor=strokeColor,
                                                                                                           strokeOpacity=strokeOpacity,
                                                                                                           strokeWeight=swxx[i],
                                                                                                           geodesic=geodesic,
                                                                                                           clickable=clickable,
                                                                                                           fillOpacity=fillOpacity,
                                                                                                           zIndex=zIndex),'\n',sep="") 
                                     )
                            ,polyName,'.push(polygon); \n',sep="",collapse='\n')
              
              # Put all variables together
              var<-paste(var,var1)
              
              
              functions<-paste(functions,'\n showO(',polyName,',"',boxname,'"); \n',sep="")
              
              
              if (!is.list(previousMap)) {
                endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                           \n <div id="cBoxes"> \n')
              } else { endhtm<- previousMap$endhtm }
              
              endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input 
                            type="checkbox" id="',boxname,'" onClick=\'boxclick(this,',
                            polyName,',"',boxname,'");\' /> ', 
                            layerName ,' </td>  </tr> \n',sep="")
              endhtm<- paste(endhtm,' \n <tr> <td> \n <input type="text" id="',
                             textname,'" value="50" onChange=\'setOpac(',
                             polyName,',"',textname,'")\' size=3 /> 
                             Opacity (0-100 %) </td> </tr> \n',sep="")
              endhtm<- paste(endhtm,' \n <tr> <td> \n <input type="text"  checked="checked"  id="',
                             textnameW,'" value="1" onChange=\'setLineWeight(',
                              polyName,',"',textnameW,'")\' size=3 /> Line weight
                               (pixels) </td> </tr> \n </table> \n',sep="")
              
               }
               
else if   (class(SP)[1]=="SpatialPolygonsDataFrame")      {

                  polyName<-paste('poly',nameOfSP,sep="")
                  boxname<-paste(nameOfSP,'box',sep="")
                  textname<- paste(nameOfSP,'text',sep="")
                  divLegendImage<-tempfile("Legend")  
                  divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
                  legendboxname<-paste('box',divLegendImage,sep="")
                  textnameW<-paste(textname,'W',sep="")

                  att<-rep(NA,length(slot(SP.ll,"polygons")))
                  att1=""
                  
                  if (!is.list(previousMap)) {
                  var<-""
                  # Declare variables in JavaScript marker and map
                  var<-c(' \n var map \n')
                  # Create all markers and store them in markersArray - PointsName
                  }else{ var<-previousMap$var}
                  
                  var<-paste(var,'var ',polyName,'=[] ; \n')
                  var1=""

                      cxx<-PolyCol(attribute,colPalette,at=at)
                      xx<- cxx$cols
                      pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage)
                   
                  if(length(attribute)==length(colPalette)){xx= colPalette}

                  
                  if(length(strokeWeight)==length(SP.ll@polygons)){
                    swxx=strokeWeight
                  }else{swxx=rep(strokeWeight,length(SP.ll@polygons)) }       
                  


var1<- paste( lapply(as.list(1:length(SP.ll@polygons)), function(i) paste(var1,createPolygon(SP.ll@polygons[[i]],
                                                                                            fillColor=xx[i],
                                                                                            strokeColor=strokeColor,
                                                                                            strokeOpacity=strokeOpacity,
                                                                                            strokeWeight=swxx[i],
                                                                                            geodesic=geodesic,
                                                                                            clickable=clickable,
                                                                                             fillOpacity=fillOpacity,
                                                                                            zIndex=zIndex),'\n',sep="") 
                     )
             ,polyName,'.push(polygon); \n',sep="",collapse='\n')  


                  k = 1:length(names(SP.ll@data))
att<- paste ( lapply(as.list(1:length(SP.ll@polygons)), function(i) paste(names(SP.ll@data),':',
                                                                          sapply(k ,function(k) as.character(SP.ll@data[i,k])),'<br>', collapse="")  )   )

                  var<-paste(var,var1)
                  
                  infW<-""
infW<- paste ( lapply(as.list(1:length(SP.ll@polygons)), function(i) paste(infW,createInfoWindowEvent(Line_or_Polygon=
                                                                    paste(polyName,'[',i-1,'] ',sep=""),content=att[i]),' \n')  )  ,collapse='\n' )                  

                  
                  functions<-paste(functions,infW,'showO(',polyName,',"',boxname,'");',sep="")
                  
                  
                  
                  if (!is.list(previousMap)) {
                    endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                              \n <div id="cBoxes"> \n')
                  } else { endhtm<- previousMap$endhtm }
                  
                  endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> 
                                 <input type="checkbox" id="',boxname,'" 
                                 onClick=\'boxclick(this,',polyName,',"',boxname,
                                 '");\' /> <b> ', layerName,'<b> </td> </tr> \n',sep="")
                  endhtm<- paste(endhtm,' \n <tr> <td> \n <input type="text" id="',
                                 textname,'" value="50" onChange=\'setOpac(',
                                 polyName,',"',textname,'")\' size=3 /> 
                                 Opacity (0-100 %) </td> </tr> \n',sep="")
                  endhtm<- paste(endhtm,' \n <tr>  <td> \n <input type="text" 
                                 id="',textnameW,'" value="1" onChange=\'
                                 setLineWeight(',polyName,',"',textnameW,'")\' 
                                 size=3 /> Line weight (pixels) </td> </tr> \n ',sep="")
                  if(legenda){
                    endhtm<- paste(endhtm,' \n <tr>  <td> <input type="checkbox"  checked="checked" id="'
                                   ,legendboxname,'" onClick=\'legendDisplay(this,"',
                                   divLegendImage,'");\' /> LEGEND </td> </tr>  <tr> <td>',
                                   attributeName,'</td> </tr>
                                    <tr> <td> <div style="display:block;" id="',
                                   divLegendImage,'"> <img src="',divLegendImage,
                                   '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n  <hr> \n',sep="") }else{ endhtm<- paste(endhtm, '</tr> \n </table> \n <hr>  \n') }
                                     }
else if (class(SP)[1]=="SpatialPixelsDataFrame" || class(SP)[1]=="SpatialGridDataFrame") {
                  
                rasterName<-tempfile("grid")
                rasterName<-substr(rasterName, start=regexpr("grid",rasterName),stop=nchar(rasterName))
                boxname<-paste(nameOfSP,'box',sep="")
                divLegendImage<-tempfile("Legend")  
                divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
                legendboxname<-paste('box',divLegendImage,sep="")
                textname<- paste(nameOfSP,'text',sep="")

                if (!is.list(previousMap)) {
                var<-""
                # Declare variables in JavaScript marker and map
                var<-c(' \n var map \n')
                # Create all markers and store them in markersArray - PointsName
                }else{ var<-previousMap$var}
                var<-paste(var,'\n var ',rasterName,'imageBounds = new google.maps.LatLngBounds
                (new google.maps.LatLng(',sw[1],',',sw[2],'),
                new google.maps.LatLng(',ne[1],',',ne[2],')); \n',sep="")
                var<-paste(var,'var ', rasterName ,'= new USGSOverlay(',rasterName,'imageBounds',',"',rasterName,  '.png", map); \n',sep="")
                
                 if(is.null(colPalette)){
                pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
                xx<-colPalette<-as.character(substr(pal(12),1,7)) } else { xx<-colPalette<-as.character(substr(colPalette,1,7)) }
                
                         if(is.factor(attribute)){
                         
                              if(length(colPalette)!=nlevels(attribute)) {
                                pal<-colorRampPalette(c( "green", "orange","brown"), space = "Lab")
                               xx<-colPalette<- as.character(substr(pal(nlevels(attribute)),1,7))    }
                              
                              #pp<-rasterLegend(attribute,colPalette=xx,legendName=divLegendImage)
                              SP$arg1111<-as.numeric(SP[attributeName]@data[,1])
                              }else{

                                  SP$arg1111<-SP[zcol]@data[,1]
                                     }
                
                cxx=PolyCol(SP[attributeName]@data[,1],colPalette,at)

                xx=cxx$cols
                pp<-legendbar(cxx$brks,colPalette=cxx$col.uniq,legendName=divLegendImage)            
                              
                SGqk <- GE_SpatialGrid(SP.ll)
                png(filename =paste(rasterName,'.png',sep=""), width=10*SGqk$width, height=10*SGqk$height, bg="transparent")
                par(mar=c(0,0,0,0), xaxs="i", yaxs="i")
                image(as.image.SpatialGridDataFrame(SP["arg1111"]),breaks=cxx$brks,  col=cxx$col.uniq)
                
                dev.off()
                
                
                 functions<-paste(functions,'showR(',rasterName,',"',boxname,'");',sep="")
                
                
                if (!is.list(previousMap)) {
                  endhtm<-c('</script> \n </head> \n <body onload="initialize()"> \n 
                            <div id="map_canvas"></div>  \n
                           \n <div id="cBoxes"> \n')
                } else { endhtm<- previousMap$endhtm }
                
                endhtm<- paste(endhtm,'<table border="0"> \n <tr> \n  <td> <input type="checkbox" id="',
                                  boxname,'" onClick=\'boxclickR(this,',rasterName,',"',boxname,
                                  '");\' /> <b> ', layerName ,'<b> </td> </tr> \n',sep="")
                                  
                  endhtm<- paste(endhtm,'  \n  <tr> <td> <input type="text" id="',
                                  textname,'" value="50" onChange=\'setOpacR(',
                                  rasterName,',"',textname,'")\' size=3 />  Opacity (0-100 %)</td>  </tr> \n',sep="")
                  
                endhtm<-paste(endhtm,' \n <tr> <td> <input type="checkbox"  checked="checked" id="'
                              ,legendboxname,'" onClick=\'legendDisplay(this,"',
                              divLegendImage,'");\' /> LEGEND </td> </tr>  <tr> <td>',
                              attributeName,'</td></tr>
                              <tr> <td> <div style="display:block;" id="',
                              divLegendImage,'"> <img src="',divLegendImage,
                              '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n   <hr> \n',sep="")
                                                                              }
else  {
    message("SP object must be Spatial class!") }
    


if (add==F){functions<- paste(functions," google.maps.event.addListener(map, 'rightclick', function(event) {
    var lat = event.latLng.lat();
    var lng = event.latLng.lng();
    alert('Lat=' + lat + '; Lng=' + lng);}); " , " \n }" )
endhtm<-paste(endhtm,'</div> \n </body>  \n  </html>')
write(starthtm, filename,append=F)
write(var, filename,append=TRUE)
write(functions, filename,append=TRUE)
write(endhtm, filename,append=TRUE)
if(openMap){browseURL(filename)}
            }


x <- list(starthtm=starthtm,var=var, functions=functions,endhtm=endhtm)
return(x)


              }
