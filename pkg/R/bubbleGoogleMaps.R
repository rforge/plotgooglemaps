
bubbleGoogleMaps <-
function(SP,
                     filename="",
                     zcol=1,
                     max.radius=100,
                     key.entries = quantile(SP@data[,zcol],(1:5)/5),
                     do.sqrt = TRUE,
                     add=FALSE,
                     previousMap=NULL,
                     colPalette=NULL,
                     strokeColor="#FFAA00",
                     strokeOpacity=1,
                     fillOpacity=0.7,
                     strokeWeight=1,
                     geodesic=TRUE,
                     clickable=TRUE,
                     zIndex="null",
                     shape="c",
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
                               openMap=TRUE) {


################################################################################
################################################################################
################################################################################

disableDefaultUI=FALSE



	obj = as(SP, "SpatialPointsDataFrame")
	data = obj@data
	if (NCOL(data) == 1){
		z = as.numeric(data[,1])
	         }else {
    	z = as.numeric( data[, zcol] )  }
    	# avoid negative values

kkk=c(min(z),key.entries)

kkk=sapply(2:length(kkk), function(i) mean( c(kkk[i],kkk[i-1]) ) )
    
    	ke<-abs(kkk) + mean(abs(kkk))    # no zeros as max for radius vecor
    	# creating a vector for subgroups
    	if(do.sqrt){
    	scale.level<- sqrt(ke/(max(ke)) ) }else{scale.level<-ke/(max(ke))}
	radius.level<-max.radius*scale.level
	 # list of radiuses for createSphereCircle
	 breakss<-factor(c(min(z),key.entries))
   break_unique<-as.numeric(levels(breakss))
   break_unique[length(break_unique)]<-max(z)
   
  if(length(unique(z))==length(key.entries)){ zz=factor(z,labels=radius.level)
                                              radius.vector<-floor(as.numeric(as.vector(zz))) 
                                            }else{ 
                                                  zz=factor(cut(z,break_unique,include.lowest=TRUE ),labels=radius.level)
	radius.vector<-floor(as.numeric(as.vector((zz))))
                                                  }


  SP.ll <- spTransform(SP, CRS("+proj=longlat +datum=WGS84"))


Centar=c(mean(SP.ll@bbox[1,]),mean(SP.ll@bbox[2,]))
sw<-c(SP.ll@bbox[2,1],SP.ll@bbox[1,1])
ne<-c(SP.ll@bbox[2,2],SP.ll@bbox[1,2])
###################################################

nameOfSP<-sapply(as.list(substitute({SP})[-1]), deparse)
nameOfSP<-gsub('[!,",#,$,%,&,(,),*,+,-,.,/,:,;,<,=,>,?,@,^,`,|,~]', "_", nameOfSP)
nameOfSP<-gsub('[[]', "_", nameOfSP)
nameOfSP<-gsub('[]]', "_", nameOfSP)
if(filename==""){
filename <- paste(nameOfSP,'.htm',sep="")}
attribute=SP@data[,zcol]
polyName<-paste('poly',nameOfSP,sep="")
boxname<-paste(nameOfSP,'box',sep="")
textname<- paste(nameOfSP,'text',sep="")
divLegendImage<-tempfile("Legend")  
divLegendImage<-substr(divLegendImage, start=regexpr("Legend",divLegendImage),stop=nchar(divLegendImage))
legendboxname<-paste('box',divLegendImage,sep="")
textnameW<-paste(textname,'W',sep="")
if(layerName==""){
layerName=nameOfSP}



if(strokeColor!=""){
rgbc<-col2rgb(strokeColor)
strokeColor<-rgb(rgbc[1],rgbc[2],rgbc[3],maxColorValue=255) }

if(is.null(colPalette) & min(key.entries)<0){
  colPalette=rep("#99000D",length(key.entries))
  colPalette[which(key.entries<0)]="#084594"
                                       }

if(!is.null(colPalette)){
rgbc<-col2rgb(colPalette)
colPalette<-apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))}

if(length(colPalette)==1){
  colPalette=rep(colPalette,length(key.entries))
  rgbc<-col2rgb(colPalette)
  colPalette<-apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))}

if(length(colPalette)==2 & min(key.entries)<0){
  cop=rep(colPalette[2],length(key.entries))
  cop[which(key.entries<0)]=colPalette[1]
  rgbc<-col2rgb(cop)
  colPalette<-apply(rgbc,2,function(x) rgb(x[1],x[2],x[3],maxColorValue=255))}



for(i in 1:length(SP.ll@data)) {
if( identical(attribute,SP.ll@data[,i])){
 attributeName<-names(SP.ll@data)[i]  }
}

att<-rep(NA,length(SP.ll@coords[,1]))
att1=""

if (!is.list(previousMap)) {
var<-""
# Declare variables in JavaScript marker and map
var<-c(' \n var map \n')
# Create all markers and store them in markersArray - PointsName
}else{ var<-previousMap$var}

var<-paste(var,'var ',polyName,'=[] ; \n')
var1=""

cxx<-PolyCol(factor(zz,labels=key.entries),colPalette)

xx<-cxx$cols

if( length(key.entries)==1){
  att_L<-key.entries } else{ att_L<- factor(cut(z,break_unique,include.lowest=TRUE ) )  }

pp<-bubbleLegend(shape=shape,attribute=att_L,colPalette=colPalette
                 ,legendName=divLegendImage,scale.level=scale.level,strokeColor=strokeColor)



var1<- paste( lapply(as.list(1:length(SP.ll@coords[,1])), function(i) paste(var1,createSphereShape(shape=shape,center=c(SP.ll@coords[i,1],
                                                                                                                        SP.ll@coords[i,2]),
                                                                                                   radius=radius.vector[i],
                                                                                                   fillColor=xx[i],
                                                                                                   strokeColor=strokeColor,
                                                                                                   strokeOpacity=strokeOpacity,
                                                                                                   strokeWeight=strokeWeight,
                                                                                                   geodesic=geodesic,
                                                                                                   clickable=clickable,
                                                                                                   fillOpacity=fillOpacity,
                                                                                                   zIndex=zIndex),'\n',sep="") 
)
              ,polyName,'.push(polygon); \n',sep="",collapse='\n')

k = 1:length(names(SP.ll@data))
att<- paste ( lapply(as.list(1:length(SP.ll@coords[,1])), function(i) paste(names(SP.ll@data),':',sapply(k ,function(k) as.character(SP.ll@data[i,k]))
                                                                            ,'<br>', collapse="")  )   )



# Put all variables together
var<-paste(var,var1)
if (!is.list(previousMap)) {
functions<-""
# Creating functions for checkbox control, Show , Hide and Toggle control
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
functions<-paste(functions,"google.maps.event.addListener(map,'mousemove',function(event) {
  document.getElementById('latlong').innerHTML = event.latLng.lng() + ', ' + event.latLng.lat()}); \n", sep="")
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
infW<-""

infW<- paste ( lapply(as.list(1:length(SP.ll@coords[,1])), function(i) 
  paste(infW,createInfoWindowEvent(Line_or_Polygon=paste(polyName,'[',i-1,'] ',sep=""),content=att[i]),' \n')  )  ,collapse='\n' )


functions<-paste(functions,infW,'showO(',polyName,',"',boxname,'");',sep="")


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

endhtm<- paste(endhtm,' \n <tr>  <td> <input type="checkbox"  checked="checked" id="'
               ,legendboxname,'" onClick=\'legendDisplay(this,"',
               divLegendImage,'");\' /> LEGEND </td> </tr>  <tr> <td>',
               attributeName,'</td> </tr>
                                    <tr> <td> <div style="display:block;" id="',
                                   divLegendImage,'"> <img src="',divLegendImage,
               '.png" alt="Legend" height="70%"> </div>
                           </td> </tr> \n </table> \n  <hr> \n',sep="")

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
