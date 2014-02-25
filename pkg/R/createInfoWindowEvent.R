createInfoWindowEvent <-
function(Line_or_Polygon,
                                map="map",
                                event="click",
                                content="The content",
                                position="event.latLng",
                                disableAutoPan=FALSE,
                                maxWidth="null",
                                pixelOffset="null",
                                zIndex="null") {

             if (disableAutoPan!=FALSE)
                  {disableAutoPan='true'}else{ disableAutoPan='false' }


          if (is.numeric(position)) {
          position=paste('position=new google.maps.LatLng(',position[2],',',position[1],')')
          }
           if (is.numeric(position)) {
          pixelOffset=paste('new google.maps.Size',pixelOffset[1],',',pixelOffset[1],')')
          }
x=paste( ' var infowindow = new google.maps.InfoWindow({ content: "" }); \n google.maps.event.addListener(',Line_or_Polygon,
',"',event,'",function(event){ \n infowindow.content="',
content,'"; \n  infowindow.position =',position,
'; \n infowindow.disableAutoPan=', disableAutoPan,
'; \n infowindow.maxWidth=',maxWidth,
';\n infowindow.pixelOffset=',
pixelOffset,';\n infowindow.zIndex='
,zIndex,'; infowindow.open(',map,')}); ',sep="")

return(x)

}
