## 7/12/2015: Common function that adds Javascript functions to map layers
## Write once instead of repeating in each routine
## 3/12/2017: Added support for traffic, transit and bicycle layers

createMapFunctions <- function() {

  ## Note: 7/12/2015: setOpacR used with SpatialGridDataFrame
  
  funs <- paste("function showR(R,boxname, map) {
  R.setMap(map);
  document.getElementById(boxname).checked = true; 
}

function hideR(R,boxname) {
  R.setMap(null);
  document.getElementById(boxname).checked = false; 
}

function showO(MLPArray,boxname, map ) { 
  for (var i = 0; i < MLPArray.length; i++) { 
    MLPArray[i].setMap(map); 
  } 
  document.getElementById(boxname).checked = true; 
}

function hideO(MLPArray,boxname) { 
  for (var i = 0; i < MLPArray.length; i++) { 
    MLPArray[i].setMap(null);
  } 
  document.getElementById(boxname).checked = false; 
} 

function boxclick(box,MLPArray,boxname, map) { 
  if (box.checked) { 
    showO(MLPArray,boxname, map); 
  } else {  
    hideO(MLPArray,boxname);
  } 
}

function showHeatmap(MLPArray,boxname,map) {
  MLPArray.setMap(map);
  document.getElementById(boxname).checked = true;
}

function hideHeatmap(MLPArray,boxname) {
  MLPArray.setMap(null);
  document.getElementById(boxname).checked = false;
}

function boxclickHeatmap(box,MLPArray,boxname, map) { 
  if (box.checked) { 
    showHeatmap(MLPArray,boxname, map); 
  } else {
    hideHeatmap(MLPArray,boxname);
  } 
}

function setOpac(MLPArray,textname) {
  opacity=0.01*parseInt(document.getElementById(textname).value) 
  for(var i = 0; i < MLPArray.length; i++) {
    MLPArray[i].setOptions({strokeOpacity: opacity, fillOpacity: opacity}); 
  }
}

function setOpacL(MLPArray,textname) {
  opacity=0.01*parseInt(document.getElementById(textname).value) 
  for (var i = 0; i < MLPArray.length; i++) {
    MLPArray[i].setOptions({strokeOpacity: opacity});
  }
}

function setOpacR(Raster,textname) {
  opac=0.01*parseInt(document.getElementById(textname).value)
  Raster.div_.style.opacity= opac 
}

function setLineWeight(MLPArray,textnameW) {
  weight=parseInt(document.getElementById(textnameW).value)
  for (var i = 0; i < MLPArray.length; i++) {
    MLPArray[i].setOptions({strokeWeight: weight}); 
  } 
}

function boxclickR(box,R,boxname, map) {
  if (box.checked) {
    showR(R,boxname,map); 
  } else { 
    hideR(R,boxname);
  } 
}

// Added for TRAFFIC support
function showTraffic(boxname, map) {
  if (typeof(trafficLayer) === 'undefined') {
	  trafficLayer = new google.maps.TrafficLayer();
	}
  trafficLayer.setMap(map);
  document.getElementById(boxname).checked = true;
}

function hideTraffic(boxname, map) {
  if (typeof(trafficLayer) === 'undefined') {
	  trafficLayer = new google.maps.TrafficLayer();
  }
  trafficLayer.setMap(null);
  document.getElementById(boxname).checked = false;
}

function boxclickTraffic(box, boxname, map) {
  if (box.checked) {
    showTraffic(boxname, map);
  } else {
    hideTraffic(boxname);
  }
}

// Added for TRANSIT support
function showTransit(boxname, map) {
  if (typeof(transitLayer) === 'undefined') {
	  transitLayer = new google.maps.TransitLayer();
	}
  transitLayer.setMap(map);
  document.getElementById(boxname).checked = true;
}

function hideTransit(boxname, map) {
  if (typeof(transitLayer) === 'undefined') {
	  transitLayer = new google.maps.TransitLayer();
	}
  transitLayer.setMap(null);
  document.getElementById(boxname).checked = false;
}

function boxclickTransit(box, boxname, map) {
  if (box.checked) {
    showTransit(boxname, map);
  } else {
    hideTransit(boxname);
  }
}

// Added for BICYCLE support
function showBicycle(boxname, map) {
  if (typeof(bicycleLayer) === 'undefined') {
	  bicycleLayer = new google.maps.BicyclingLayer();
	}
  bicycleLayer.setMap(map);
  document.getElementById(boxname).checked = true;
}

function hideBicycle(boxname, map) {
  if (typeof(bicycleLayer) === 'undefined') {
	  bicycleLayer = new google.maps.BicyclingLayer();
  }
  bicycleLayer.setMap(null);
  document.getElementById(boxname).checked = false;
}

function boxclickBicycle(box, boxname, map) {
  if (box.checked) {
    showBicycle(boxname, map);
  } else {
    hideBicycle(boxname);
  }
}

function legendDisplay(box,divLegendImage) {
  element = document.getElementById(divLegendImage).style; 
  if (box.checked) { 
    element.display='block';
  } else {  
    element.display='none';
  }
}\n\n",sep="")

  return(funs)
}
