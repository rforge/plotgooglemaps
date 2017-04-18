## Creates the html code required to show the traffic, transit and bicycle map layers in the map legend
## createMapLayerJsCalls() is the companion function for createMapLayerHtml()
createMapLayerHtml <- function(map,
                               boxnamePrefix,
                               trafficLayerEnabled,
                               trafficLayerName,
                               transitLayerEnabled,
                               transitLayerName,
                               bicycleLayerEnabled,
                               bicycleLayerName) {
  
  ## Control name is created by appending "Traffic", "Transit", "Bicycle" to boxnamePrefix
  ## Creating multiple traffic map layers is not recommended as that will results in having
  ## different controls that can turn traffic on/off
  mapLayerHtml <- paste0(ifelse(is.null(trafficLayerEnabled) || is.null(trafficLayerName),
                                '',
                                paste0("<table style=\"border-collapse:collapse; width:100%;\"><tr><td><input type=\"checkbox\" id=\"",
                                       boxnamePrefix,"Traffic\" onClick='boxclickTraffic(this,\"",
                                       boxnamePrefix,"Traffic\",map);' /><b> ",
                                       as.character(trafficLayerName),"</b></td></tr></table>\n")),
                         
                         ifelse(is.null(transitLayerEnabled) || is.null(transitLayerName),
                                '',
                                paste0("<table style=\"border-collapse:collapse; width:100%;\"><tr><td><input type=\"checkbox\" id=\"",
                                       boxnamePrefix,"Transit\" onClick='boxclickTransit(this,\"",
                                       boxnamePrefix,"Transit\",map);' /><b> ",
                                       as.character(transitLayerName),"</b></td></tr></table>\n")),
                         
                         ifelse(is.null(bicycleLayerEnabled) || is.null(bicycleLayerName),
                                '',
                                paste0("<table style=\"border-collapse:collapse; width:100%;\"><tr><td><input type=\"checkbox\" id=\"",
                                       boxnamePrefix,"Bicycle\" onClick='boxclickBicycle(this,\"",
                                       boxnamePrefix,"Bicycle\",map);' /><b> ",
                                       as.character(bicycleLayerName),"</b></td></tr></table>\n")))

  return(mapLayerHtml)
}