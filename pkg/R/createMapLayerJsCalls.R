## Creates the JavaScript function calls to show/hide the traffic, transit and bicycle map layers
## createMapLayerHtml() is the companion function for createMapLayerJsCalls()
createMapLayerJsCalls <- function(map,
                                  boxnamePrefix,
                                  trafficLayerEnabled,
                                  transitLayerEnabled,
                                  bicycleLayerEnabled) {
  
  ## Control name is created by appending "Traffic", "Transit", "Bicycle" to boxnamePrefix
  ## Creating multiple traffic map layers is not recommended as that will results in having
  ## different controls that can turn traffic on/off
  jsFunc <- paste0(ifelse(is.null(trafficLayerEnabled),
                          '',
                          ifelse(trafficLayerEnabled,
                                 paste0(' showTraffic("',boxnamePrefix,'Traffic",',map,');\n'),
                                 paste0(' hideTraffic("',boxnamePrefix,'Traffic",',map,');\n'))),
                   
                   ifelse(is.null(transitLayerEnabled),
                          '',
                          ifelse(transitLayerEnabled,
                                 paste0(' showTransit("',boxnamePrefix,'Transit",',map,');\n'),
                                 paste0(' hideTransit("',boxnamePrefix,'Transit",',map,');\n'))),
                   
                   ifelse(is.null(bicycleLayerEnabled),
                          '',
                          ifelse(bicycleLayerEnabled,
                                 paste0(' showBicycle("',boxnamePrefix,'Bicycle",',map,');\n'),
                                 paste0(' hideBicycle("',boxnamePrefix,'Bicycle",',map,');\n'))))
  
  return(jsFunc)
}