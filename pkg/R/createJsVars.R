createJsVars <- function(map) {
  
  ## 3/11/2017 Common function to create required JavaScript variables
  
  jsVars <- paste0('var ',map,';
var trafficLayer;
var transitLayer;
var bicycleLayer;
var drawingManager;
')
  
  return(jsVars)
}