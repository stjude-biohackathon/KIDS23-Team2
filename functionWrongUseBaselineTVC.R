### requires tMergeToConventional function

### input is tmerge-d data
### output is survfit using the 5th column OR 1 specified variable name as a
###   time-varying covariate that is incorrectly being used as a baseline covariate

wrongUseBaselineTVC <- function(tmData,optVarName=NULL){
  convData <- tMergeToConventional(tmData) # returns survival data with the last covariate profile; this is wrong if time-varying covariate
  
  if (is.null(optVarName)) tvcVar <- as.name(colnames(tmData)[5]) else {
    if (!any(optVarName %in% colnames(tmData))) {
      stop() # need to break if variable name not found
      } else tvcVar <- as.name(optVarName)
  }
  
  out <- survfit(Surv(Y,D)~get(tvcVar),data=convData)
  
  return(out)
}

### function to plot multiple stratified survival curves created by one survfit command
### input is survfit object
### output is list with multiple data.frame(time,n.risk,survEstimate) corresponding to each stratum level

getMultiSurvfitCurves <- function(survfitObj){
  concDat <- summary(survfitObj) # data in this object are concatenated by groupings
  
  defLabels <- concDat$strata # the (default) indices to indicate which data belong to which strata; uses default labels
  out <- vector("list",length=length(levels(defLabels)))
  names(out) <- levels(defLabels)
  
  for (k in 1:length(out)){ # allow for arbitrary number of strata l