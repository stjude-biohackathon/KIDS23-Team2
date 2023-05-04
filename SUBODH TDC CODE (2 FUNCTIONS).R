### expects a dataset with columns 1-4 being  id, tstart, tstop, endpt
### optional 5th+ column to be (possibly, time-varying) covariates: THE LAST STATE IS THE SAVED COVARIATE VALUE
### (last row should be the terminal event or final censoring time in conventional survival analysis;
### not to be used with intermediate events, recurrent events, etc.)
tMergeToConventional <- function(tmData){
  require(dplyr)
  colnames(tmData)[1] <-"id" # want to use column name id for group_by below
  oneRowDat <- tmData %>% group_by(id) %>%
    slice(n()) # saves the last row per id
  out <- oneRowDat %>% rename(
    "Y"=colnames(oneRowDat)[3], # rename tstop to Y
    "D"=colnames(oneRowDat)[4], # rename endpt to D
  ) %>% select(
    id,
    Y:last_col() # want to drop the tstart column since it is irrelevant and confusing
  )
  return(as.data.frame(out))
}

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
  for (k in 1:length(out)){
    out[[k]] <-
      as.data.frame(cbind(
        concDat$time,concDat$n.risk,concDat$n.event,concDat$surv
      )[defLabels==levels(defLabels)[k],]) # grab the data corresponding to the kth stratum
    colnames(out[[k]]) <- c("time","n.risk","n.event","survEst.km")
  }
  return(out)
}
