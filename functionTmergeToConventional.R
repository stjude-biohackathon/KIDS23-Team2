### expects a dataset with columns 1-4 being  id, tstart, tstop, endpt
### optional 5th+ column to be (possibly, time-varying) covariates: THE LAST STATE IS THE SAVED COVARIATE VALUE
### (last row should be the terminal event or final censoring time in conventional survival analysis;
### not to be used with intermediate events, recurrent events, etc.)

### input is tmerge-d data
### output is conventional-style data, with covariate values corresponding to the last covariate profile of the subject

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
  
  return(out)
}
