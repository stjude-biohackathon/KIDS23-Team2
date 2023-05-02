
## Function for implementing survival estimators described in  
## "Displaying survival of patient groups defined by covariate paths: Extensions of the Kaplan-Meier estimator"
## By M Jay and RA Betensky

survfitCP <- function(data, covariate_vals=c(0,1), transition_times, weights = F, conf_level = 0.95){
  
  # Here, we assume data are formatted as a data frame with the corresponding columns: 
  # id, start, stop, event status, discrete time-varying covariate, additional covariates if using stabilized weights
  # covariate_vals = vector of covariate values on path
  # transition_times = vector of times at which covariate value switches (length should be one less than length of covariate.vals)
  # weights = whether or not stabilized weights should be used
  
  id <- data[,1]
  start_time <- data[,2]
  stop_time <- data[,3]
  event_ind <- data[,4]
  cov_val <- data[,5]
  
  event_times <- sort(unique(stop_time[event_ind == 1]))
  transition_times <- unique(c(-Inf, transition_times, Inf))
  path_vals <- covariate_vals[findInterval(event_times, transition_times, left.open = T)]
  
  # data for people who are alive at each event time
  at_risk <- lapply(1:length(event_times), function(i){
    data_subset <- data[(start_time < event_times[i] & stop_time >= event_times[i]),]
  })
  
  if(weights){
    
    wts <- sapply(1:length(event_times), function(i){
      
      data_subset <- at_risk[[i]]
      data_subset$response <- ifelse(data_subset[,5] == path_vals[i], 1, 0)
      prop_numer <- sum(data_subset[,5] == path_vals[i]) / nrow(data_subset)
      numer <- ifelse(data_subset$response == 1, prop_numer, 1 - prop_numer)
      form <- paste0('response ~ ', paste(colnames(data_subset)[6:(ncol(data_subset)-1)], collapse = '+'))
      prop_denom <- glm(form, family = 'binomial', data = data_subset)$fitted.values
      denom <- ifelse(data_subset$response == 1, prop_denom, 1 - prop_denom)
      data_subset$w <- numer / denom
      n <- sum(data_subset$w[((data_subset[,5] == path_vals[i]) & (data_subset[,2] < event_times[i]) & (data_subset[,3] >= event_times[i]))])
      d <- sum(data_subset$w[((data_subset[,5] == path_vals[i]) & (data_subset[,2] < event_times[i]) & (data_subset[,3] == event_times[i])) 
                             & data_subset[,4] == 1])
      return(c(n,d))
      
    })
    
    risk_and_event <- t(wts)

  }else{ 
    
    n <- sapply(1:length(event_times), 
                function(i){sum(ifelse(start_time < event_times[i] & stop_time >= event_times[i] & cov_val == path_vals[i], 1, 0))})
    d <- sapply(1:length(event_times), 
                function(i){sum(ifelse(stop_time == event_times[i] & cov_val == path_vals[i] & event_ind == 1, 1, 0))})
    
    risk_and_event <- cbind(n,d)
    
    }
  
  surv_table <- data.frame(event_times, risk_and_event)
  colnames(surv_table)[2:3] <- c("n", "d")
  surv_table <- subset(surv_table, d != 0)
  surv_table$km <- 1 - surv_table$d / surv_table$n
  surv_table$surv <- cumprod(surv_table$km)
  
  z_val <- abs(qnorm((1-conf_level)/2))
  
  surv_table$var <- 1 / (log(surv_table$surv)^2) * cumsum(surv_table$d / (surv_table$n * (surv_table$n - surv_table$d)))
  surv_table$lower_ci <- exp(-exp(log(-log(surv_table$surv)) + z_val*sqrt(surv_table$var)))
  surv_table$upper_ci <- exp(-exp(log(-log(surv_table$surv)) - z_val*sqrt(surv_table$var)))
  
  colnames(surv_table)[1] <- 't'
  
  return(surv_table[,c(1:3,5,7:8)])
  
}
