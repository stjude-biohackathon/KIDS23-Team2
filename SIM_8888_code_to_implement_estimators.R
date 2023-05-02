
## Code for obtaining survival estimators described in  
## "Displaying survival of patient groups defined by covariate paths: Extensions of the Kaplan-Meier estimator"
## By M Jay and RA Betensky
## This code can be used to implement the two proposed estimators, Snapinn et al's estimator, and Xu et al's estimator


source("C:/Users/ezeng38/OneDrive - St. Jude Children's Research Hospital/Projects/HCT2RETRO/working code/SIM_8888_function_survfitCP.R") # First, source the survfitCP function

# Example using the PBC dataset in the survival package -------------------

library(survival)
library(tidyverse)

# Create bilirubin dataset

pbc_copy <- filter(pbc, id <= 312) %>%
  select(id:trt, sex, age)

pbcseq_copy <- select(pbcseq, id, day, bili, stage, alk.phos, ascites, hepato, spiders)

pbc_copy <- tmerge(pbc_copy, pbc_copy, id = id, endpt = event(time, status))

bili <- tmerge(pbc_copy, pbcseq_copy, id = id, bili = tdc(day, bili), stage = tdc(day, stage)) %>%
  mutate(endpt = ifelse(endpt == 2, 1, 0), bili = ifelse(bili < 2, 0, 1),
         tstart = tstart / 365, tstop = tstop / 365, female = ifelse(sex == 'f', 1, 0),
         trt = trt - 1) %>%
  select(id, tstart, tstop, endpt, bili, age, female, trt, stage)

## Implement Snapinn et al's estimator

# Using survival package
snapinn_bili <- survfit(Surv(tstart, tstop, endpt) ~ bili, data = bili, conf.type = 'log-log')
summary(snapinn_bili)

# Using survfitCP function
snapinn_normal_bili <- survfitCP(bili, 
                                 covariate_vals = c(0,1), # Values on covariate path
                                 transition_times = Inf, # Never transition to covariate value 1
                                 weights = F) # No stabilized weights
snapinn_normal_bili

snapinn_high_bili <- survfitCP(bili, 
                           covariate_vals = c(0,1), # Values on covariate path
                           transition_times = 0, # Immediately transition to covariate value 1 (at time 0)
                           weights = F) # No stabilized weights
snapinn_high_bili

## Implement Xu et al's estimator

# Using survfitCP function
xu_normal_bili <- survfitCP(bili,
                            covariate_vals = c(0,1), # Values on covariate path
                            transition_times = Inf, # Never transition to covariate value 1
                            weights = T) # Use stabilized weights

xu_normal_bili

xu_high_bili <- survfitCP(bili, 
                      covariate_vals = c(0,1), # Values on covariate path
                      transition_times = 0, # Immediately transition to covariate value 1 (at time 0)
                      weights = T) # Use stabilized weights

xu_high_bili

## Implement proposed unadjusted estimator for the following covariate path:
## Normal bilirubin until time 5 then high bilirubin

unadjusted_normal_to_high_5 <- survfitCP(bili, 
                                         covariate_vals = c(0,1), # Values on covariate path
                                         transition_times = 5, # Transition from covariate value 0 to 1 at time 5
                                         weights = F) # No stabilized weights

unadjusted_normal_to_high_5

## Implement proposed adjusted estimator for the following covariate path:
## Normal bilirubin until time 5 then high bilirubin

adjusted_normal_to_high_5 <- survfitCP(bili, 
                                       covariate_vals = c(0,1), # Values on covariate path
                                       transition_times = 5, # Transition from covariate value 0 to 1 at time 5
                                       weights = T) # Use stabilized weights

adjusted_normal_to_high_5

## combine estimators into one dataset with est type column

xu_normal_bili$est_type <- rep("xu_normal", nrow(xu_normal_bili))
unadjusted_normal_to_high_5$est_type <- rep("proposed_unadjusted", nrow(unadjusted_normal_to_high_5))
xu_high_bili$est_type <- rep("xu_high", nrow(xu_high_bili))
adjusted_normal_to_high_5$est_type <- rep("proposed_adjusted", nrow(adjusted_normal_to_high_5))
snapinn_high_bili$est_type <- rep("snapinn_high", nrow(snapinn_high_bili))
snapinn_normal_bili$est_type <- rep("snapinn_normal", nrow(snapinn_normal_bili))

test <- data.frame(rbind(xu_normal_bili, xu_high_bili, snapinn_high_bili, snapinn_normal_bili, unadjusted_normal_to_high_5, adjusted_normal_to_high_5))

## step plot
ggplot(test, aes(t, surv)) + geom_step(aes(color = est_type))

