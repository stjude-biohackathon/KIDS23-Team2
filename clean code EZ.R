source("C:/Users/emily/Documents/survfit_function.R") # First, source the survfitCP function
source("C:/Users/emily/Documents/SUBODH TDC CODE (2 FUNCTIONS).R")

# Example using the PBC dataset in the survival package -------------------

## load libraries 
library(survival)
library(ggplot2)
library(survminer)
library(gridExtra)
library(stringr)

# Create bilirubin dataset

pbc_copy <- filter(pbc, id <= 312) %>%
  select(id:trt, sex, age)

tmp <- filter(pbc, id<=312) %>% select(id:trt,sex,age)

pbcseq_copy <- select(pbcseq, id, day, bili, stage, alk.phos, ascites, hepato, spiders)

pbc_copy <- tmerge(pbc_copy, pbc_copy, id = id, endpt = event(time, status))

## tstart and tstop in years (previously in days)
## stage and billi are tdc
## billi: binary variable, 0 vs 1 = <2 vs >2
## endpt: binary variable, 1 vs 0  = dead vs censored/transplant
bili <- tmerge(pbc_copy, pbcseq_copy, id = id, bili = tdc(day, bili),
               stage = tdc(day, stage)) %>%
  mutate(endpt = ifelse(endpt == 2, 1, 0),
         bili = ifelse(bili < 2, 0, 1),
         tstart = tstart / 365, tstop = tstop / 365,
         female = ifelse(sex == 'f', 1, 0),
         trt = trt - 1) %>%
  select(id, tstart, tstop, endpt, bili, age, female, trt, stage)

# write.csv(bili, "C:/Users/emily/Documents/bili.csv", row.names = F)


## Normal bilirubin until time 5 then high bilirubin

adjusted_normal_to_high_5 <- survfitCP(bili, 
                                       covariate_vals = c(0,1), # Values on covariate path
                                       transition_times = 5, # Transition from covariate value 0 to 1 at time 5
                                       weights = T) # Use stabilized weights

## high bili immediately 
adjusted_high <- survfitCP(bili, covariate_vals = c(0,1), transition_times = 0, weights = T)

## combine high and normal to high bili datasets into one with new col for transition)
all_data <- data.frame(rbind(cbind(adjusted_high, "transition" = 0),
                             cbind(adjusted_normal_to_high_5, "transition" = 5)))
all_data$transition <- as.factor(all_data$transition)

## plot to compare transition times (no risk table)
ggplot(all_data, aes(t,surv)) + geom_step(aes(color = transition)) +
  geom_ribbon(aes(color = transition, ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
  labs(x = "Time", y = "Survival Probability") +
  scale_color_discrete(name = "Transition")

## plot normal to high with risk table at bottom (just one survival curve)
grid.arrange(ggplot(data = adjusted_normal_to_high_5, aes(t,surv)) + 
               geom_step(linewidth = 0.75) +
               geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
               labs(x = "Time in Years", y = "Survival Probability"),
             ggrisktable(survfit(Surv(tstart, tstop, endpt) ~ 1, data = bili), data = bili,
                         break.time.by = 3, fontsize = 3, ylab = "",
                         risk.table.title = "Number at risk*"))

## plot to compare transition times with risk table at bottom 
grid.arrange(ggplot(data = all_data, aes(t,surv)) + 
               geom_step(linewidth = 0.75, aes(color = transition)) +
               geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, color = transition), alpha = 0.2, ) +
               labs(x = "Time in Years", y = "Survival Probability"),
             ggrisktable(survfit(Surv(tstart, tstop, endpt) ~ 1, data = bili), data = bili,
                         break.time.by = 3, fontsize = 3, ylab = "",
                         risk.table.title = "Number at risk*"))

## PLOT DIFFERENT LEVELS OF COVARIATE

## use subodh's functions to get list that has multiple dataframes (1 for each level of covariate and their survival times)
curves_list <- getMultiSurvfitCurves(wrongUseBaselineTVC(bili))
## create dataframe to combine dataframes, also create new column for strata level 
df <- data.frame()         
for (i in 1:length(getMultiSurvfitCurves(wrongUseBaselineTVC(bili)))) {
  df0 <- cbind(curves_list[[i]], "stratum" = str_sub(names(curves_list)[i], -1))
  df <- rbind(df, df0)
  df$stratum <- as.factor(df$stratum)
}

## plot to compare covariate stratum levels 
ggplot(df, aes(time,survEst.km)) +
  geom_step(linewidth = 0.75, aes(color = stratum)) +
  labs(x = "Time", y = "Survival Probability", color = "TDC Stratum")
