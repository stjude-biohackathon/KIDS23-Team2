KIDS23 Time-Dependent Covariate Survival Visualization App


About the app
The KTCSVis App is an R Shiny App that can perform survival analysis using an appropriate user-uploaded file. Typical examples of survival analysis clinical data include information about time-to-event occurrence of death, withdrawal, relapse, adverse drug reaction and the appearance of secondary infection or disease.
The main objective of the app is to provide an intuitive interface for interpreting survival analysis results with time-varying covariates. Extensions of the Cox proportional hazard to study the association between a variable that can change after baseline and a time-to-event outcome of interest exist, but the interpretations can be challenging. A new approach described by Jay & Betensky (2021) overcomes this problem by predicting the survival function for a user-specified trajectory of one time-varying covariate. The KTCSVis app uses this method to predict survival curves for interpretable results of biomedical research.

How to use the app
Once the user uploads the data file (specification below) in the DATA tab, the app provides a survival prediction curve that can be viewed or downloaded in the PLOT tab. This plot is accompanied by a ‘number at risk table’, the overall number of subjects at risk at fixed time points. The app also allows for comparison of multiple different transition times in the COMPARISON PLOT tab.
#Please Note: The app currently only supports binary (0,1) time dependent covariates with a one-way transition. The transition time represents the time at which a group transitions from covariate value 0 to value 1. 

Data file format
The expected data file should be in comma-separated values (csv) format with the corresponding columns in the specified order: id, start, stop, event status, discrete time-varying covariate, additional covariates if using stabilized weights (see Jay & Betensky article for details on weighting). 
Conventional survival data typically needs to be transformed using the tmerge() function in the R survival library. This creates a dataset with multiple rows of time intervals for each subject that represent covariate values changing over time. For more information about tmerge and time-varying covariates, please refer to the R documentation and vignette (Therneau et al.).

Customisations
a. Users can vary the transition times of the survival curves in the PLOT tab.
b. Users can compare the survival curves between multiple different transition times of their choice in the COMPARISON tab. 

References
1. Jay M, Betensky RA. Displaying survival of patient groups defined by covariate paths: Extensions of the Kaplan‐Meier estimator. Statistics in medicine. 2021 Apr 15;40(8):2024-36.
2. Therneau T, Crowson C, Atkinson E. Using time dependent covariates and time dependent coefficients in the cox model. Survival Vignettes. 2023 March 11;1-30.
3. https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/tmerge
