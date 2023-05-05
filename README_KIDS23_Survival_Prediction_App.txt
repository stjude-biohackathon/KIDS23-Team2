KIDS23 Survival Prediction App

About the app
The KIDS23 Survival Prediction App is an R Shiny App that can perform survival analysis using an appropriate user-uploaded file. Typical examples of survival analysis clinical data include information about time-to-event occurrence of death, withdrawal, relapse, adverse drug reaction and the appearance of secondary infection or disease.
The main objective of the app is to provide an intuitive interface for interpreting survival analysis results with time-varying covariates. Usually, methods that use extensions of the Cox proportional hazard to study the association between a variable that can/cannot change after baseline and a time-to-event outcome of interest fail to accommodate more than one covariate value changes at intermediate time points. A new approach described by Jay & Betensky (2021) overcomes this problem and predicts the survival function for a user-specified trajectory of time-varying covariates. The KIDS23 app uses this specific method to predict survival curves for interpretable results of biomedical research and clinical trials concerning effectiveness of treatments and interventions. 

How to use the app
Once the user uploads the data file in the DATA tab, the app provides a survival prediction curve that can be viewed or downloaded in the PLOT tab. This plot is accompanied by a ‘number at risk table’ that gives information about number of subjects at risk in each group at each relevant time point. The app also allows for comparison of two different transition times in the COMPARISON PLOT tab.

Data File Format
The expected data file should be in comma-separated values (csv) format with the corresponding columns in the specified order: id, start, stop, event status, discrete time-varying covariate, additional covariates if using stabilized weights. The data needs to be transformed using the tmerge() function in r survival library. This will allow the users to create a dataset with multiple time intervals for the different covariate values for each subject. For more information about tmerge and time-varying covariates, please refer to the r documentation/vignette.

Customisations
a. Users can play with the transition times of the survival curves in the PLOT tab. It also gives information about the 'number at risk.' 
b. Users can compare the survival curves between two different transition times of their choice in the COMPARISON tab. 
#Please Note: The app only supports binary (0,1) time dependent covariates.

References
1. Jay M, Betensky RA. Displaying survival of patient groups defined by covariate paths: Extensions of the Kaplan‐Meier estimator. Statistics in medicine. 2021 Apr 15;40(8):2024-36.
2. Therneau T, Crowson C, Atkinson E. Using time dependent covariates and time dependent coefficients in the cox model. Survival Vignettes. 2023 March 11;1-30.
3. https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/tmerge
