KIDS23 Survival Prediction App


About the app
The KIDS23 Survival Prediction App is a R Shiny App that can perform survival analysis using any user-uploaded file. Once the user uploads the data file in the DATA tab, the app provides a survival prediction curve that can be viewed or downloaded in the PLOT tab. These survival prediction curves are highly useful in biomedical research and clinical trials concerning effectiveness of treatments and interventions. A typical analysis deals with clinical data containing information about time-to-event occurrence of death, withdrawal, relapse, adverse drug reaction and the appearance of secondary infection or disease.


Data File Format
The ideal data file should be in comma-separated values (csv) format with the corresponding columns in the specified order: id, start, stop, event status, discrete time-varying covariate, additional covariates if using stabilized weights. The data file needs to be transformed using the tmerge() function in r survival library. This will allow the users to create a dataset with multiple time intervals for the different covariate values for each subject. For more information, please refer to the r documentation/vignette.


Customisations
a. Users can play with the transition times of the survival curves in the PLOT tab. It also gives information about the 'number at risk.' 
b. Users can compare the survival curves between two different transition times of their choice in the COMPARISON tab. 


#Please Note: The app only supports binary (0,1) time dependent covariates.

References
1. Jay M, Betensky RA. Displaying survival of patient groups defined by covariate paths: Extensions of the Kaplan‐Meier estimator. Statistics in medicine. 2021 Apr 15;40(8):2024-36.
2. Therneau T, Crowson C, Atkinson E. Using time dependent covariates and time dependent coefficients in the cox model. Survival Vignettes. 2023 March 11;1-30.
3. https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/tmerge