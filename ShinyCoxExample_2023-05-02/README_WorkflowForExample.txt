Workflow for working example of ShinyCox (a related previous project)

Note: there is no expectation to have the 2023 biohackathon product write Shiny apps; instead, current focus is on developing a single interactive

Steps for working example:
1. Download the following files:
	cox-predictions.R
	shiny-blocks.R
	shiny-builder.R
	test-shiny-Cox.R
2. Review and update the file paths in test-shiny-Cox.R
	Note: a new folder should be created and its path should be used for app.dir; this folder will hold the interactive application
3. source("test-shiny-Cox.R")
	Note: this will write one application into the folder app.dir
4. Without exiting R (i.e., using the same R environment as step 3), open shinyCoxapp.R in the app.dir folder
5. Run the app 
	In Rstudio default settings, "Run App" is in the upper right of the scripting section where "Source" typically is

Necessary packages:
	survival
	shiny
	Rstudio IDE (I'm not sure if it is necessary or just very helpful)