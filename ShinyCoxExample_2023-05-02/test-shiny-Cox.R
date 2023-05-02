#######################################
# Initial set-up

rm(list=ls()); options(stringsAsFactors=F)
library(survival)


##################################
# Generate an example data set

n=500
x=rnorm(n)              # predictor x
u=runif(n)              # predictor u
v=rexp(n)               # predictor v
tm=rexp(n)                  # event time
ev=rbinom(n,1,0.5)          # event indicator
srv=Surv(tm,ev)
grp=sample(LETTERS[1:3],n,T)
sex=sample(c("male","female"),n,T)
strt=rep(1:2,n/2)
strt2=rep(LETTERS[1:2],each=n/2)


dset=cbind.data.frame(grp=grp,x=x,
                      u=u,v=v,
                      sex=sex,
                      ev=ev,tm=tm,
                      strt=strt,
                      srv=srv)

##########################################
# Fit a cox model to the example data set

cox.fit1=coxph(srv~sex+u+v+grp+strata(strt),
               data=dset,
               x=T,model=T)

########################################
# Remove example data set to ensure no inadvertent access to it while building app

rm(dset)

########################################
# Generate an app for the example data set model


source("~/Research/Projects/Methods/ShinyCox/cox-predictions.R")
source("~/Research/Projects/Methods/ShinyCox/shiny-blocks.R")
source("~/Research/Projects/Methods/ShinyCox/shiny-builder.R")
app.dir="~/Research/Projects/Methods/ShinyCox/TestApps/"

test.res=shine.coxph(cox.fit1,         # pass the coxph fit result
                     app.dir=app.dir,  # application directory
                     launch.app=T)     # indicates whether to launch the app after it is created
