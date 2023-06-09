
######################################################################
# Create a simplified representation of a coxph model fit 
# with minimum information necessary to compute model predictions

prep.coxfit=function(coxph.result,  # result of coxph
                     tol=1e-7)      # numerical tolerance of prediction differences
  
{
  check.coxph(coxph.result)               # check that coxph.result is adequate for our purposes
  cox.fit=simplify.coxph(coxph.result)    # simplify coxph.result to cox.fit
  check.coxfit(cox.fit,coxph.result,tol)  # check that predictions of cox.fit match those of coxph.result
  return(cox.fit)                         # return cox.fit
}


###############################################################################################
# Simplifies a coxph.result object for visualization in the app
# Strips the predictor data and model data to best protect patient privacy
# Keeps the model coefficients and the assignment of input data to model data

simplify.coxph=function(coxph.result)
  
{
  ########
  # extract survival and linear predictor data from coxph.result
  sdat=coxph.result$y
  lp=predict(coxph.result,type="lp")
  lp.surv=cbind.data.frame(sdat=sdat,lp=lp)    
  
  ########
  # compute the baseline Cox survival function in a data.frame format
  bl.cox=survfit(coxph.result) 
  bl.surv=cbind.data.frame(time=bl.cox$time,  # baseline survival function corresponding to baseline hazard function
                           surv=bl.cox$surv)

  ########
  # add strata column if necessary
  if (!is.null(bl.cox$strata))
  {
    strt=rep(names(bl.cox$strata),bl.cox$strata)
    eq.pos=regexpr("=",strt,fixed=T)
    strt.vname=substring(strt[1],1,eq.pos[1]-1)
    #strt=substring(strt,eq.pos+1)    
    bl.surv=cbind.data.frame(strata=strt,bl.surv)
    names(bl.surv)=c(strt.vname,"time","surv")
  }
  
  ########
  # extract tables of estimates and evaluation of proportional hazards assumption
  cox.smry=coef(summary(coxph.result))
  cox.CIs=confint(coxph.result)
  cox.pha=cox.zph(coxph.result)
  HR.tbl=cbind(Hazard.Ratio=cox.smry[,"exp(coef)"],
               Lower.Bound=exp(cox.CIs[,1]),
               Upper.Bound=exp(cox.CIs[,2]),
               p.value=cox.smry[,"Pr(>|z|)"])
  pha.tbl=cox.pha
  
  
  ########
  # extract minimal sufficient information to compute survival estimates
  
  cox.terms=attr(coxph.result$terms,"term.labels")
  cox.types=attr(coxph.result$terms,"dataClasses")
  cox.types=cox.types[cox.terms]
  cox.coefs=coef(coxph.result)
  #cox.assign=coxph.result$assign
  cox.means=coxph.result$means
  xlevels=coxph.result$xlevels
  strata=coxph.result$strata
  form=coxph.result$formula
  x.rng=apply(coxph.result$x,2,range)
  rownames(x.rng)=c("minimum","maximum")
  
  res=list(bl.surv=bl.surv,    # baseline survival function estimate
           lp.surv=lp.surv,    # subject level linear predictor and survival data
           #assign=cox.assign,  # assignment of input data values to regression model matrix values
           types=cox.types,    # vector of data types (character, numeric, strata, etc)
           coefs=cox.coefs,    # regression coefficient estimates
           means=cox.means,    # means of the regression model matrix columns
           num.x.rng=x.rng,    # range of the regression model matrix columns
           xlevels=xlevels,    # levels of categorical predictors
           strata=strata,      # strata
           form=form,          # model formula
           HR.table=HR.tbl,    # hazard ratio estimates
           PHA.table=pha.tbl)  # tests of proportional hazards assumption
  
  return(res)                  # returns list object with all the above information
}



#####################################################
# Computes predicted survival outcomes for one patient based on a Cox model fit

predict.one.coxfit=function(coxfit,          # result of prep.coxfit
                            newdata)         # new input data vector
{
  
  ok=check.coxfit.newdata(coxfit,newdata)               # check input newdata
  x=compute.coxfit.xvector(coxfit,newdata)              # convert the input data into an x vector
  coef.names=names(coxfit$coefs)                        # extract the names of the coeffients
  x.beta=sum(coxfit$coefs*(x[coef.names]-coxfit$means)) # see help(predict.coxph) for notes on centering
  res=coxfit$bl.surv                                    # initialize result as baseline survival function
  res[,"surv"]=coxfit$bl.surv$surv^exp(x.beta)          # formula for predicted survival of one new patient

  ###########
  # if stratified model, then limit to the stratum of newdata
  if (!is.null(coxfit$strata))                     
  {
    strt.var=coxfit$strata[1]
    eq.pos=regexpr("=",strt.var,fixed=T)
    strt.var=substring(strt.var,1,eq.pos-1)
    strt.mtch=(newdata[,strt.var]==res[,strt.var])
    if (!any(strt.mtch))
      stop(paste0("Unable to match strata variable ",
                  strt.var," in newdata."))
    
    res=res[strt.mtch,]
  }  
  
  res=as.data.frame(res)
  attr(res,"lp")=x.beta
  attr(res,"newdata")=newdata
  return(res)
}


#############################################
# compute the x vector for a newdata observation for a coxfit object

compute.coxfit.xvector=function(coxfit,
                                newdata)
  
{
  # create right-hand side of formula
  form=deparse(coxfit$form)
  tilde.pos=regexpr("~",form,fixed=T)
  form=substring(form,tilde.pos)

  # use model matrix to create x vector
  form=as.formula(form)
  mtx=model.matrix(form,data=newdata,xlev=coxfit$xlevels)
  x=mtx[1,names(coxfit$coefs)]
  return(x)
}


##############################################################
# Check that predictions from simplified coxph model object 
# match those of the input coxph model for each subject

check.coxfit=function(cox.fit,coxph.result,tol=1e-7)
  
{
  message("Double checking calculations from minimal coxph fit information to those from full coxph information.")
  cox.input=coxph.input.data(coxph.result)
  n=nrow(cox.input)
  for (i in 1:n)
  {
    new.data=as.data.frame(cox.input[i,-1])                        # get data for subject i
    cox.pred1=predict.one.coxfit(cox.fit,new.data)                 # prediction with new object
    cox.pred2=one.survfit(coxph.result,newdata=new.data)           # prediction by survival package
    pred.diff=abs(cox.pred1[,"surv"]-cox.pred2$surv)               # absolute value of difference
    ok=(max(pred.diff)<tol)                                        # check if within numerical error
    if (!ok)                                                       # stop if error detected
      stop("Error encountered in double-checking survival prediction calculations from reduced coxph result object.")
  }
  return(invisible())
}

###################################
# Compute predicted survival outcomes using survival package

one.survfit=function(coxph.result,newdata)
  
{
  sf.res=survfit(coxph.result,newdata=newdata)
  res=cbind.data.frame(time=sf.res$time,
                       surv=sf.res$surv)
  if (!is.null(coxph.result$strata))
  {
    strt=rep(names(sf.res$strata),
             sf.res$strata)
    res=cbind.data.frame(strata=strt,res)
    vname=res$strata[1]
    eq.pos=regexpr("=",vname,fixed=T)
    vname=substring(vname,1,eq.pos-1)
    mtch=which(res$strata==newdata[,vname])
    res=res[mtch,]
  }
  return(res)
}



#######################################
# Recreate input data from coxph.result object

coxph.input.data=function(coxph.result)
  
{
  res=coxph.result$model
  if (!is.null(coxph.result$strata))
  {
    orig.vname=coxph.result$strata[1]
    eq.pos=regexpr("=",orig.vname,fixed=T)
    orig.vname=substring(orig.vname,1,eq.pos-1)
    
    orig.value=substring(coxph.result$strata,eq.pos+1)
    
    new.vname=paste0("strata(",orig.vname,")")
    #res[,new.vname]=orig.value
    new.clm=grep(new.vname,colnames(res),fixed=T)
    colnames(res)[new.clm]=orig.vname
  }
  res=as.data.frame(res)
  return(res)
}


###############################################
# Check that a coxph result object is suitable for generating a shiny app

check.coxph=function(coxph.result)
  
{
  #########
  # stop for invalid input
  if (!any(class(coxph.result)=="coxph"))
    stop("The argument coxph.object must be the result object of the coxph function.")
  
  cox.names=names(coxph.result)
  if (!all(is.element(c("x","model"),cox.names)))
    stop("To perform this calculation, coxph.result must have components 'model' and 'x'.  Rerun coxph with model=T and x=T.")
  
  
  #########
  # get the terms of the Cox model and their data types
  cox.terms=attr(coxph.result$terms,
                 "dataClasses")       # gets all the terms of the model
  cox.terms=cox.terms[-1]             # drop the survival response variable to get predictor terms
  
  ########
  # Stop if there are no predictor terms
  if (length(cox.terms)==0)         # no predictor terms
  {
    stop("The coxph.result object has no predictor terms.")
  }
  
  return(invisible())
  
}


############################################
# Check input newdata for predict.one.coxfit

check.coxfit.newdata=function(coxfit,   # result of prep.coxfit
                              newdata)  # data.frame with data for one patient
  
{
  
  ##############
  # Check that input data has only one row
  if (is.data.frame(newdata))
  {
    if (nrow(newdata)>1)
      stop("Input data MUST have only ONE row.")
    newdata=as.vector(newdata[1,])
  }
  
  ##############
  # check that newdata contains essential names
  new.names=names(newdata)
  cox.names=names(coxfit$assign)
  if (length(setdiff(cox.names,new.names))>0)
    stop("newdata missing some essential elements for prediction.")
  
  return(invisible())
}