
shine.coxph=function(...)
  
{
  ########################
  # determine the class of each input argument
  input.list=list(...)
  n.list=length(input.list)
  list.class=rep("",n.list)
  for (i in 1:n.list)
    list.class[i]=class(input.list[[i]])
  
  #######################
  # extract the coxph models from the input
  cox.list=which(list.class=="coxph")
  n.model=length(cox.list)
  model.list=vector("list",n.model)
  for (i in 1:n.model)
    model.list[[i]]=input.list[[cox.list[i]]]
  names(model.list)=names(input.list)[cox.list]
  
  #######################
  # get colors list
  clrs=input.list$clrs
  if (is.null(clrs))
    clrs=rainbow(n.model)
  
  #######################
  # convert the coxph models to coxfit objects
  coxfit.list=vector("list",n.model)
  for (i in 1:n.model)
    coxfit.list[[i]]=prep.coxfit(model.list[[i]])
  names(coxfit.list)=names(model.list)
  
  #######################
  # get different code sections
  input.data.code=write.coxfit.input.data.code(coxfit.list)
  KM.plot.code=write.KM.plot.code(coxfit.list,clrs)
  
  ########################
  # generate ui code
  ui.code=c("ui=fluidPage(",
            "             h1('Cox Model Survival Predictions'),",
            "             sidebarLayout(",
            "             sidebarPanel(",
            paste0("                          ",input.data.code$ui.code),
            "actionButton(inputId = 'go', label = 'Generate Plot'),",
            "actionButton(inputId = 'app.exit', label = 'Exit App')",
             "),",
            "mainPanel(",
            "h3('Predicted Survival Curve'),",
            "plotOutput(outputId = 'KM'))))")
  
  #########################
  # generate server code
  
  
}

################################
# write the shiny code to generate plots

write.KM.plot.code=function(cox.fit.list,clrs)
{
  n.models=length(cox.fit.list)
  ui.code=server.code=NULL
  
  ############
  # server code to initialize KM.hat object
  
  initialize.KM.hat=c("n.models=length(cox.fit.list)",
                      "KM.hat=vector('list',n.models)",
                      "lp=rep(NA,n.models)",
                      "names(KM.hat)=names(cox.fit.list)")
  server.code=c(server.code,
                initialize.KM.hat)
  
  ##########
  # server code to compute KM.hat object
  
  compute.KM.hat=c("for (i in 1:n.models)",
                   "{",
                   "   km.hat=predict.one.cox.fit(cox.fit.list[[i]],new.data)",
                   "   lp[i]=attr(km.hat,'lp')",
                   "   sfit=list(time=km.hat$time,surv=km.hat$surv)",
                   "   class(sfit)='survfit'",
                   "   KM.hat[[i]]=sfit",
                   "}")
  server.code=c(server.code,
                compute.KM.hat)
  
  #########
  # server and ui code to display KM plots
  
  display.KM.server=c("output$KM=renderPlot({cox.KM.plots(KM.hat,clrs=clrs)})")
  display.KM.ui=c("plotOutput(outputId = 'KM')")
  
  ui.code=c(ui.code,
            display.KM.ui)
  server.code=c(server.code,
                display.KM.server)
  
  res=list(ui.code=ui.code,
           server.code=server.code)
  
  return(res)
}


#############################
# Generate Cox predicted KM plots

cox.KM.plots=function(KM.hat,clrs=NULL)
  
{
  n.models=length(KM.hat)
  if (is.null(clrs))
    clrs=rainbow(n.models)
  
  if (is.null(names(KM.hat)))
    names(KM.hat)=paste0("model ",1:n.models)
  
  max.time=0
  for (i in 1:n.models)
    max.time=max(max.time,
                 max(KM.hat[[i]]$time,na.rm=T))
  
  plot(c(0,1.2*max.time),
       c(0,1),xlab="Time",las=1,
       ylab="Prob",type="n")
  
  for (i in 1:n.models)
    lines(KM.hat[[i]],col=clrs[i])
  
  legend(1.05*max.time,1,
         col=clrs,lwd=1,
         legend=names(KM.hat),
         cex=0.5)
}


#################################
# write the shiny code to obtain user inputs

write.coxfit.input.data.code=function(cox.fit.list)
  
{

  if(is.null(names(cox.fit.list)))
    names(cox.fit.list)=paste0("model ",1:length(cox.fit.list))
  
  ###############
  # Get the set of input variables across all models
  vnames=get.vnames.cox.fits(cox.fit.list)
  
  ############
  # Get the range of numeric predictor variables
  num.x.rng.mtx=get.xrng.cox.fits(cox.fit.list,
                                  vnames)
  
  ###########
  # Get the levels of categorical predictor variables
  cat.lvls=get.levels.cox.fits(cox.fit.list,
                               vnames)

  #############
  # Generate shiny code for each variable
  
  ui.code=server.code=NULL # initialize shiny code for ui and server
  
  if(!is.null(cat.lvls))
  {
    for (i in 1:length(cat.lvls))
    {
      cat.pick=ez.pickone(names(cat.lvls)[i],
                          names(cat.lvls)[i],
                          cat.lvls[i])
      ui.code=c(ui.code,cat.pick$ui.code)
      server.code=c(server.code,
                    cat.pick$server.code)
    }
  }
  
  if(!is.null(num.x.rng.mtx))
  {
    for (i in 1:ncol(num.x.rng.mtx))
    {
      x.slider=ez.slider(colnames(num.x.rng.mtx)[i],
                         colnames(num.x.rng.mtx)[i],
                         num.x.rng.mtx[1,i],
                         num.x.rng.mtx[2,i],
                         mean(num.x.rng.mtx))
      ui.code=c(ui.code,x.slider$ui.code)
      server.code=c(server.code,
                    x.slider$server.code)
    }
  }
  
  new.data.code=paste0("new.data = cbind.data.frame(",
                       paste0(server.code,collapse=","),")")
  
  code.res=list(ui.code=ui.code,
                server.code=new.data.code)
  
  return(code.res)
  
}

#####################################
# get the set of unique predictor 
# variable names
# from a list of cox.fit objects

get.vnames.cox.fits=function(cox.fit.list)
{
  n.models=length(cox.fit.list)
  var.name=NULL
  var.type=NULL
  for (i in 1:n.models) # loop over models
  {
    var.name=c(var.name,
               names(cox.fit.list[[i]]$types))
    var.type=c(var.type,
               cox.fit.list[[i]]$types)
  }
  dup.name=duplicated(var.name)
  var.name=var.name[!dup.name]
  var.type=var.type[!dup.name]
  
  res=cbind.data.frame(var.name=var.name,
                       var.type=var.type)
  
  return(res)
}

###############################
# Get the levels for the categorical variables

get.levels.cox.fits=function(cox.fit.list,vnames)
{
  cat.vars=which(vnames[,"var.type"]!="numeric")
  if (length(cat.vars)==0)
    return(NULL)
  
  n.vars=length(cat.vars)
  n.models=length(cox.fit.list)
  cat.lvls=vector("list",n.vars)
  cat.names=vnames[cat.vars,"var.name"]
  names(cat.lvls)=cat.names
  
  for (i in 1:n.models)
  {
    mod.lvls=cox.fit.list[[i]]$xlevels
    mod.vars=names(mod.lvls)
    for (j in mod.vars)
    {
      cat.lvls[[j]]=c(cat.lvls[[j]],
                      mod.lvls[[j]])
    }
  }
  
  for (j in 1:length(cat.lvls))
    cat.lvls[[j]]=unique(cat.lvls[[j]])
  
  cat.names=gsub("strata(","",cat.names,fixed=T)
  cat.names=gsub(")","",cat.names,fixed=T)
  names(cat.lvls)=cat.names
  
  return(cat.lvls)
  
}

####################################
# Get the range of the numeric variables
# across a list of cox.fit objects

get.xrng.cox.fits=function(cox.fit.list,vnames)
  
{
  num.vars=which(vnames[,"var.type"]=="numeric")
  if (length(num.vars)==0)
    return(NULL)
  
  n.models=length(cox.fit.list)
  rng.mtx=matrix(NA,2,length(num.vars))
  rng.mtx[1,]=NA
  rng.mtx[2,]=NA
  colnames(rng.mtx)=vnames[num.vars,"var.name"]
  
  for (i in 1:n.models)
  {
    x.rng=cox.fit.list[[i]]$x.rng # num.x.rng does not exist in this iteration; change to read in all 
    for (j in num.vars) # change to index only numeric variables rather than all of x.rng, which includes non-numeric variables
    {
      x.name=colnames(x.rng)[j]
      rng.mtx[1,x.name]=min(x.rng[1,j],rng.mtx[1,x.name],na.rm=T) #  min across models
      rng.mtx[2,x.name]=max(x.rng[2,j],rng.mtx[2,x.name],na.rm=T)  # max across models, changed from min
    }
  }
  return(rng.mtx)
}

