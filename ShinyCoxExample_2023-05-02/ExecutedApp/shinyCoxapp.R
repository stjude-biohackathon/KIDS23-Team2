#rm(list=ls())
options(stringsAsFactors=F)
load('~/Research/Projects/Methods/ShinyCox/TestApps//2023-05-03-09-25-35/appData.Rdata')
ui=fluidPage(
             h1('Cox Model Survival Predictions'),
             sidebarLayout(
             sidebarPanel(
                          selectInput(inputId = 'sex', label = 'sex', choices = c("female", "male")),
                          selectInput(inputId = 'grp', label = 'grp', choices = c("A", "B", "C")),
                          selectInput(inputId = 'strt', label = 'strt', choices = c("strt=1", "strt=2")),
                          sliderInput(inputId = 'u', label = 'u', min = 0, max = 1, value = 0.499406939197797),
                          sliderInput(inputId = 'v', label = 'v', min = 0.000975985778495669, max = 0.996651771012694, value = 0.499406939197797),
textInput('predProbTimes','Times for predicted probabilities',placeholder='Enter values separated by a comma'),
actionButton(inputId = 'go', label = 'Generate Plot'),
actionButton(inputId = 'reset', label = 'Reset'),
actionButton(inputId = 'app.exit', label = 'Exit App'),
),
mainPanel(
h3('Predicted Survival Curve'),
plotOutput(outputId = 'KM'),
h3('Predicted Probability at Fixed Times'),
textOutput(outputId='noPredTimes'),
tableOutput(outputId = 'cox.times'),
h3('Hazard Ratio Summary Table'),
tableOutput(outputId = 'HR'),
h3('Assessing the Proportional Hazards Assumption'),
tableOutput(outputId = 'PHA')
)))
server=function(input,output)
{
          observeEvent(input$app.exit, {stopApp()}) # Exit when exit button is pressed
          observeEvent(input$go, {
selectInput(inputId = 'sex', label = 'sex', choices = c("female", "male"))
selectInput(inputId = 'grp', label = 'grp', choices = c("A", "B", "C"))
selectInput(inputId = 'strt', label = 'strt', choices = c("strt=1", "strt=2"))
sliderInput(inputId = 'u', label = 'u', min = 0, max = 1, value = 0.499406939197797)
sliderInput(inputId = 'v', label = 'v', min = 0.000975985778495669, max = 0.996651771012694, value = 0.499406939197797)
new.data = cbind.data.frame(sex = input$sex,grp = input$grp,strt = input$strt,u = input$u,v = input$v)
plotOutput(outputId = 'KM')
n.models=length(cox.fit.list)
KM.hat=vector('list',n.models)
lp=rep(NA,n.models)
names(KM.hat)=names(cox.fit.list)
for (i in 1:n.models)
{
   km.hat=predict.one.coxfit(cox.fit.list[[i]],new.data)
   lp[i]=attr(km.hat,'lp')
   sfit=list(time=km.hat$time,surv=km.hat$surv)
   class(sfit)='survfit'
   KM.hat[[i]]=sfit
}
output$KM=renderPlot({cox.KM.plots(KM.hat,clrs=clrs)})
predProbTable <- cox.times.table(KM.hat,input$predProbTimes)
if (is.null(predProbTable)) output$noPredTimes <- renderText('No input times detected. If you provided times, check that you separated numbers with a single comma and you provided valid numbers.') else output$noPredTimes <- renderText(invisible())
output$cox.times=renderTable(predProbTable,rownames=TRUE)
output$HR=renderTable(cox.fit.list[[1]]$HR.table,rownames=TRUE)
output$PHA=renderTable(cox.fit.list[[1]]$PHA.table$table,rownames=TRUE)
})
          observeEvent(input$reset, {output$KM <- output$HR <- output$PHA <- output$cox.times <- NULL}) # Reset main area
}
cox.app=shinyApp(ui,server)
runApp(cox.app)
