
library(shiny)
library(survival)
#library(survminer)
library(tidyverse)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fileInput("file","File input"),
      
      # selectInput("Survival",
      #             label = "Length of Survival",
      #             choices =  c('survival1',
      #                          'survival2'),
      #             selected = 'survival1'),
      
      # selectInput("Treatment",
      #             "Treatment",
      #             choices = c("treatment1", "treatmentt2"),
      #             selected = "treatment1"),
      
      # selectInput("Endpoint",
      #             "Endpoint",
      #             choices = c("endpoint1", "endpoint2"),
      #             selected = "endpoint1")
      
      ## have user be able to change transition time   
      numericInput(inputId = "transition_time", label = "Transition Time", value = 1),
      
      checkboxInput("CI_checkbox",label = "CI Bands Yes/No", value = TRUE)
      
    ),
    mainPanel(
      plotOutput("KM")
      #textOutput('modelSummary')
    )
  )
)

server <- function(input, output) {
  options(shiny.maxRequestSize=40*1024^2)
  raw_surv_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  surv_data <- reactive({
    raw_surv <- raw_surv_data()
    # data.frame(
    #   Time = raw_surv[[input$Survival]],
    #   Treatment    = raw_surv[[input$Treatment]],
    #   Endpoint  = raw_surv[[input$Endpoint]]
    # )
  })
  
  
  # surv_fit <- reactive({
  # 
  #   survfit(Surv(tstart , tstop) ~ female, data = surv_data())
  # })
  
  cp_survfit <- reactive({

    survfitCP(surv_data(), covariate_vals=c(0,1),
              transition_times = input$transition_time, weights = T)
  })
  
  # output$KM <- renderPlot({
  #   ggsurvplot(surv_fit(), risk.table = TRUE, data = surv_data())
  # })
  
  output$KM <- renderPlot({
    p <- ggplot(data = cp_survfit(), aes(t, surv)) + 
      geom_step(size = 0.75) + 
      labs(x = "Time in Years", y = "Survival Probability") +
      {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)} 
    
    p

  })
  
  # cox_fit <- reactive({
  #   coxph(Surv(Time, Endpoint) ~ Treatment, data = surv_data())
  # })
  
  
  # output$modelSummary <- renderPrint({
  #   summary(cox_fit)
  # })
  
  
}

shinyApp(ui = ui, server = server)
