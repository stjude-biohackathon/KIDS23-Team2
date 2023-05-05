
library(shiny)
library(survival)
#library(survminer)
library(tidyverse)
library(fresh)
library(shinythemes)
library(shinyWidgets)
source("survfitCP.R")

ui <- fluidPage(titlePanel(div(tags$h1("Visualizing Survival Curves with Time-Varying Covariates"),img(src="SJ_Tag_H_C_PNG.Jpg", height=140, width=400) )),
                # sidebarLayout(
                #   sidebarPanel(),
                #   mainPanel(
                #     img(src="SJ_Tag_H_C_PNG.Jpg", height=140, wdith=400)
                #   )
                # ),
                navbarPage(
                  theme = "mytheme.css",  # <--- To use a theme, uncomment this
                  title="",
                  tabPanel(title=tags$h4("Description"), 
                           sidebarPanel(
                             "This app was created by BioHackathon Team 2."
                           ), # sidebarPanel
                           mainPanel(
                             h1("Survival Prediction App"),
                             h3("Introduction"),
                             p("The Survival Prediction App provides a survival prediction curve 
                               for time to event data of a group of subjects. These survival prediction 
                               curves are highly useful in biomedical research and clinical trials
                               concerning effectiveness of treatments and interventions. It typically 
                               deals with clinical datasets containing information about time-to-event occurrence
                               of death, withdrawal, relapse, adverse drug reaction and the appearance of 
                               secondary infection or disease.", style="color:black"),
                             h3("About the App"),
                             p("The app has three main tabs. The Description tab consists of the basic description of
                               the app, the Data tab is for user defined data input and visualization, and the Plot
                               tab is to view the Jay-Betensky survial curves in the presence of time-varying covariates",
                               span(tags$b(tags$a(href="https://www.pubmed.ncbi.nlm.nih.gov/33530128/", "(ref)."))), style="color:black"),
                             h4("Input"),
                             p("Users can input data saved as a .csv file. User defined input data needs to be formatted as a data.frame with the corresponding columns
                               in the specified order: id, start, stop, event status, discrete time-varying covariate, additional
                               covariates if using stabilized weights. All the input data needs to be transformed using the",
                               span(tags$code("tmerge()")),
                               "function in the",
                               span(tags$code("R survival")),
                               "library",
                               span(tags$b(tags$a(href="https://cran.r-project.org/web/packages/survival/index.html", "(ref)."))),
                               "This will allow the users to create a dataset with multiple time
                               intervals for the different covariate values for each subject. For more information, please refer to 
                               the", 
                               span(tags$code("tmerge()")),
                               span(tags$b(tags$a(href="https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/tmerge", "documentation."))), style="color:black"),
                             p("Example datasets include GCD, PBC in the survival package.", style="color:black"),
                             p("Note: The app only supports binary (0,1) time-depedent covariates at this time.", style="color:black"),
                             h4("Customizations in the app"),
                             p("> Transition Time", style="color:black"),
                             p("> Whether to show confidence intervals", style="color:black"),
                             p("> Whether to plot adjusted or unadjusted curves", style="color:black"),
                             h4("Output"),
                             p("The output is a 2D survival prediction curve that gives the probabiliyt of survival
                               (Y axis) over a given length of time (X axis), cosnidering time in several small
                               intervals.", style="color:black"),
                             h3("References"),
                             p("1. Jay M, Betensky RA. Displyaing survival of patient groups defined by covariate paths:
                               Extensions of the Kaplan-Meier estimator. Statistics in Medicine. 2021 Apr 15;40(8):2024-36.", style="color:black"),
                             p("2. Therneau T, Crowson C, Atkinson E. Using time dependent covariates and time dependent coefficients
                               in the Cox model. Survival Vignettes. 2023 March 11;1-30.", style="color:black"),
                             p("3. https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/tmerge", style="color:black")
                             
                           ) # mainPanel
                  ), # Navbar 1, tabPanel
                  tabPanel(title=tags$h4("Data"), 
                           sidebarLayout(sidebarPanel(
                             fileInput('file','File input',
                                       accept=c('text/csv',
                                                'text/comma-separated-values,text/plain',
                                                '.csv'))),
                             mainPanel(
                               DT::dataTableOutput('contents'),
                               plotOutput('distPlot')
                             )
                           )    
                  ),
                  tabPanel(title=tags$h4("Plot"), 
                           sidebarLayout(
                             sidebarPanel(
                               #fileInput("file","File input"),
                               #numericInput(inputId = "transition_time", label = "Transition Time", value = 1),
                               setSliderColor(c("#d11947"), c(1)),
                               sliderInput(inputId = "transition_time", label = "Transition Time", min = 0, max = 10, step = 0.1, value = 1),
                               checkboxInput("CI_checkbox",label = "CI Bands Yes/No", value = TRUE),
                               checkboxInput("weight", label = "Adjusted Yes/No", value = TRUE),
                               downloadButton("KMplot", "Save Survival Curve Plot")
                             ),
                             mainPanel(
                               h2("Jay-Betensky Survival Curve "),
                               plotOutput("KM"),
                               h2("Comparison of Transition Times"),
                               plotOutput("compare_plot")
                             )
                           )
                  )
                  
                ) # navbarPage
) # fluidPage


# Define server function
server <- function(input, output) {
  raw_surv_data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  output$contents <- DT::renderDataTable({
    DT::datatable(raw_surv_data())
  })
  surv_data <- reactive({
    raw_surv <- raw_surv_data()
    # data.frame(
    #   Time = raw_surv[[input$Survival]],
    #   Treatment    = raw_surv[[input$Treatment]],
    #   Endpoint  = raw_surv[[input$Endpoint]]
    # )
  })
  cp_survfit <- reactive({
    survfitCP(surv_data(), covariate_vals=c(0,1),
              transition_times = input$transition_time, weights = input$weight)
  })
  KM_plot <- reactive({
    p <- ggplot(data = cp_survfit(), aes(t, surv)) +
      geom_step(size = 0.75,color = "red") +
      labs(x = "Time in Years", y = "Survival Probability") +
      {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    p
  })
  output$KM <- renderPlot({
    KM_plot()
  })
  output$KMplot <- downloadHandler(
    filename = function() {
      paste("survival_curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = KM_plot())
    }
  )
  
  ## add comparison  plot
  
  adjusted_normal_to_high_5 <- reactive({
    survfitCP(surv_data(), 
              covariate_vals = c(0,1), # Values on covariate path
              transition_times = 5, # Transition from covariate value 0 to 1 at time 5
              weights = T) # Use stabilized weights
  })
  
  adjusted_high <- reactive({
    survfitCP(surv_data(), covariate_vals = c(0,1), transition_times = 0, weights = T)
    
  })
  
  
  
  all_data <- reactive({
    data.frame(rbind(cbind(adjusted_high(), "transition" = 0),
                     cbind(adjusted_normal_to_high_5(), "transition" = 5)))
  })
  
  
  all_data_factor <- reactive({
    data <- all_data()
    data$transition <- as.factor(data$transition)
    return(data)
  })
  
  
  compareplot <- reactive({
    p <- ggplot(all_data_factor(), aes(t,surv)) + geom_step(aes(color = transition)) +
      geom_ribbon(aes(color = transition, ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
      labs(x = "Time", y = "Survival Probability") +
      scale_color_discrete(name = "Transition") +
      theme(legend.position = "bottom") 
    
    p
  })
  
  output$compare_plot <- renderPlot({
    compareplot()
  })
  
} # server
# Create Shiny object
shinyApp(ui = ui, server = server)
