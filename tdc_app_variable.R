
library(shiny)
library(survival)
#library(survminer)
library(tidyverse)
library(fresh)
library(shinythemes)
library(shinyWidgets)
source("survfitCP.R")
source("function.R")
library(dplyr)
library(DT)
library(RColorBrewer)

ui <- fluidPage(titlePanel(div(tags$h1("Visualizing Survival Curves with Time-Varying Covariates"),img(src="SJ_Tag_H_C_PNG.Jpg", height=140, width=400) )),
                # sidebarLayout(
                #   sidebarPanel(),
                #   mainPanel(
                #     img(src="SJ_Tag_H_C_PNG.Jpg", height=140, wdith=400)
                #   )
                # ),
                navbarPage(
                  theme = "mythemesj.css",  # <--- To use a theme, uncomment this
                  title="",
                  tabPanel(title=tags$h4("Description"), 
                           sidebarPanel(
                             p(h3("About us:")),
                             p(h4("This app was created by the St. Jude KIDS23 BioHackathon Team 2.")),
                             #p(h1(HTML(paste0("Hello O",tags$sub("2"))))),
                             p(h4(HTML(paste0("Team Lead: Subodh Selukar", tags$sup("1"))))),
                             p(h4(HTML(paste0("Team Members: Chandrika Konwar,", tags$sup("2")," Ashish Makani,", tags$sup("3"),
                                              " Yonghui Ni,", tags$sup("1"),
                                              " Anna Eames Seffernick,", tags$sup("1"), " Chengzhou Wu,", tags$sup("4"), " Emily Zeng", tags$sup("1"))))),
                             p(div(tags$h4("This work was inspired by Rosalie."),img(src="Rosalie.jpg", height=300, width=210) )),
                             p(h5(HTML(paste0(tags$sup("1"), "St. Jude Children's Research Hospital, Department of Biostatistics")))),
                             p(h5(HTML(paste0(tags$sup("2"), "CalTech")))),
                             p(h5(HTML(paste0(tags$sup("3"), "?")))),
                             p(h5(HTML(paste0(tags$sup("4"), "University of Memphis, School of Public Health, Department of Biostatistics"))))
                             
                             
                             
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
                                                '.csv')),
                             # Select Variable from the selected Dataset 
                             uiOutput('select'),
                             # show example data
                             checkboxInput("example","Example data",value=FALSE),
                           ),
                           mainPanel(
                             DT::dataTableOutput('contents'),
                             verbatimTextOutput('summary'),
                             #plotOutput('distPlot')
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
                               plotOutput("KM")
                             )
                           )
                  ),
                  tabPanel(title=tags$h4("Comparison Plots"),
                           sidebarLayout(
                             sidebarPanel(
                               
                               textInput(inputId="times", label="Times to Compare", 
                                         placeholder ="Enter values separated by a comma..."),
                               h6("Input up to 10 transition times, separated by a comma.", style="color:black"),
                               checkboxInput("CI_checkbox2", label="CI Bands Yes/No", value=TRUE),
                               checkboxInput("weights2", label="Adjusted Yes/No", value=TRUE),
                               downloadButton("CompPlot", "Save Comparison Plot")
                               #textOutput("timesresult")
                             ),
                             mainPanel(
                               #h2("Output 1"),
                               #verbatimTextOutput("times"),
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
  raw_surv_data_1 <- reactive({
    data<-raw_surv_data()
    data<-tMergeToConventional(data)
    data<-data[,5:ncol(data)]
  })
  example_data <- reactive({
    #paste0(getwd(), "/example.csv")
    data<-read.csv(paste0(getwd(), "/data_example.csv"))
  })
  output$contents <- DT::renderDataTable({
    if (input$example){DT::datatable(example_data(),rownames=FALSE,options = list(searching=FALSE, pageLength=10))
    } else{
      DT::datatable(raw_surv_data(),rownames=FALSE,options = list(searching=FALSE, pageLength=10))%>%
        #formatRound(c(2,3,4,6),digits=2)%>%
        formatStyle(columns = c(1:ncol(raw_surv_data())), 'text-align' = 'center')
    }
  })
  
  # select based on drop down variable
  output$select <- renderUI({
    selectInput("Variable","Variable",names(raw_surv_data_1()),selected=NULL)
  })
  
  output$summary <- renderPrint({
    
      df<-raw_surv_data_1()
      df<-as.data.frame(df)
  
    if (length(unique(df[[input$Variable]]))>10){
      summary<-summary(df[[input$Variable]])
    } else {summary<-table(df[[input$Variable]])}
    print(summary)
  })  

  surv_data <- reactive({
    raw_surv <- raw_surv_data()
    
  })
  cp_survfit <- reactive({
    survfitCP(surv_data(), covariate_vals=c(0,1),
              transition_times = input$transition_time, weights = input$weight)
  })
  surv_data_example<-reactive({
    raw_surv_example<-example_data()
  })
  cp_survfit_example<- reactive({
    survfitCP(surv_data_example(), covariate_vals=c(0,1),
              transition_times = input$transition_time, weights = input$weight)
  })
  KM_plot <- reactive({
    p <- ggplot(data = cp_survfit(), aes(t, surv)) +
      geom_step(size = 0.75,color = "#d11947") +
      labs(x = "Time in Years", y = "Survival Probability") +
      theme_classic()+
      {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    p
  })
  output$KM <- renderPlot({
    if (input$example){
      p <- ggplot(data = cp_survfit_example(), aes(t, surv)) +
        geom_step(size = 0.75,color = "#d11947") +
        theme_classic()+
        labs(x = "Time in Years", y = "Survival Probability") +
        {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    }
    else{
      p <- ggplot(data = cp_survfit(), aes(t, surv)) +
        geom_step(size = 0.75,color = "#d11947") +
        theme_classic()+
        labs(x = "Time in Years", y = "Survival Probability") +
        {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    }
    p
  })
  output$KMplot <- downloadHandler(
    filename = function() {
      paste("survival_curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = KM_plot())
    }
  )
  #output$value <- renderText({ input$times })
  
  # With user input data
  comp_data <- reactive({
    nums <- extract(input$times)
    tdc_list <- list()
    tdc_df_list <- list()
    for(i in 1:length(nums)){
      tempCP <- survfitCP(surv_data(),
                          covariate_vals = c(0,1), # Values on covariate path
                          transition_times = nums[i], # Transition from covariate value 0 to 1 at time 5
                          weights = input$weights2) # Use stabilized weights
      tempCP.df <- cbind.data.frame(tempCP, "transition"=paste0(nums[i]))
      tdc_list[[i]] <- tempCP
      tdc_df_list[[i]] <- tempCP.df
    }
    data <- do.call(rbind.data.frame, tdc_df_list)
    data
  })
  
  extract <- function(text){
    text <- gsub(" ", "", text)
    split <- strsplit(text, ",", fixed=FALSE)[[1]]
    as.numeric(split)
  }
  
  # adjusted_normal_to_high_5 <- reactive({
  #   survfitCP(surv_data(), 
  #             covariate_vals = c(0,1), # Values on covariate path
  #             transition_times = 5, # Transition from covariate value 0 to 1 at time 5
  #             weights = T) # Use stabilized weights
  # })
  # 
  # adjusted_high <- reactive({
  #   survfitCP(surv_data(), covariate_vals = c(0,1), transition_times = 0, weights = T)
  #   
  # })
  
  
  
  # all_data <- reactive({
  #   data.frame(rbind(cbind(adjusted_high(), "transition" = 0),
  #                    cbind(adjusted_normal_to_high_5(), "transition" = 5)))
  # })
  
  
  all_data_factor <- reactive({
    data <- comp_data()
    levs <- unique(data$transition[order(as.numeric(data$transition))])
    data$transition <- factor(data$transition, levels=levs)
    return(data)
  })
  
  # With user example data
  comp_data_example <- reactive({
    nums <- extract(input$times)
    tdc_list <- list()
    tdc_df_list <- list()
    for(i in 1:length(nums)){
      tempCP <- survfitCP(example_data(),
                          covariate_vals = c(0,1), # Values on covariate path
                          transition_times = nums[i], # Transition from covariate value 0 to 1 at time 5
                          weights = input$weights2) # Use stabilized weights
      tempCP.df <- cbind.data.frame(tempCP, "transition"=paste0(nums[i]))
      tdc_list[[i]] <- tempCP
      tdc_df_list[[i]] <- tempCP.df
    }
    data <- do.call(rbind.data.frame, tdc_df_list)
    data
  })
  
  all_data_factor_example <- reactive({
    data <- comp_data_example()
    levs <- unique(data$transition[order(as.numeric(data$transition))])
    data$transition <- factor(data$transition, levels=levs)
    return(data)
  })
  
  
  
  compareplot <- reactive({
    #pal <- c("#d11947", "#cb5036", "#bf7235", "#b28c48", "#a7a168", "#a5b28c", "#afbfae", "#c7c9c8")
    pal <- brewer.pal(n=9, name="Reds")[9:3]
    pal2 <- c("#D11947", "#C21343", "#B40E3F", "#A6093B", "#970337", "#910B3C", "#A23B61", "#B36A84", "#C599A9", "#D6C9CD", "#CFD1D0", "#AFB1B3", "#8F9296", "#6F7379", "#4F535C", "#5B5F67", "#767A7F", "#919497", "#ACAEAF", "#C7C9C8")
    pal3 <- c("#D11947", "#C11342", "#B20D3E", "#A3083A", "#940236", "#850837", "#75193E", "#662A46", "#563B4D", "#474C55")
    pal4 <- c("#8d0034","#a64355","#bc6f79", "#d0999f","#e2c5c7","#cccdcf","#a9abaf",
              "#878a90","#666a72","#474c55")
    p <- ggplot(all_data_factor(), aes(t,surv)) + geom_step(aes(color = transition)) +
      labs(x = "Time", y = "Survival Probability") +
      theme_classic() +
      labs(color="Transition Time")+
      #scale_color_discrete(name = "Transition", values=pal) +
      theme(legend.position = "bottom") +
      #scale_cor_brewer(palette="Reds", direction=-1)+
      scale_color_manual(values=pal4)+
      {if (input$CI_checkbox2) geom_ribbon(aes(color = transition, ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    
    p
  })
  
  compareplot_example <- reactive({
    #pal <- c("#d11947", "#cb5036", "#bf7235", "#b28c48", "#a7a168", "#a5b28c", "#afbfae", "#c7c9c8")
    pal <- brewer.pal(n=9, name="Reds")[9:3]
    pal2 <- c("#D11947", "#C21343", "#B40E3F", "#A6093B", "#970337", "#910B3C", "#A23B61", "#B36A84", "#C599A9", "#D6C9CD", "#CFD1D0", "#AFB1B3", "#8F9296", "#6F7379", "#4F535C", "#5B5F67", "#767A7F", "#919497", "#ACAEAF", "#C7C9C8")
    pal3 <- c("#D11947", "#C11342", "#B20D3E", "#A3083A", "#940236", "#850837", "#75193E", "#662A46", "#563B4D", "#474C55")
    pal4 <- c("#8d0034","#a64355","#bc6f79", "#d0999f","#e2c5c7","#cccdcf","#a9abaf",
              "#878a90","#666a72","#474c55")
    p <- ggplot(all_data_factor_example(), aes(t,surv)) + geom_step(aes(color = transition)) +
      labs(x = "Time", y = "Survival Probability") +
      theme_classic() +
      labs(color="Transition Time")+
      #scale_color_discrete(name = "Transition", values=pal) +
      theme(legend.position = "bottom") +
      #scale_cor_brewer(palette="Reds", direction=-1)+
      scale_color_manual(values=pal4)+
      {if (input$CI_checkbox2) geom_ribbon(aes(color = transition, ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    
    p
  })
  
  
  output$compare_plot <- renderPlot({
    if(input$example){
      q <- compareplot_example()
    }
    else{
      q<- compareplot()
    }
    q
  })
  
  output$CompPlot <- downloadHandler(
    filename = function() {
      paste("compare_survival_curve_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = compareplot())
    }
  )
} # server
# Create Shiny object
shinyApp(ui = ui, server = server)
