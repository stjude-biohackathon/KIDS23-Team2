
library(shiny)
library(survival)
library(survminer)
library(tidyverse)
library(fresh)
library(shinythemes)
library(shinyWidgets)
source("survfitCP.R")
library(dplyr)
library(DT)
library(RColorBrewer)
library(ggpubr)
library(gridExtra)
library(cowplot)

ui <- fluidPage(titlePanel(div(tags$h1("KIDS23 Time-Dependent Covariate Survival Visualization App"),img(src="SJ_Tag_H_C_PNG.Jpg", height=140, width=400) )),
                # sidebarLayout(
                #   sidebarPanel(),
                #   mainPanel(
                #     img(src="SJ_Tag_H_C_PNG.Jpg", height=140, wdith=400)
                #   )
                # ),
                navbarPage(
                  theme = "mythemesj.css",  # <--- To use a theme, uncomment this
                  title="",
                  tabPanel(title=tags$h4("DESCRIPTION"), 
                           sidebarPanel(
                             p(h4("About us:")),
                             p(h6("This app was created by the St. Jude KIDS23 BioHackathon Team 2.", style="color:black")),
                             #p(h1(HTML(paste0("Hello O",tags$sub("2"))))),
                             p(h6(HTML(paste0("Team Lead: Subodh Selukar", tags$sup("1"))), style="color:black")),
                             p(h6(HTML(paste0("Team Members: Chandrika Konwar,", tags$sup("2")," Ashish Makani,", tags$sup("3"),
                             " Yonghui Ni,", tags$sup("1"),
                               " Anna Eames Seffernick,", tags$sup("1"), " Chengzhou Wu,", tags$sup("4"), " Emily Zeng", tags$sup("1"))), style="color:black")),
                             p(tags$h6("This work was inspired by Rosalie.", style="color:black")),
                             div(img(src="Rosalie.jpg", height=300, width=210), style="text-align:center"),
                             p(h6(HTML(paste0(tags$sup("1"), "St. Jude Children's Research Hospital, Department of Biostatistics")), style="color:black")),
                             p(h6(HTML(paste0(tags$sup("2"), "California Institute of Technology, Biology and Bioengineering Division")), style="color:black")),
                             p(h6(HTML(paste0(tags$sup("3"), "?")))),
                             p(h6(HTML(paste0(tags$sup("4"), "University of Memphis, School of Public Health, Department of Biostatistics")), style="color:black"))
                             
                             
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("About the App"),
                             h3("Background"),
                             p("The TDC Vis App is an R Shiny App that can perform survival analysis using an appropriate user-uploaded file.
                             Typical examples of survival analysis clinical data include information about time-to-event occurrence of death,
                             withdrawal, relapse, adverse drug reaction and the appearance of secondary infection or disease.
                               The main objective of the app is to provide an intuitive interface for interpreting survival analysis results
                               with time-varying covariates. Extensions of the Cox proportional hazard to study the association between a variable 
                               that can change after baseline and a time-to-event outcome of interest exist, but the interpretations can be challenging.
                               A new approach described by Jay & Betensky (2021) overcomes this problem by predicting the survival function for
                               a user-specified trajectory of one time-varying covariate. The TDC Vis app uses this method to predict survival curves
                               for interpretable results of biomedical research.", style="color:black"),
                             h3("How to use the App"),
                             p("Once the user uploads the data file (specification below) in the DATA tab, the app provides a survival prediction curve
                             that can be viewed or downloaded in the PLOT tab. This plot is accompanied by a ‘number at risk table’, the overall number
                             of subjects at risk at fixed time points. The app also allows for comparison of multiple different transition times in the
                             COMPARISON PLOT tab",style="color:black"),
                             p(tags$b("Please Note: The app currently only supports binary (0,1) time dependent covariates with a one-way transition.
                               The transition time represents the time at which a group transitions from covariate value 0 to value 1. "), style="color:black"),
                             h4("Data File Format"),
                             p("The expected data file should be in comma-separated values (csv) format with the corresponding columns in the 
                             specified order: id, start, stop, event status, discrete time-varying covariate, additional covariates if using 
                             stabilized weights (see Jay & Betensky article for details on weighting). Conventional survival data typically 
                               needs to be transformed using the ",
                               span(tags$code("tmerge()")), "function in the ",
                               span(tags$code("R survival")), "library. This creates a dataset with 
                               multiple rows of time intervals for each subject that represent covariate values changing over time.
                               For more information about tmerge and time-varying covariates, please refer to the R documentation and vignette 
                               (Therneau et al.).", style="color:black"),
                             h4("Customizations in the app"),
                             p("a. Users can vary the transition times of the survival curves in the PLOT tab.", style="color:black"),
                             p("b. Users can compare the survival curves between multiple different transition times of their choice in the COMPARISON tab.", style="color:black"),
                             h3("References"),
                             p("1. Jay M, Betensky RA. Displyaing survival of patient groups defined by covariate paths:
                               Extensions of the Kaplan-Meier estimator. Statistics in Medicine. 2021 Apr 15;40(8):2024-36", 
                               span(tags$b(tags$a(href="https://www.pubmed.ncbi.nlm.nih.gov/33530128/", "(link)."))), style="color:black"),
                             p("2. Therneau T, Crowson C, Atkinson E. Using time dependent covariates and time dependent coefficients
                               in the Cox model. Survival Vignettes. 2023 March 11;1-30",
                               span(tags$b(tags$a(href="https://cran.r-project.org/web/packages/survival/index.html", "(link)."))),style="color:black"),
                             p("3.", span(tags$b(tags$a(href="https://www.rdocumentation.org/packages/survival/versions/3.5-5/topics/tmerge", "tmerge."))), style="color:black")
                             
                           ) # mainPanel
                           ), # Navbar 1, tabPanel
                  tabPanel(title=tags$h4("DATA"), 
                           sidebarLayout(sidebarPanel(
                             fileInput('file','File input',
                                       accept=c('text/csv',
                                                'text/comma-separated-values,text/plain',
                                                '.csv')),
                             checkboxInput("example","Example data",value=FALSE),
                             ),
                             mainPanel(
                               DT::dataTableOutput('contents'),
                               plotOutput('distPlot')
                             )
                           )    
                  ),
                  tabPanel(title=tags$h4("PLOT"), 
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
                               h6("* Raw number at risk.", style="color:black")
                             )
                           )
                  ),
                  tabPanel(title=tags$h4("COMPARISON PLOT"),
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
    data<-data[,5:8]
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
  surv_data_example<-reactive({
    raw_surv_example<-example_data()
  })
  cp_survfit_example<- reactive({
    survfitCP(surv_data_example(), covariate_vals=c(0,1),
              transition_times = input$transition_time, weights = input$weight)
  })
  KM_plot <- reactive({
    # p <- ggplot(data = cp_survfit(), aes(t, surv)) +
    #   geom_step(size = 0.75,color = "#D11947") +
    #   labs(x = "Time in Years", y = "Survival Probability") +
    #   theme_classic()+
    #   {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
    p <- grid.arrange(ggplot(data = cp_survfit(), aes(t,surv)) +
                        geom_step(linewidth = 0.75, color = "#D11947") +
                        labs(x = "Time in Years", y = "Survival Probability")+
                        theme_classic() +
                        {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)},
                      ggrisktable(survfit(Surv(tstart, tstop, endpt) ~ 1, data = example_data()), data = example_data(),
                                  break.time.by = 3, fontsize = 3, ylab = "",
                                  risk.table.title = "Number at risk*"))
      
    p
  })
  output$KM <- renderPlot({
    if (input$example){
      # p <- ggplot(data = cp_survfit_example(), aes(t, surv)) +
      #   geom_step(size = 0.75,color = "#D11947") +
      #   theme_classic()+
      #   labs(x = "Time in Years", y = "Survival Probability") +
      #   {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
      #
       # p <- grid.arrange(ggplot(data = cp_survfit_example(), aes(t,surv)) +
       #                         geom_step(linewidth = 0.75, color = "#D11947") +
       #                         labs(x = "Time in Years", y = "Survival Probability")+
       #                         theme_classic() +
       #                         {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)},
       #                       ggrisktable(survfit(Surv(tstart, tstop, endpt) ~ 1, data = example_data()), data = example_data(),
       #                                   break.time.by = 3, fontsize = 3, ylab = "",
       #                                   risk.table.title = "Number at risk*"))
      p1 <- ggplot(data = cp_survfit_example(), aes(t,surv)) +
                                geom_step(linewidth = 0.75, color = "#D11947") +
                                labs(x = "Time in Years", y = "Survival Probability")+
                                #theme(axis.text=element_text(size=20), axis.title=element_text(size=22))+
                                scale_x_continuous(name="Time", breaks=c(0,3, 6, 9, 12), limits=c(0, 12))+
                                theme_classic() +
        {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
      q <- ggrisktable(survfit(Surv(tstart, tstop, endpt) ~ 1, data = example_data()), data = example_data(),
                                                         break.time.by = 3, fontsize = 5, ylab = "",
                                                         risk.table.title = "Number at risk*", axes.offset=F, xlim(0,12))
      #theme(axis.text=element_text(size=20), axis.title=element_text(size=22))
      #p <- ggdraw()+draw_plot(p1, 0, 0.5, 1, 0.5) + draw_plot(q, 0, 0, 1, 0.5)
      p <- ggarrange(p1, q, heights=c(12, 4), nrow=2, align="v")
    }
    else{
      # p <- ggplot(data = cp_survfit(), aes(t, surv)) +
      #   geom_step(size = 0.75,color = "#D11947") +
      #   theme_classic()+
      #   labs(x = "Time in Years", y = "Survival Probability") +
      #   {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)}
       p <- grid.arrange(ggplot(data = cp_survfit(), aes(t,surv)) +
                               geom_step(linewidth = 0.75, color = "#D11947") +
                               labs(x = "Time in Years", y = "Survival Probability")+
                               theme_classic() +
                               {if (input$CI_checkbox) geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2)},
                             ggrisktable(survfit(Surv(tstart, tstop, endpt) ~ 1, data = example_data()), data = example_data(),
                                         break.time.by = 3, fontsize = 3, ylab = "",
                                         risk.table.title = "Number at risk*"))
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