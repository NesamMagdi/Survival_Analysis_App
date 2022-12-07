library(ggfortify)
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(highcharter)
library(survival)
library(tidyverse)
library(shiny)
library(tidyverse)
library(shiny)
library(survminer)
library(finalfit)
library(DT)
library(gtsummary)
library(recipes)
library(shinyjs)
library(dplyr)


convertMenuItem <- function(mi,tabName) {
    mi$children[[1]]$attribs['data-toggle']="tab"
    mi$children[[1]]$attribs['data-value'] = "tabName"
    mi
}


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = tags$i("Survival Analysis"), tags$li(a(href = 'https://www.shinyapps.io/',
                                                                   img(src = 'images.png', height = "30", width="60"),
                                                                   style = "padding-top:10px; padding-bottom:10px;"),
                                                                 class = "dropdown")),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Choosing Data", tabName = "data", icon= icon("database")),
            menuItem("Exploring Data", tabName = "explore", icon= icon("fas fa-chart-line")),
            menuItem("Setting Up Variables", tabName = "variables", icon= icon("columns")),
            menuItem("Summaries Data", tabName = "summary", icon= icon("chart-line")),
            menuItem("Survival Analysis", tabName = "survival", icon=icon("stopwatch-20")),
            menuItem("Hazard Model", tabName = "hazard", icon=icon("biohazard")))),
    
    
    
    dashboardBody(
        tabItems(
            tabItem("data",
                    fluidPage(tags$i(h3("First, Upload Your Data")),
                              h4("Survival analysis, also called event history analysis in social science, or reliability
                                 analysis in engineering, deals with time until occurrence of an event of interest.
                                 However, this failure time may not be observed within the relevant time period,
                                 producing so-called censored observations.")), 
                    fluidRow(
                        box(title = "Choose Your Data", status = "primary", solidHeader = TRUE,
                            fileInput("file", h4("Select Data:")),
                            h5("Please, select .csv files only"))
                    )),
            tabItem("explore",
                    fluidPage(tags$i(h3("Second, Exploring data variables")),
                              h4("After choosing the dataset you want to work on,
                                please, check the normality of variables:")), 
                    fluidRow(
                        box(title = "Data Variables:", status = "primary", solidHeader = TRUE,
                            convertMenuItem(selectInput(inputId = "explore1",
                                                        label = "Select Variables to be Explored:",
                                                        choices="",selected="")
                            ))),
                    fluidRow(box(title = "Normality", status = "primary", solidHeader = TRUE,
                                 plotlyOutput("plot10"))),
                    fluidRow(box(title = "Shapiro Test of Normaliy (For Numeric Variables Only)", status = "primary", solidHeader = TRUE,
                                 textOutput("text1")))),
            
            tabItem("variables",
                    fluidPage(tags$i(h3("Third, Select Time, Event")),
                              h4("Please, choose the variables that represents both Time & Event
                                 in your dataset:"),
                              h4("(Median time till survival is represented in the box below)")
                    ),
                    fluidRow(box(title = "Select Time Variable", status = "primary", solidHeader = TRUE,
                                 convertMenuItem(selectInput(inputId = "selecttime",
                                                             label = "Select Time",
                                                             choices="",selected=""
                                 ))), 
                             box(title = "Select Event Variable", status = "primary", solidHeader = TRUE,
                                 convertMenuItem(selectInput(inputId = "selectevent",
                                                             label = "Select Event:",
                                                             choices="",selected=""
                                 )))),
                    fluidRow(box(title = "Kaplan-Meier Curve", status = "primary", solidHeader = TRUE,
                                 highchartOutput("plot2"), width = 6),
                             box(title = "Median time", status = "primary", solidHeader = TRUE,
                                 verbatimTextOutput("text2")))
                    
                    
            ),
            tabItem("survival",
                    fluidPage(tags$i(h3("Now, Kaplan-Meier Estimator & Curve"))),
                    fluidRow(box(title = "Select Group Variable", status = "primary", solidHeader = TRUE, 
                                 convertMenuItem(selectInput(inputId = "selectgroup",
                                                             label = "Select Strata:",
                                                             choices="",selected=""
                                 )), width = 5)),
                    fluidRow(box(title = "Kaplan-Meier Curve", status = "primary", solidHeader = TRUE,
                             highchartOutput("plot1"), width = 5)),
                    fluidRow(box(title = "Kaplan-Meier Estimator", status = "primary", solidHeader = TRUE,
                             DT::dataTableOutput("Model"), width = 13)),
                    fluidRow(box(title = "Interpretetion", status = "primary", solidHeader = TRUE,
                                 textOutput("text3"), width = 5))

            ),
            tabItem("summary",
                    fluidPage(tags$i(h3("Summary"))),
                    fluidRow(box(title = "Select Group Variable", status = "primary", solidHeader = TRUE, 
                                 convertMenuItem(selectInput(inputId = "selectgroup3",
                                                             multiple = TRUE,
                                                             label = "Select Strata:",
                                                             choices="",selected=""
                                 )), width = 5)),
                    fluidRow(box(title = "Summary Table", status = "primary", solidHeader = TRUE,
                                 gt::gt_output("table2"), width = 6))
                    
            ),
            tabItem("hazard",
                    fluidPage(tags$i(h3("Finally, Cox-Proportional Hazard Model")),
                              fluidRow(box(title = "Select Group Variable", status = "primary", solidHeader = TRUE, 
                                           convertMenuItem(selectInput(inputId = "selectgroup2",
                                                                       label = "Select Strata:",
                                                                       multiple = TRUE,
                                                                       choices="",selected=""
                                           )), width = 5)),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Cox-Proportional Hazard Model", br(),
                                                         DT::dataTableOutput("table1")),
                                            tabPanel("Plot", br(), plotOutput("plot3"),
                                                     downloadButton(outputId = "down", br(),
                                                      label = "Download the Plot"))

                                )))

            )
            
            
        )
    )
    
)



















server<-shinyServer(function(input,output,session){
    
    values <- reactiveValues(df_data = NULL)
    
    observeEvent(input$file, {
        values$df_data <- read.csv(input$file$datapath, stringsAsFactors = F)

    })

    
    observeEvent(input$file, {
        updateSelectInput(session, "selecttime", choices = names(values$df_data))
    })
    
    observeEvent(input$file, {
        updateSelectInput(session, "selectevent", choices = names(values$df_data))
    })

    
    
    observeEvent(input$file, {
        updateSelectInput(session, "explore1", choices = names(values$df_data))
    })
    
    
    observeEvent(input$file, {
        updateSelectInput(session, "selectgroup", choices = names(values$df_data))
    })
    
    
    observeEvent(input$file, {
      updateSelectInput(session, "selectgroup2", choices = names(values$df_data))
    })
    
    
    observeEvent(input$file, {
      updateSelectInput(session, "selectgroup3", choices = names(values$df_data))
    })
    
    selectedData <- reactive({
      ndf_new <- values$df_data[ , input$explore1]
    })

    
    output$text1 <- renderText({
      
      ND_G1 <- reactive({
        values$df_data %>% 
          group_by_(selectedData())%>%
          summarize_at(vars(input$explore1),
                       funs(statistic = shapiro.test(.)$statistic, 
                                                p.value= shapiro.test(.)$p.value))
      })
      
      ND_G <- reactive({
        if (ND_G1()$p.value >= 0.05) {
          "normaly distributed."
        }
        else
        {
          "not normaly distributed."
        }
      })
      
      paste("The p-value is: ",ND_G1()$p.value, ",which means that", input$explore1,
            "variable is", ND_G())
      
      })

    

    output$plot10 <- renderPlotly({
  
      plot_ly(values$df_data, x= ~eval(as.name(input$explore1)), y=~frequency(eval(as.name(input$explore1))),
              type = 'bar')%>%
        layout( xaxis = list(title = paste(input$explore1, "Variable")), 
                yaxis = list(title = 'Density')) 

    })


    
    
    x2 <- "eval(as.name(input$selectgroup))"
    y2 = "Surv(eval(as.name(input$selecttime)),eval(as.name(input$selectevent)))"
    
    form2 <- as.formula(paste(y2, "~", x2))
    
    runsurv <- reactive({
      surv_fit(form2, data = values$df_data)
    })
    
    output$plot1 <- renderHighchart({

      hchart(runsurv(), markTimes = TRUE,
             symbol = "plus",
             ranges = TRUE,
             rangesOpacity = 0.3)

    })

    y = "Surv(eval(as.name(input$selecttime)),eval(as.name(input$selectevent)))"
    
    form <- as.formula(paste(y, "~ 1"))
    
    
    runsurv2 <- reactive({
      survfit(form, data = values$df_data)
      
    })


    output$plot2 <- renderHighchart({

      
      hchart(runsurv2(), markTimes = TRUE,
             symbol = "plus",
             ranges = TRUE,
             rangesOpacity = 0.3)
      
    })
    
    
    output$text2 <- renderPrint({
      runsurv2()

    })
    
    output$Model <- DT::renderDataTable({
      summary(runsurv())$table
      
    })
    
    output$text3 <- renderText({
      paste("Median time of survival and CI, are represnted in the above table for each group")
    })
    
    
    
    
    surv_data <- reactive({
      req(input$selectgroup2)
      data.frame(
        Time = values$df_data[[input$selecttime]],
        Endpoint = values$df_data[[input$selectevent]],
        values$df_data %>%
          dplyr::group_by(across(input$selectgroup2))
      )
      
    })

    dependent <- reactive({"Surv(Time, Endpoint)"})


    cox1 <- reactive({

      surv_data() %>%
      finalfit::finalfit(dependent = dependent(), explanatory = input$selectgroup2,
                         add_dependent_label = T)
      
    })

    
    output$table1 <- DT::renderDataTable({
                      datatable(cox1())
    })
    
    
    output$plot3 <- renderPlot({
      surv_data() %>%
        hr_plot(dependent = dependent(), explanatory = input$selectgroup2,
                add_dependent_label = T)
    })
    
    
    
    output$down <- downloadHandler(
      filename = function(){
        paste("cox", "png", sep = ".")
      },
      content = function(file){
        png(file)
        
        surv_data() %>%
          hr_plot(dependent = dependent(), explanatory = input$selectgroup2,
                  add_dependent_label = T)
        dev.off()
      }
    )


    summary1 <- reactive({
      
      values$df_data %>%
        summary_factorlist(dependent = input$selectevent, explanatory = input$selectgroup3,
                           add_dependent_label = T)
      
    })
    
    
    varY <- reactive({
      input$selectevent
    })
    varX <- reactive({
      input$selectgroup3
    })

    
    
    output$table2 <- gt::render_gt({
      
      by <- varY()
      table2 <- values$df_data %>% 
        select(all_of(c(varX(), by))) %>%
        tbl_summary(by = by) %>%
        add_p(pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% 
        bold_labels() %>%
        bold_p() %>%
        modify_caption("**Patients' Characteristics by Status**") %>%
        as_gt()

      })


    

})
    
    
    
    
# Run the application 
shinyApp(ui = ui, server = server)

















