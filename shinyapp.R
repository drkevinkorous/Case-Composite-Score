# ------------------------------------------------------------------------ #
#
# This is an Example Shiny web application. 
# Originally created to compute a composite score and its distribution
# Trimmed down for simplicity and public facing
# Click the 'Run App' button above to execute
#
# ------------------------------------------------------------------------ #

# Packages Needed ----
library(shiny)
library(bslib)
library(openxlsx)
library(tidyverse)

# Pseudo weights for composite score ----
cs_weights <- data.frame(
  "var_name" = c(
    "base",
    "x_1",
    "x_2",
    "x_3",
    "x_4",
    "x_5",
    "x_6"
  ),
  "weights_planning" = c(
    0.376,
    0.123,
    0.251,
    -0.050,
    -0.015,
    0,
    0
  ),
  "weights_performance" = c(
    0.327,
    0.120,
    0.250,
    -0.050,
    -0.017,
    0.099,
    0.019
  )
)

# Define UI for application that draws a histogram ----
ui <- page_sidebar(
  ## App title ----
  title = "Composite Score Scenario Data & Distribution",
  ## Sidebar panel: Model ----
  sidebar = sidebar(
    radioButtons(
      inputId = 'cs_model',
      label = 'Select Model',
      choices = list( 
      "Planning" = "Planning", 
      "Performance" = "Performance"
      )),
    ## Input: Slider for sample proportions ----
    sliderInput(inputId = "samplesize",
                label = "Sample Size:",
                min = 0,
                max = 10000,
                value = 3000),
    sliderInput(inputId = "x_1",
                label = "Proportion x_1:",
                min = 0,
                max = 1,
                value = .29),
    sliderInput(inputId = "x_2",
                label = "Proportion of x_2:",
                min = 0,
                max = 1,
                value = .05),
    sliderInput(inputId = "x_3",
                label = "Proportion of x_3:",
                min = 0,
                max = 1,
                value = .45),
    sliderInput(inputId = "x_4",
                label = "Proportion of x_4:",
                min = 0,
                max = 1,
                value = .13),
    sliderInput(inputId = "x_5",
                label = "Proportion of x_5:",
                min = 0,
                max = 1,
                value = .22),
    sliderInput(inputId = "x_6",
                label = "Proportion of x_6:",
                min = 0,
                max = 1,
                value = .03),
    downloadButton('download',"Download Data and Scenario")
        ),
  ## Output: Histogram ----
  plotOutput(outputId = "distPlot", width = 'auto', height = 'auto'), #change width and height if experiencing margin issues
  ## Output: Table ----
  tableOutput("myTable")
    )

# Define server logic to create dataframes and draw histogram ----
server <- function(input, output) {
  # Model data based on proportion of sample with x_i = 1 ----
  modeldata <- reactive({
    dat <- data.frame(list(
      "unique_id" = seq(1,input$samplesize),
      "x_1" = rbinom(input$samplesize, size = 1, prob = input$x_1), 
      "x_2" = rbinom(input$samplesize, size = 1, prob = input$x_2), 
      "x_3" = rbinom(input$samplesize, size = 1, prob = input$x_3),
      "x_4" = rbinom(input$samplesize, size = 1, prob = input$x_4),   
      "x_5" = rbinom(input$samplesize, size = 1, prob = input$x_5),    
      "x_6" = rbinom(input$samplesize, size = 1, prob = input$x_6)
     )
)
    # Computing composite score differently based on model ----
    if (input$cs_model == "Planning"){
    dat <- dat %>% 
      pivot_longer(-c(unique_id), names_to = 'var_name', values_to = 'value') %>% 
      left_join(cs_weights %>% select(var_name, weights_planning), by = 'var_name') %>% 
      mutate(weighted_value = (value*weights_planning),
             base_cs = min(cs_weights$weights_planning[which(cs_weights$var_name == 'base')])) %>% 
      filter(var_name != 'base') %>% 
      group_by(unique_id) %>% 
      summarise(cs = sum(weighted_value)+min(base_cs)) %>% 
      left_join(dat, by = 'unique_id')
      
    } else if(input$cs_model == "Performance"){
    dat <- dat %>% 
      pivot_longer(-c(unique_id), names_to = 'var_name', values_to = 'value') %>% 
      left_join(cs_weights %>% select(var_name, weights_performance), by = 'var_name') %>% 
      mutate(weighted_value = (value*weights_performance),
             base_cs = min(cs_weights$weights_performance[which(cs_weights$var_name == 'base')])) %>% 
      filter(var_name != 'base') %>% 
      group_by(unique_id) %>% 
      summarise(cs = sum(weighted_value)+min(base_cs))%>% 
      left_join(dat, by = 'unique_id')
    }
  })

  # Input for summary table and axis label ----
  textinput <- reactive({
  if (input$cs_model == "Planning"){
    paste(input$cs_model)
    } else if(input$cs_model == "Performance"){
    paste(input$cs_model)
    }  
  })
  # Histogram ----
  output$distPlot <- renderPlot({
    df <- modeldata()
   
    ggplot(df, aes(x = cs)) + 
      geom_histogram(binwidth = .05, fill = 'lightblue') + 
      geom_vline(xintercept = mean(df$cs), color = 'darkorange', linetype = 'dotted', linewidth = 1) + 
      labs(x = paste(textinput(),'Composite Score',sep = " "),
           y = "Count") +
      jtools::theme_nice()+
      theme(text = element_text(size = 18))
    })
  # Data frame of model parameters and average score ----
  datasetInput <- reactive({
    dat <- modeldata()
    data.frame(
      "average_score" = mean(dat$cs),
      "model" = paste(textinput(),'Composite Score',sep = " "),
      "x_1" = input$x_1, 
      "x_2" = input$x_2,  
      "x_3" = input$x_3,
      "x_4" = input$x_4,    
      "x_5" = input$x_5,
      "x_6" = input$x_6
    )
  })
  # Table output ----
  output$myTable <- renderTable({
   datasetInput()
  })
  # Download table to XLSX: 2 Sheets ----
  output$download <- downloadHandler(
    filename = function(){paste("scenario_data-", Sys.Date(), ".xlsx", sep="")}, 
    content = function(file){
      openxlsx::write.xlsx(list(
        "Model Params-Mean Score" = datasetInput(),
        "Simulated Model Data" = modeldata()), 
        file = paste(file))
    })
}

# Run the application ----
shinyApp(ui = ui, server = server)

