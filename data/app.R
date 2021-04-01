## app.R ##
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(purrr)
library(shiny)
library(shinydashboard)
library(DiagrammeR)
library(shinyWidgets)
library(gt)
library(readr)
library(DT)

source("functions2.R")


data_sources <- names(colours)
covariate_data <- read_rds("data/merged_cov_counts.rds")
covariate_data_cont <- read_rds("data/merged_cov_cont.rds")


addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button", 
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

ui <- dashboardPage(
  dashboardHeader(),
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id ='menu',
                                shinydashboard::menuItem("Cohort counts", tabName = "Counts", icon = shiny::icon("home")),
                                shinydashboard::menuItem("Population attrition", tabName = "Attrition", icon = shiny::icon("table")),
                                shinydashboard::menuItem("Demographics", tabName = "Demographics", icon = shiny::icon("bar-chart")),
                                shinydashboard::menuItem("Characterization", tabName = "Characterization", icon = shiny::icon("calendar")),
                                shinydashboard::menuItem("Information", tabName = "Information", icon = shiny::icon("info"))
    ),
    conditionalPanel(condition = "input.menu=='Attrition'",

                     shinyWidgets::pickerInput("attritionDs", "Data source:",
                                               choices = data_sources,
                                               selected = data_sources[[1]],
                                               multiple = FALSE),
                     
                     shinyWidgets::pickerInput("attritionCond", "Condition:",
                                               choices = c("COVID-19", "Influenza"),
                                               selected = "COVID-19",
                                               multiple = FALSE)),
    conditionalPanel(condition = "input.menu=='Demographics'",
                     
                     shinyWidgets::pickerInput("demographicsCohort", "Cohort:",
                                               choices = c("COVID-19 diagnosis or positive test" = 2887,  
                                                           "Hospitalized with COVID-19" = 2885,
                                                           "Influenza diagnosis or positive test" = 2899,
                                                           "Hospitalized with Influenza" = 2935),
                                               selected = 1,
                                               multiple = FALSE)
                     ),
    conditionalPanel(condition = "input.menu=='Characterization'",
                     
                     shinyWidgets::pickerInput("demographicsCohort", "Cohort:",
                                               choices = c("COVID-19 diagnosis or positive test" = 2887, 
                                                           "Hospitalized with COVID-19" = 2885,
                                                           "Influenza diagnosis or positive test" = 2899,
                                                           "Hospitalized with Influenza" = 2935),
                                               selected = 1,
                                               multiple = FALSE),
                     
                     shinyWidgets::pickerInput("followUp", "Time period:",
                                               choices = c("Pre-index (day -730 to -1)" = -730, 
                                                           "Post-index (day 0 to 30)" = 0),
                                               selected = 1,
                                               multiple = FALSE),
                     
                     shinyWidgets::pickerInput("domainInput", "Domain",
                                               choices = c(
                                                 "conditions"="condition_era group",
                                                 "drugs"="drug_era group",
                                                 "measurements"="measurement",
                                                 "measurements with value"="measurement value",
                                                 "observations"="observation",
                                                 "procedures"="procedure_occurrence"
                                               ),
                                               selected = "condition_era group",
                                               multiple = TRUE)
                     
                     
    )
    
    
                     
                     #shiny::selectInput(
                     #  "selectResult",
                     #  label = shiny::h4("Result:"),
                     #  choices = myResultList
                     #)

    ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'))),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "Counts",
                            box(title = "Cohort definitions and counts",
                                solidHeader = TRUE,
                                width = 12,
                                gt_output("summary_table"))
                            
                            ),
      shinydashboard::tabItem(tabName = "Attrition",
                              box(title = textOutput("attrition_text"),
                                  solidHeader = TRUE,
                                  width = 12,
                                grVizOutput('attrition_plot', width = "100%", height = "650"))
      ),
      shinydashboard::tabItem(tabName = "Demographics",
                              fluidRow(
                                box(title = textOutput("month_text"),
                                    solidHeader = TRUE,
                                    width = 12,
                                    
                                    gt_output("month_dist"))
                              ),
                              
                              
                              fluidRow(
                              box(title = textOutput("age_text"),
                                  solidHeader = TRUE,
                                  width = 12,
                                
                                  plotOutput("age_plot", height = "600px", width = "900px"))
                              ),
                              
                              fluidRow(
                                box(title = textOutput("gender_text"),
                                    solidHeader = TRUE,
                                    width = 12,
                                    
                                    gt_output("gender_dist"))
                              ),
                              
                              fluidRow(
                                box(title =  textOutput("obs_prior_text"),
                                    solidHeader = TRUE,
                                    width = 12,
                                    
                                    gt_output("observation_prior"))
                              ),
                              
                              fluidRow(
                                box(title =  textOutput("follow_up_text"),
                                    solidHeader = TRUE,
                                    width = 12,
                                    gt_output("observation_post"))
                              )

      ),
      shinydashboard::tabItem(tabName = "Characterization",
                              box(title ="Covariates",
                                  solidHeader = TRUE,
                                  width = 12,
                                  DT::dataTableOutput("covariate_table"))
      ),
      shinydashboard::tabItem(tabName = "Information",
                              box(title ="Information",
                                  solidHeader = TRUE,
                                  width = 12,
                                  h4("information about project, version, github repo etc")))
  )
)
)

server <- function(input, output) { 
  
  output$attrition_plot <- renderGrViz({draw_attrition(ds = input$attritionDs, condition = input$attritionCond)})
  
  map(setNames(data_sources, data_sources), function(x){
      map(setNames(c("COVID-19", "Influenza"), c("COVID-19", "Influenza")), 
          ~draw_attrition(x, .x)
          
  }))
  
  
  
  output$attrition_text <- renderText({stringr::str_c("Population attrition: ", input$attritionCond, " (",input$attritionDs,")")})

  output$month_text <- renderText({str_c("Index month: ",get_readable_names() %>% filter(cohortDefinitionId == input$demographicsCohort) %>% .$name)})

  output$month_dist <- render_gt({draw_table_dist(data_sources, input$demographicsCohort, covariate_data = covariate_data, cov_string = "index year and month: ", show_total = T)})
    
  output$age_plot <- renderPlot({plot_age_dist(data_sources, input$demographicsCohort, covariate_data = covariate_data)}, res = 100)
  
  output$age_text <- renderText({str_c("Age distribution: ",get_readable_names() %>% filter(cohortDefinitionId == input$demographicsCohort) %>% .$name)})
  
  output$gender_dist <- render_gt({draw_table_dist(data_sources, input$demographicsCohort, covariate_data = covariate_data, cov_string = "gender = ", show_total = F)})
  
  output$gender_text <- renderText({str_c("Gender distribution: ",get_readable_names() %>% filter(cohortDefinitionId == input$demographicsCohort) %>% .$name)})
  
  output$covariate_table <- DT::renderDataTable({get_covariate_data(data_sources, covariate_data = covariate_data, cohort_id = input$demographicsCohort, follow_up = input$followUp, domain_input = input$domainInput)},
                                                rownames= FALSE)  

  output$obs_prior_text <- renderText({str_c("Baseline days: ",get_readable_names() %>% filter(cohortDefinitionId == input$demographicsCohort) %>% .$name)})
  
  output$follow_up_text <- renderText({str_c("Follow-up days: ",get_readable_names() %>% filter(cohortDefinitionId == input$demographicsCohort) %>% .$name)})
  
  #output$observation_post <- renderPlot({plot_observation_boxplot(data_sources, input$demographicsCohort, covariate_data_cont, "after index")}, res = 100)
  
  #output$observation_prior <- renderPlot({plot_observation_boxplot(data_sources, input$demographicsCohort, covariate_data_cont, "prior to index")}, res = 100)
  
  output$observation_post <- render_gt({draw_observation_table(data_sources, input$demographicsCohort, covariate_data_cont = covariate_data_cont,"after index")})
  
  output$observation_prior <- render_gt({draw_observation_table(data_sources, input$demographicsCohort, covariate_data_cont = covariate_data_cont,"prior to index")})
  
  output$summary_table <- render_gt({summary_table()})
  
    
}

shinyApp(ui, server)