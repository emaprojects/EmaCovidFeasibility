## app.R ##
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(purrr)
library(shiny)
library(readr)
library(shinydashboard)
library(DiagrammeR)
library(shinyWidgets)
library(gt)
library(readr)
library(DT)
library(fontawesome)

fileConn <- file("output.txt")

dataFolder <- shinySettings$dataFolder

read_names <- get_readable_names() %>% split(.$cohortDefinitionId) %>% map(~.$name)

#covariate_data <- read_rds(file.path(dataFolder,"merged_cov_counts.rds"))
#covariate_data_cont <- read_rds(file.path(dataFolder,"merged_cov_cont.rds"))
prep_cov_data <- read_rds(file.path(dataFolder,"prepared_cov_counts.rds"))

summary_table <- read_rds(file.path(dataFolder,"summary_table.rds"))
all_age_plot <- read_rds(file.path(dataFolder,"all_age_plot.rds"))
all_attrition <- read_rds(file.path(dataFolder,"all_attrition.rds"))
all_month_dist <- read_rds(file.path(dataFolder,"all_month_dist.rds"))
all_mortality_dist <- read_rds(file.path(dataFolder,"all_mortality_dist.rds"))
all_gender_dist <- read_rds(file.path(dataFolder,"all_gender_dist.rds"))
all_obs_pre <- read_rds(file.path(dataFolder,"obs_pre.rds"))
all_obs_post <- read_rds(file.path(dataFolder,"obs_post.rds"))

data_sources <- names(all_attrition)

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
                                shinydashboard::menuItem("Information", tabName = "Information", icon = shiny::icon("info")),
                                addInfo(shinydashboard::menuItem("Cohorts", tabName = "Counts", icon = shiny::icon("home")),"cohorts"),
                                addInfo(shinydashboard::menuItem("Population attrition", tabName = "Attrition", icon = shiny::icon("sitemap")),"key"),
                                addInfo(shinydashboard::menuItem("Index month", tabName = "indexmonth", icon = shiny::icon("calendar")),"months"),
                                addInfo(shinydashboard::menuItem("Demographics", tabName = "Demographics", icon = shiny::icon("user-friends")),"demographics"),
                                addInfo(shinydashboard::menuItem("Observation time", tabName = "Observation", icon = shiny::icon("clock")),"observation"),
                                addInfo(shinydashboard::menuItem("Vital status", tabName = "Vital", icon = shiny::icon("skull")),"vital"),
                                addInfo(shinydashboard::menuItem("Population covariates", tabName = "Characterization", icon = shiny::icon("bar-chart")),"covariates")
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
    conditionalPanel(condition = "input.menu=='Demographics'||input.menu=='Observation'||input.menu=='indexmonth'",
                     
                     shinyWidgets::pickerInput("demographicsCohort", "Cohort:",
                                               choices = c("COVID-19 diagnosis or positive test" = 2887,  
                                                           "Hospitalized with COVID-19" = 2885,
                                                           "Influenza diagnosis or positive test" = 2899,
                                                           "Hospitalized with Influenza" = 2935),
                                               selected = 1,
                                               multiple = FALSE)
    ),
    conditionalPanel(condition = "input.menu=='Vital'",
                     
                     shinyWidgets::pickerInput("mortalityCohort", "Cohort:",
                                               choices = c("COVID-19 diagnosis or positive test" = 2887,  
                                                           "Influenza diagnosis or positive test" = 2899),
                                               selected = 1,
                                               multiple = FALSE)
                     
                     
    ),
    conditionalPanel(condition = "input.menu=='Characterization'",
                     
                     shinyWidgets::pickerInput("charCohort", "Cohort:",
                                               choices = c("COVID-19 diagnosis or positive test" = 2887, 
                                                           "Hospitalized with COVID-19" = 2885,
                                                           "Influenza diagnosis or positive test" = 2899,
                                                           "Hospitalized with Influenza" = 2935),
                                               selected = 1,
                                               multiple = FALSE)#,
                     
                     # shinyWidgets::pickerInput("followUp", "Time period:",
                     #                           choices = c("Pre-index (day -730 to -1)" = -730, 
                     #                                       "Post-index (day 0 to 30)" = 0),
                     #                           selected = 1,
                     #                           multiple = FALSE),
                     # 
                     # shinyWidgets::pickerInput("domainInput", "Domain",
                     #                           choices = c(
                     #                             "conditions"="condition_era group",
                     #                             "drugs"="drug_era group",
                     #                             "measurements"="measurement",
                     #                             "measurements with value"="measurement value",
                     #                             "observations"="observation",
                     #                             "procedures"="procedure_occurrence"
                     #                           ),
                     #                           selected = "condition_era group",
                     #                           multiple = TRUE)
                     
                     
    )
    
    
    
    #shiny::selectInput(
    #  "selectResult",
    #  label = shiny::h4("Result:"),
    #  choices = myResultList
    #)
    
  ),
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }'), tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "my.css"))),
    shinydashboard::tabItems(
      shinydashboard::tabItem(tabName = "Counts",
                              fluidRow(
                                box(title = strong("Cohort counts"),
                                    solidHeader = TRUE,
                                    width = 10,
                                    gt_output("summary_table")))
                              
      ),
      
      
      shinydashboard::tabItem(tabName = "indexmonth",
                              
                              fluidRow(
                                box(title = strong(textOutput("month_text")),
                                    solidHeader = TRUE,
                                    width = 12,
                                    
                                    gt_output("month_dist"))
                              )
                              
                              
      ),
      shinydashboard::tabItem(tabName = "Attrition",
                              fluidRow(box(title = strong(textOutput("attrition_text")),
                                           solidHeader = TRUE,
                                           width = 6,
                                           grVizOutput('attrition_plot', width = "100%", height = "650")))
      ),
      shinydashboard::tabItem(tabName = "Demographics",
                              fluidRow(
                                box(title = strong(textOutput("age_text")),
                                    solidHeader = TRUE,
                                    width = 9,
                                    
                                    plotOutput("age_plot", height = "400px", width = "1000px"))
                              ),
                              
                              fluidRow(
                                box(title = strong(textOutput("gender_text")),
                                    solidHeader = TRUE,
                                    width = 9,
                                    
                                    gt_output("gender_dist"))
                              )
                              
      ),
      shinydashboard::tabItem(tabName = "Observation",
                              
                              fluidRow(box(title =  strong(textOutput("obs_prior_text")),
                                           solidHeader = TRUE,
                                           width = 9,
                                           
                                           gt_output("observation_prior"))),
                              
                              fluidRow(
                                box(title =  strong(textOutput("follow_up_text")),
                                    solidHeader = TRUE,
                                    width = 9,
                                    gt_output("observation_post"))
                              )
      ),
      shinydashboard::tabItem(tabName = "Vital",
                              fluidRow(box(title =  strong(textOutput("mortality_text")),
                                           solidHeader = TRUE,
                                           width = 7,
                                           
                                           gt_output("mortality")))
      ),
      shinydashboard::tabItem(tabName = "Characterization",
                              box(title = strong("Covariates"),
                                  solidHeader = TRUE,
                                  width = 12,
                                  DT::dataTableOutput("covariate_table"))
      ),
      shinydashboard::tabItem(tabName = "Information",
                              box(title =  strong(h3("Project overview")),
                                  solidHeader = F,
                                  width = 12,
                                  collapsible = T,
                                  includeMarkdown("md/about.md"))
                              # ,box(title = strong(h3("Data sources")),
                              #     solidHeader = TRUE,
                              #     width = 12,
                              #     collapsible = T,
                              #     collapsed = F,
                              #     gt_output("ds_table"))
                              
                              
      )
    )
  )
)

server <- function(input, output, session) { 
  
  #output$ds_table <- render_gt({ make_ds_desc_table()})
  
  output$attrition_plot <- renderGrViz({all_attrition[[input$attritionDs]][[input$attritionCond]]})
  
  output$attrition_text <- renderText({stringr::str_c(input$attritionCond, " population attrition (",input$attritionDs,")")})
  
  output$month_text <- renderText({str_c("Percentage of ",read_names[[as.character(input$demographicsCohort)]], " cases identified each month from the total of cases in the database")})
  
  output$month_dist <- render_gt({all_month_dist[[as.character(input$demographicsCohort)]]})
  
  output$age_text <- renderText({str_c("Distribution of ",read_names[[as.character(input$demographicsCohort)]], " cases by age")})
  
  output$gender_dist <- render_gt({all_gender_dist[[as.character(input$demographicsCohort)]]})
  
  output$mortality <- render_gt({all_mortality_dist[[as.character(input$mortalityCohort)]]})
  
  output$age_plot <- renderPlot({all_age_plot[[as.character(input$demographicsCohort)]]})
  
  output$mortality_text <- renderText({str_c("Distribution of ",read_names[[as.character(input$mortalityCohort)]], " vital status")})
  
  output$gender_text <- renderText({str_c("Distribution of ",read_names[[as.character(input$demographicsCohort)]], " cases by gender")})
  
  output$obs_prior_text <- renderText({str_c("Distribution of available lookback days for ",read_names[[as.character(input$demographicsCohort)]], " cases")})
  
  output$follow_up_text <- renderText({str_c("Distribution of available follow-up days for  ",read_names[[as.character(input$demographicsCohort)]], " cases")})
  
  #output$observation_post <- renderPlot({plot_observation_boxplot(data_sources, input$demographicsCohort, covariate_data_cont, "after index")}, res = 100)
  
  #output$observation_prior <- renderPlot({plot_observation_boxplot(data_sources, input$demographicsCohort, covariate_data_cont, "prior to index")}, res = 10
  
  
  # output$mortalityCohortUi <- renderUI({
  #   
  #   browser()
  #   
  #   inv <- case_when(input$demographicsCohort %in% c(2887, 2885) ~ 1,
  #                    TRUE ~ 2)
  #   
  # 
  # })
  
  
  observeEvent(input$attritionCond, {
    
    pick <- case_when(input$attritionCond %in% "COVID-19"~ 2887,
                      input$attritionCond %in% "Influenza" ~ 2899)  
    
    
    updatePickerInput(session, "mortalityCohort",
                      selected = pick)
    
    pick <- case_when(input$attritionCond %in% "COVID-19"~ 
                        case_when(input$demographicsCohort==2885 ~ 2885,
                                  TRUE ~ 2887),
                      input$attritionCond %in% "Influenza" ~ 
                        case_when(input$demographicsCohort==2935 ~ 2935,
                                  TRUE ~ 2899)) 
    
    updatePickerInput(session, "demographicsCohort",
                      selected = pick)
    
    updatePickerInput(session, "charCohort",
                      selected = pick)
    
  })
  
  
  observeEvent(input$mortalityCohort, {
    
    pick <- case_when(input$mortalityCohort %in% c(2887) ~ "COVID-19",
                      input$mortalityCohort %in% c(2899) ~ "Influenza") 
    
    
    updatePickerInput(session, "attritionCond",
                      selected = pick)
    
    pick <- case_when(input$mortalityCohort == 2885 ~ 
                        case_when(input$demographicsCohort==2885 ~ 2885,
                                  TRUE ~ 2887),
                      input$mortalityCohort == 2899 ~ 
                        case_when(input$demographicsCohort==2935 ~ 2935,
                                  TRUE ~ 2899)) 
    
    updatePickerInput(session, "demographicsCohort",
                      selected = pick)
    
    updatePickerInput(session, "charCohort",
                      selected = pick)
    
  })
  
  
  observeEvent(input$demographicsCohort, {
    
    updatePickerInput(session, "charCohort",
                      selected = input$demographicsCohort)
    
    pick <- case_when(input$demographicsCohort %in% c(2887, 2885) ~ 2887,
                      input$demographicsCohort %in% c(2899, 2935) ~ 2899)  
    
    updatePickerInput(session, "mortalityCohort",
                      selected = pick)
    
    pick <- case_when(input$demographicsCohort %in% c(2887, 2885) ~ "COVID-19",
                      input$demographicsCohort %in% c(2899, 2935) ~ "Influenza") 
    
    
    updatePickerInput(session, "attritionCond",
                      selected = pick)
    
  })
  
  observeEvent(input$charCohort, {
    
    updatePickerInput(session, "demographicsCohort",
                      selected = input$charCohort)
    
    pick <- case_when(input$charCohort %in% c(2887, 2885) ~ 2887,
                      input$charCohort %in% c(2899, 2935) ~ 2899)  
    
    updatePickerInput(session, "mortalityCohort",
                      selected = pick)
    
    pick <- case_when(input$charCohort %in% c(2887, 2885) ~ "COVID-19",
                      input$charCohort %in% c(2899, 2935) ~ "Influenza") 
    
    
    updatePickerInput(session, "attritionCond",
                      selected = pick)
    
  })
  
  
  output$observation_post <- render_gt({all_obs_post[[as.character(input$demographicsCohort)]]})
  
  output$observation_prior <-  render_gt({all_obs_pre[[as.character(input$demographicsCohort)]]})
  
  output$summary_table <- render_gt({summary_table})
  
  output$covariate_table <- DT::renderDataTable({prep_cov_data %>% filter(cohortDefinitionId == input$charCohort) %>% select(-cohortDefinitionId)},
                                                rownames= FALSE, filter = list(
                                                  position = 'top', clear = FALSE,
                                                  options = list(autoWidth = TRUE)))  
  
  # Functionality for help messages
  showInfoBox <- function(title, htmlFileName) {
    showModal(modalDialog(
      title = title,
      easyClose = TRUE,
      footer = NULL,
      size = "l",
      renderUI(HTML(readLines(htmlFileName)))
    ))
  }
  
  observeEvent(input$key, {
    showInfoBox("Population Attrition", "md/key.html")
  })
  
  observeEvent(input$cohorts, {
    showInfoBox("Cohorts", "md/cohorts.html")
  })
  
  observeEvent(input$months, {
    showInfoBox("Index Month", "md/months.html")
  })
  
  observeEvent(input$demographics, {
    showInfoBox("Demographics", "md/demographics.html")
  })
  
  observeEvent(input$observation, {
    showInfoBox("Observation time", "md/obs_time.html")
  })
  
  observeEvent(input$covariates, {
    showInfoBox("Population covariates", "md/covariates.html")
  })
  
  observeEvent(input$vital, {
    showInfoBox("Vital status", "md/vital.html")
  })
  
  
  
  
}

shinyApp(ui, server)