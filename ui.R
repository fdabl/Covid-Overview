library(shinydashboard)
library(shiny)
library(plotly)
library(shinyWidgets)
library(maps)
library(viridis)
library(lubridate)
library(tidyverse)
library(DT)




data <- read_csv("data_processed.csv")

earliest <- min(data$date_processed)
most_recent <- max(data$date_processed)

data_plot <- data %>%
  filter(date_processed == most_recent)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Lifting lockdown")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    # links to each tab
    menuItem("Welcome", tabName = "welcome", icon = icon("door-open")),
    menuItem("First graphs", tabName = "graphs", icon = icon("chart-line")),
    
    # link to our page
    menuItem("Who we are", icon = icon("users"), 
             href = "https://dataversuscorona.com/about/")
  )
)

#body of the dashboard; each tabItem corresponds to a menuItem
body <- dashboardBody(
  
  tabItems(
    
    # homepage; we need to think about something smarter to say here. 
    # Maybe also a map?
    tabItem(
      tabName = "welcome",
          box(title = paste0("Overview of Lockdown Measures"),
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          tags$br(),
          tags$p(""),
          tags$br(),
          tags$p("This dashboard shows an overview of how different lockdown measures were implemented across different countries", 
                 style = "font-size:150%", align = 'center'),
          tags$br(),
          tags$p("The source of the data is Hale, Thomas, Sam Webster, Anna Petherick, Toby Phillips, and Beatriz Kira (2020). Oxford COVID-19 Government Response Tracker, Blavatnik School of Government. Data use policy: Creative Commons Attribution CC BY standard.", 
                 style = "font-size:100%", align = 'center'),
          
          tags$br(),
          width = "100%"
          ),
          box(title = "World Overview",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            align = "center",
            plotlyOutput("heatmap", width = "75%"),
            sliderInput(inputId = "mapdate", label = "Date:", 
                        min = earliest, 
                        max = most_recent,
                        value = most_recent,
                        step = 1,
                        width = "75%"),
            radioButtons(inputId = "infoType", label = "Information displayed:", 
                         choices = c("Stringency" = "StringencyIndexForDisplay",
                                     "Deaths" = "DeathsPerMillion",
                                     "Cases" = "CasesPerMillion"),
                         selected = "StringencyIndexForDisplay",
                         inline = TRUE),
            width = "100%"
            ),

      
          box(title="Table",
              status="primary",
              solidHeader = TRUE,
              collapsible = TRUE,
              align = "center",
              multiInput(
                inputId = "t_id", label = "Countries:",
                choices = data_plot$CountryName,
                
                # it's nice to start with a default; I chose our countries + UK; we can change that
                selected = c("Germany", "Netherlands", "Romania", "Serbia", "United Kingdom"), width = "350px",
                options = list(
                  enable_search = TRUE,
                  non_selected_header = "Choose between:",
                  selected_header = "You have selected:"
                )
              ),
              
              DT::dataTableOutput("table"),
              width = "100%")
    ),
    
    # this is the first tab with graphs
    
    tabItem(
      tabName = "graphs",
      fluidRow(box(
               title = "First graphs",
               status = "primary",
               solidHeader = TRUE, 
               collapsible = TRUE,
               
               # let the user choose the countries they want to be displayed
               multiInput(
                 inputId = "id", label = "Countries:",
                 choices = data_plot$CountryName,
                 
                 # it's nice to start with a default; I chose our countries + UK; we can change that
                 selected = c("Germany", "Netherlands", "Romania", "Serbia", "United Kingdom"), width = "350px",
                 options = list(
                   enable_search = TRUE,
                   non_selected_header = "Choose between:",
                   selected_header = "You have selected:"
                 )
               ),
               plotOutput("first_plot"),
               height = 1000, width = "100%"
               
      )
            )
        )
    )
    
)
  
dashboardPage(header, sidebar, body, skin='blue') 
