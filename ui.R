library(shinydashboard)
library(shiny)
library(plotly)



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
          
          height = "100%", width = "100%")
    ),
    
    # this is the first tab with graphs
    
    tabItem(
      tabName = "graphs",
      fluidRow(box(
               title = "First graphs",
               status = "primary",
               solidHeader = TRUE, 
               collapsible = TRUE,
               plotlyOutput("first_plot"),
               height = 1000, width = "100%"
               )
               
            )
        )
    )
    
)
  
dashboardPage(header, sidebar, body, skin='blue') 
