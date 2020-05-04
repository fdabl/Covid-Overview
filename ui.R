library('shinydashboard')
library('shinyWidgets')
library('DT')
source('helpers.R')


world <- get_world_data()
dat <- get_stringency_csv()

eu_countries <- c(
  'Austria','Belgium','Bulgaria','Croatia','Cyprus',
  'Czech Republic','Denmark','Estonia','Finland','France',
  'Germany','Greece','Hungary','Ireland','Italy','Latvia',
  'Lithuania','Luxembourg','Malta','Netherlands','Poland',
  'Portugal','Romania','Slovakia','Slovenia','Spain',
  'Sweden','United Kingdom'
)

# TODO:
# Showing all countries slows the initial rendering of the app
# down because it needs to generate all these html elements
COUNTRIES <- eu_countries
# COUNTRIES <- dat$Country # extremely slow
MIN_DATE <- min(dat$Date)
MAX_DATE <- max(dat$Date)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Overview', tabName = 'welcome', icon = icon('door-open')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = 'welcome',
      box(
        title = ('Overview of Lockdown Measures'), status = 'primary',
        solidHeader = TRUE, collapsible = TRUE,
        tags$br(),
        tags$p(''),
        tags$br(),
        tags$p(
          'This dashboard shows an overview of how different lockdown measures were
          implemented across different countries', style = 'font-size:150%', align = 'center'
        ),
        tags$br(),
        tags$p(
          'The source of the data is Hale, Thomas, Sam Webster, Anna Petherick, Toby Phillips,
          and Beatriz Kira (2020). Oxford COVID-19 Government Response Tracker, Blavatnik School of Government.
          Data use policy: Creative Commons Attribution CC BY standard.',
          style = 'font-size:100%', align = 'center'
        ),
        tags$br(),
        width = '100%'
      ),
      
      box(
        title = 'The Lockdown Across the Globe', status = 'primary', width = '100%',
        solidHeader = TRUE, collapsible = TRUE, align = 'center',
        
        # plotlyOutput('heatmap', width = '75%'),
        plotOutput('heatmap', width = '75%'),
        
        sliderInput(
          inputId = 'mapdate', label = 'Date:', 
          min = MIN_DATE, 
          max = MAX_DATE,
          value = MAX_DATE,
          step = 1, width = '75%'
        ),
        
        radioButtons(
          inputId = 'variable_type',
          label = 'Information displayed:', 
          choices = c(
            'Stringency' = 'StringencyIndex',
            'Deaths' = 'Deaths',
            'Cases' = 'Cases'
          ), selected = 'StringencyIndex', inline = TRUE
        )
      ),
      
      box(
        title = 'Stringency Index and Daily Deaths', status = 'primary', solidHeader = TRUE,
        collapsible = TRUE, align = 'center', width = '100%',
        
       plotOutput('lockdown_plot'),
        
        multiInput(
          inputId = 'countries_lockdown',
          label = 'Countries:',
          choices = COUNTRIES,
          selected = c('Germany', 'Netherlands', 'Romania', 'Serbia', 'United Kingdom'),
          width = '350px',
          options = list(
            enable_search = TRUE,
            non_selected_header = 'Choose between:',
            selected_header = 'You have selected:'
           )
         )
      ),
      
      box(
        title = 'How Countries are Lifting the Lockdown', status = 'primary', solidHeader = TRUE,
        collapsible = TRUE, align = 'center', width = '100%',
        
        dataTableOutput('countries_table'),
        
        multiInput(
          inputId = 'countries_table',
          label = 'Countries:',
          choices = COUNTRIES,
          selected = c('Germany', 'Netherlands', 'Romania', 'Serbia', 'United Kingdom'),
          width = '350px',
          options = list(
            enable_search = TRUE,
            non_selected_header = 'Choose between:',
            selected_header = 'You have selected:'
            )
          )
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(width = 1000,
          h3('About'),
          p(
           'This Web App was developed by Fabian Dablander, Alexandra Rusu, Aleksandar Tomasevic,
           and Marcel Raphael Schreiner on behalf of https://scienceversuscorona.com.' 
          )
        )
      )
    )
  )
)
  

header <- dashboardHeader(title = 'Lifting Lockdowns')  
dashboardPage(header, sidebar, body, skin = 'blue') 
