library('DT')
library('shinyWidgets')
library('shinydashboard')
library('dashboardthemes')
source('helpers.R')


# world <- get_world_data()
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
# alx: solved by using unique()
#COUNTRIES <- eu_countries # alx: not necessary anymore
COUNTRIES <- dat$Country %>% unique() 
MIN_DATE <- min(dat$Date)
MAX_DATE <- max(dat$Date)


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Overview', tabName = 'welcome', icon = icon('door-open')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)


body <- dashboardBody(
  shinyDashboardThemes(
    theme = 'grey_light'
  ),
  
  tabItems(
    tabItem(
      tabName = 'welcome',
      box(
        title = ('Overview of Lockdown Measures'), status = 'primary',
        solidHeader = TRUE, collapsible = TRUE,
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
        title = "The Lockdown Across the Globe", status = "primary", width = "100%",
        solidHeader = TRUE, collapsible = TRUE, align = "center",
        
        plotlyOutput("heatmap", inline = TRUE),
        
        
        tags$head(tags$style(type="text/css", 
                             ".slider-animate-button {font-size: 20pt !important; position: absolute; left: 49.3%; margin-top: 20px}")),
        
        tags$br(),
        
        sliderInput(
          inputId = "mapdate", label = "Date:", 
          min = MIN_DATE, 
          max = MAX_DATE,
          value = MAX_DATE,
          ticks = FALSE,
          timeFormat = "%B %d",
          step = 1, width = "100%",
          animate = TRUE, animateOptions(interval = 30, loop = TRUE)
        ),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        div(style="display:inline-block; width:25%; margin-right:2%",
          selectInput(
            inputId = "variable_type",
            label = "Information displayed:", 
            choices = c(
              "Stringency" = "StringencyIndex",
              "Deaths" = "Deaths",
              "Cases" = "Cases"
            ), selected = "StringencyIndex"
          )
        ),
        
        conditionalPanel(condition = "input.variable_type == 'StringencyIndex'",
                         selectInput(inputId = "index_type", label = "Type of measures:",
                                     choices = c("Combined" = "Combined",
                                                 "School closing" = "School",
                                                 "Workplace closing" = "Workplace",
                                                 "Cancellation of public events" = "PublicEvents",
                                                 "Restrictions on gatherings" = "Gatherings",
                                                 "Closing of public transport" = "Transport",
                                                 "Stay at home requirements" = "Home",
                                                 "Restrictions on internal movement" = "Movement",
                                                 "International travel controls" = "Travel"),
                                     selected = "Combined"),
                         style = "display:inline-block; width:25%"
        ),
        
        div(style="display:inline-block; width:25%; margin-left:2%",
          selectInput(
            inputId = "region",
            label = "Region:", 
            choices = c(
              "World" = "World",
              "Europe" = "Europe",
              "North America" = "NorthAmerica",
              "South America" = "SouthAmerica",
              "Asia" = "Asia",
              "Africa" = "Africa",
              "OECD" = "OECD",
              "USA (States)" = "USA"
            ), 
            selected = "World"
          )
        )
      ),
      
      
      box(title = 'Stringency Index and Daily Deaths', status = 'primary', solidHeader = TRUE,
          collapsible = TRUE, align = 'center', width = '100%', #height = "1000px",

        div(style = 'height:1000px; overflow-y: scroll',
           
            div(style = 'display:inline-block; vertical-align:top',
                multiInput(
                  inputId = 'countries_lockdown',
                  label = 'Select specific countries:',
                  choices = COUNTRIES,
                  selected = c('Germany', 'Netherlands', 'Romania', 'Serbia', 'United Kingdom'),
                  width = '300px',
                  options = list(
                    enable_search = TRUE,
                    non_selected_header = 'Choose between:',
                    selected_header = 'You have selected:'
                  )
                )
            ),
            
            div(style = 'display:inline-block; vertical-align:top',
                selectInput(
                  'regions', 'or select a group of countries:', 
                  c('Africa', 'Asia', 'Europe', 'Oceania', 'North America', 'South America', 'OECD'),
                  width = '250px'
                ),
                
                
                selectInput(
                  'graph', 'Select the indicator:', 
                  c('Daily deaths per 10 Million', 'Daily deaths (absolute value)',
                    'New Cases per Million', 'New Cases (absolute value)'),
                  width = '250px'
                ),
                
                # selectInput(
                #   'grouping', '', 
                #   c('show selected countries', 'show selected group'),
                #   width = '250px'
                # ),
                
                prettySwitch(
                  'grouping',
                  'Display by region',
                  value = FALSE,
                  status = 'default',
                  slim = FALSE,
                  fill = FALSE,
                  bigger = FALSE,
                  inline = FALSE,
                  width = NULL
                ),
                
                actionButton('Refresh', 'Refresh graph')
            ),
            plotOutput('lockdown_plot_lines_scales')
            
        )

        #height = '1000'  #textOutput("height")
      ),
      
      box(
        title = 'How Countries are Lifting the Lockdown', status = 'primary', solidHeader = TRUE,
        collapsible = TRUE, align = 'center', width = '100%',
        multiInput(
          inputId = 'countries_table',
          label = 'Countries:',
          choices = unique(dat$CountryName),
          selected = unique(dat$CountryName),
          width = '350px',
          options = list(
            enable_search = TRUE,
            non_selected_header = 'Choose between:',
            selected_header = 'You have selected:'
          )
        ),
        radioButtons(
          inputId = 'continent_table',
          label = 'Zoom in on:', 
          choices = c(
            'World' = 'World',
            "Europe" = "Europe",
            'North & South America' = 'Americas',
            'Asia' = 'Asia',
            'Africa' = 'Africa',
            'Oceania' = 'Oceania'
          ), selected = 'World', inline = TRUE
        ),
        div(style='display:inline-block', actionButton('TableApply','Apply')),
        actionButton('TableAll', 'Select All'),
        actionButton('TableClear', 'Clear Selection'),
        dataTableOutput('countries_table')
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(width = 1000,
            HTML(
              "<h3 style = 'text-align: center;'>About</h3>
              <p style = 'font-size: 120%; text-align: center;'>
              This Web App was developed by Fabian Dablander, Alexandra Rusu, Marcel Raphael Schreiner,
              and Aleksandar Tomasevic as a <a href='http://scienceversuscorona.com/' target='_blank'>Science versus Corona</a> project
              <p>"
            )
        )
      )
    )
  )
)


header <- dashboardHeader(title = 'Lifting Lockdowns')  
dashboardPage(header, sidebar, body, skin = 'blue') 
