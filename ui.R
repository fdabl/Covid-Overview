library('DT')
library('shinyjs')
library('shinyWidgets')
library('shinydashboard')
source('helpers.R')


country_codes <- get_country_codes()
dat <- get_stringency_data(verbose = FALSE) %>% 
  left_join(
    country_codes, by = 'country_code'
  )

eu_countries <- c(
  'Austria','Belgium','Bulgaria','Croatia','Cyprus',
  'Czech Republic','Denmark','Estonia','Finland','France',
  'Germany','Greece','Hungary','Ireland','Italy','Latvia',
  'Lithuania','Luxembourg','Malta','Netherlands','Poland',
  'Portugal','Romania','Slovakia','Slovenia','Spain',
  'Sweden','United Kingdom'
)

CUSTOM <- c(
  'Germany', 'Netherlands', 'Iran', 'Brazil',
  'United Kingdom', 'United States', 'Sweden', 'South Korea'
)

COUNTRIES <- dat$country_name %>% unique() 
MIN_DATE <- min(dat$date)
MAX_DATE <- max(dat$date)
CUSTOM_SELECTION <- c(
  'Germany', 'Netherlands', 'Iran', 'Brazil',
  'United Kingdom', 'United States', 'Sweden', 'South Korea'
)


sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(
    menuItem('Overview', tabName = 'welcome', icon = icon('door-open')),
    menuItem('About', tabName = 'about', icon = icon('address-card'))
  )
)


body <- dashboardBody(
  useShinyjs(),
  
  tabItems(
    tabItem(
      tags$style(type = "text/css", "p{font-size: 150%;}"),
      tabName = 'welcome',
      box(
        width = NULL,
        tags$h1(
          'Visualising the COVID-19 Pandemic', align = 'center'
        ),
        tags$br(),
        HTML(
          "
          <p>
          The novel coronavirus has a firm grip on nearly all countries across the world,
          and there is large heterogeneity in how countries have responded to the threat.
          In three parts, this dashboard provides an overview of confirmed cases, deaths, and measures that
          countries have taken to curb the spread of the virus.
          </p>
          
          <h3>The World At Once</h3>
          <p>
          An interactive world map allows you to compare confirmed cases, deaths, and how
          countries have responded using Stringency Measures — such as school or
          workplace closures — collected by the
          <a href='https://www.bsg.ox.ac.uk/research/research-projects/coronavirus-government-response-tracker'
          target='_blank'>Oxford COVID-19 Government Response Tracker</a>. The Oxford Stringency Index combines these measures (and one
          measure about public information campaigns) into a composite score ranging from 0 to 100
          (for details, see Hale et al., <a href='https://www.bsg.ox.ac.uk/research/publications/variation-government-responses-covid-19'
          target='_blank'>2020a</a>), and you can explore how this index has changed across countries and time as well. Note that we show
          the total confirmed cases and deaths per million inhabitants. Therefore, this number is never decreasing, and population size
          needs to be taken into account when wishing to compare total outbreak sizes. For more comprehensive visualisations of confirmed
          cases and deaths, see
          <a href='https://ourworldindata.org/covid-cases' target='_blank'>here</a> and 
          <a href='https://ourworldindata.org/covid-deaths' target='_blank'>here</a>.
          </p>
          
          <h3>Comparing Individual Countries</h3>
          <p>
          Below the world map you find an interactive Figure which allows you to single out
          individual countries and visualize the Oxford Stringency Index together with confirmed cases
          per million and deaths per million. This normalization makes for more pleasant visualisations. Note, however,
          that these measures need to be interpreted relative to a country's population size. We display the total cases and
          total deaths as well as new cases and new deaths in the top left corner. Note that new cases and new deaths are those
          reported three days ago, as reporting for today may lag behind. All data here and in the world map refer to 7-day
          rolling averages, and are provided by the
          <a href='https://covid19datahub.io/' target='_blank'>covid19 R package</a>.
          </p>
          
          <h3>Who's Ready To Rollback?</h3>
          <p>
          The last part of the app is an interactive Table which shows in more detail which
          countries have imposed or lifted what Stringency Measures, and when. Individual rows are coloured
          according to how close each country is to the WHO recommendations for rolling back lockdowns, using the approach
          outlined in Hale et al. (<a href='https://www.bsg.ox.ac.uk/research/publications/lockdown-rollback-checklist'
          target='_blank'>2020b</a>)
          </p>
          
          <h3>Caveats</h3>
          <p>
          Note that international comparisons are difficult due to differences in testing and reporting across countries
          but also across time; beware of overinterpreting such comparisons. For a discussion of these difficulties,
          a more detailed explanation of this dashboard, and links to further resources,
          see <a href='https://scienceversuscorona.com/visualising-the-covid-19-pandemic' target='_blank'>this blog post</a>.
          </p>
          "
        )
      ),
      
      box(
        title = 'The World At Once', status = 'primary', width = '100%',
        solidHeader = TRUE, collapsible = TRUE, align = 'center',
        tags$head(
          tags$style(
            type = 'text/css',
            '.slider-animate-button {font-size: 20pt !important; position: absolute; left: 49.3%; margin-top: 20px}'
          )
        ),
        
        plotlyOutput('heatmap', inline = TRUE),
        tags$br(),
        
        sliderInput(
          inputId = 'mapdate', label = 'Date', 
          min = MIN_DATE, 
          max = MAX_DATE,
          value = MAX_DATE,
          ticks = FALSE,
          timeFormat = '%B %d',
          step = 1, width = '100%',
          animate = TRUE, animateOptions(interval = 30, loop = TRUE)
        ),
        
        tags$br(),
        tags$br(),
        tags$br(),
        
        div(
          style='display:inline-block; width:25%; margin-right:2%',
          selectInput(
            inputId = 'variable_type',
            label = 'Information displayed', 
            choices = c(
              'Stringency' = 'StringencyIndex',
              'Deaths' = 'Deaths',
              'Cases' = 'Cases'
              # 'Tests' = 'Tests'
            ), selected = 'StringencyIndex'
          )
        ),
        
        conditionalPanel(condition = 'input.variable_type == \'StringencyIndex\'',
                         selectInput(inputId = 'index_type', label = 'Type of measures',
                                     choices = c('Combined' = 'Combined',
                                                 'School closing' = 'School',
                                                 'Workplace closing' = 'Workplace',
                                                 'Cancellation of public events' = 'PublicEvents',
                                                 'Restrictions on gatherings' = 'Gatherings',
                                                 'Closing of public transport' = 'Transport',
                                                 'Stay at home requirements' = 'Home',
                                                 'Restrictions on internal movement' = 'Movement',
                                                 'International travel controls' = 'Travel'),
                                     selected = 'Combined'),
                         style = 'display:inline-block; width:25%'
        ),
        
        div(
          style = 'display:inline-block; width:25%; margin-left:2%',
          selectInput(
            inputId = 'region',
            label = 'Region', 
            choices = c(
              'World' = 'World',
              'Europe' = 'Europe',
              'North America' = 'North America',
              'South America' = 'South America',
              'Asia' = 'Asia',
              'Africa' = 'Africa',
              'OECD' = 'OECD',
              'USA (States)' = 'USA'
            ), 
            selected = 'World'
          )
        )
      ),
      
      
      box(
        title = 'Comparing Individual Countries', status = 'primary', solidHeader = TRUE,
        collapsible = TRUE, align = 'center', width = '100%', #height = '1000px',
        div(
          style = 'height:1000px; overflow-y: scroll',
          multiInput(
            inputId = 'countries_lockdown',
            label = 'Countries',
            choices = COUNTRIES,
            selected = CUSTOM_SELECTION,
            width = '350px',
            options = list(
              enable_search = TRUE,
              non_selected_header = 'Choose between:',
              selected_header = 'You have selected:'
            )
            
          ),
          
          #div(style = 'display:inline-block; vertical-align:top',
          selectInput(
            'regions', 'Region', 
            c('Custom', 'World', 'Europe',
              'North America', 'South America', 'Asia', 'Africa',
              'Oceania', 'OECD'),
            width = '300px'
          ),
          
          selectInput(
            'graph',
            'Indicator',
            choices = c(
              'New Cases per Million',
              'New Deaths per Million'
            ),
            
            # choices = c(
            #   'New Deaths per Million',
            #   'New Cases per Million',
            #   'New Tests per Confirmed Case'
            # ),
            selected = 'New Cases per Million',
            width = '300px'
          ),
          
          actionButton('refresh', 'Apply'),
          actionButton('all_graph', 'Select all'),
          actionButton('clear_graph', 'Clear selection'),
          tags$br(),
          tags$br(),
          plotOutput('lockdown_plot_lines_scales')
        )
      ),
      
      box(
        title = 'Who\'s Ready To Rollback? ', status = 'primary', solidHeader = TRUE,
        collapsible = TRUE, align = 'center', width = '100%',
        multiInput(
          inputId = 'countries_table',
          label = 'Countries',
          choices = unique(dat$country_name),
          selected = CUSTOM,
          width = '350px',
          options = list(
            enable_search = TRUE,
            non_selected_header = 'Choose between:',
            selected_header = 'You have selected:'
          )
        ),
        selectInput('continent_table',
                    'Region',
                    c(
                      'Custom' = 'Custom',
                      'World' = 'World',
                      'Europe' = 'Europe',
                      'North America' = 'North America',
                      'South America' = 'South America',
                      'Asia' = 'Asia',
                      'Africa' = 'Africa',
                      'Oceania' = 'Oceania'
                    ), selected = 'Custom', width = '175px',
        ) ,
        
        div(style = 'display:inline-block', actionButton('TableApply','Apply')),
        actionButton('TableAll', 'Select All'),
        actionButton('TableClear', 'Clear Selection'),
        plotOutput('table_legend', inline = FALSE, height = '220px', width = '500px'),
        dataTableOutput('countries_table')
      )
    ),
    
    tabItem(
      tabName = 'about',
      fluidPage(
        box(
          width = 1000,
          HTML(
            "
            <h2 style = 'text-align: center;'>About</h2>
            
            <p style = 'text-align: center;'>
            This Web App was developed by
            <a href='https://twitter.com/fdabl' target='_blank'>Fabian Dablander</a>,
            <a href='https://nl.linkedin.com/in/ialmi' target='_blank'>Alexandra Rusu</a>,
            <a href='http://smip.uni-mannheim.de/PhD%20Candidates/Cohort%202019/Schreiner,%20Marcel/' target='_blank'>
            Marcel Schreiner</a>,
            and <a href='https://www.atomasevic.com/' target='_blank'>Aleksandar Tomasevic</a>
            as a <a href='http://scienceversuscorona.com/' target='_blank'>Science versus Corona</a> project.
            </p>
            
            <p style = 'text-align: center;'>
            The code is available on <a href='https://github.com/fdabl/Covid-Overview' target='_blank'>Github</a>, and we invite anybody who is interested to contribute!
            </p>
            "
          )
        )
      )
    )
  )
)

header <- dashboardHeader(title = 'COVID-19 Overview')  
dashboardPage(header, sidebar, body, skin = 'blue') 
