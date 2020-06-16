library('DT')
source('helpers.R')


# dats <- get_stringency_csv()
country_codes <- get_country_codes()

dat <- get_stringency_data(verbose = FALSE) %>% 
  left_join(
    country_codes, by = 'country_code'
  )

get_countries <- function(dat, continent) {
  unique(dat[dat$continent == continent, ]$country_name)
}

AFRICA <- get_countries(dat, 'Africa')
ASIA <- get_countries(dat, 'Asia')
EUROPE <- get_countries(dat, 'Europe')
OCEANIA <- get_countries(dat, 'Oceania')



OECD <- c(
  'Australia', 'Austria', 'Belgium', 'Canada', 'Chile', 'Colombia',
  'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 
  'Germany','Greece', 'Hungary', 'Iceland', 'Ireland', 'Israel',
  'Italy', 'Japan', 'South Korea', # 'Latvia', -> not in the database
  'Lithuania', 'Luxembourg', 'Mexico', 'Netherlands', 'New Zealand', 'Norway',
  'Poland', 'Portugal', 'Slovak Republic', 'Slovenia','Spain',  'Sweden',
  'Switzerland', 'Turkey', 'United Kingdom', 'United States'
)

NA_COUNTRIES <- c(
  'United States', 'Mexico', 'Canada', 'Guatemala',
  'Cuba', 'Haiti', 'Dominican Republic', 'Honduras',
  'El Salvador', 'Nicaragua', 'Costa Rica', 'Panama',
  'Puerto Rico', 'Jamaica', 'Trinidad & Tobago', 'Guadeloupe',
  'Martinique', 'Bahamas', 'Belize', 'Barbados', 'St. Lucia',
  'St. Vincent & Grenadines', 'U.S. Virgin Islands',
  'Antigua & Barbuda', 'Dominica', 'Bermuda', 'Greenland',
  'St. Kitts & Nevis','Turks & Caicos Islands','Saint Martin (French part)',
  'British Virgin Islands', 'Caribbean Netherlands',
  'Anguilla', 'St. Barthélemy', 'St. Pierre & Miquelon',
  'Montserrat'
)

NORTH_AMERICA <- c(
  'Barbados', 'Belize', 'Canada', 'Costa Rica', 'Cuba',
  'Dominica', 'Dominican Republic', 'El Salvador', 
  'Greenland', 'Guatemala', 'Honduras', 'Jamaica', 'Mexico',
  'Nicaragua', 'Panama', 'Trinidad and Tobago', 'United States'
)

SOUTH_AMERICA <- c(
  'Argentina', 'Aruba', 'Bermuda', 'Bolivia', 'Brazil',
  'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay',
  'Peru', 'Puerto Rico', 'Suriname', 'Uruguay', 'Venezuela'
)

dat_us <- get_us_data()

continent_list <- case_when(
  country_codes$country_name %in% NA_COUNTRIES ~ 'North America',
  country_codes$continent == 'Americas' & !country_codes$country_name %in% NA_COUNTRIES ~ 'South America',
  TRUE ~ as.character(country_codes$continent)
)

shinyServer(function(session, input, output) {
  
  # Reactive Elements for the Map
  selected_mapdate <- reactive({ input$mapdate })
  selected_variable <- reactive({ input$variable_type })
  selected_measure <- reactive({ input$index_type })
  selected_region <- reactive({ input$region })
  
  # adjust selectInput options for USA and Stringency selections
  observe({
    region <- input$region
    var <- input$variable_type

    if(region == 'USA') {
      
      updateSelectInput(
        session, 'variable_type',
        choices = c(
          'Deaths' = 'Deaths',
          'Cases' = 'Cases'
          ), selected = selected_variable()
      )
      
    } else {
      
      updateSelectInput(
        session, 'variable_type',
        choices = c(
          'Stringency Index' = 'StringencyIndex',
          'Deaths' = 'Deaths',
          'Cases' = 'Cases'
        ), selected = selected_variable()
      )
      
    }

    if(var == 'StringencyIndex') {
      
      updateSelectInput(
        session, 'region',
        choices = c(
          'World' = 'World',
          'Europe' = 'Europe',
          'North America' = 'NorthAmerica',
          'South America' = 'SouthAmerica',
          'Asia' = 'Asia',
          'Africa' = 'Africa',
          'OECD' = 'OECD'),
        selected = selected_region()
      )
      
    } else {
      
      updateSelectInput(
        session, 'region',
        choices = c(
          'World' = 'World',
          'Europe' = 'Europe',
          'North America' = 'NorthAmerica',
          'South America' = 'SouthAmerica',
          'Asia' = 'Asia',
          'Africa' = 'Africa',
          'OECD' = 'OECD',
          'USA (States)' = 'USA'
        ), selected = selected_region()
      )
      
    }

  })
  
  
  
  observeEvent(input$regions, {
    
    if (input$regions == 'I want to make my own selection') {
      country_ls = c('Germany', 'Netherlands', 'Romania', 'Serbia', 'United Kingdom')
      country_choices = dat$country_name %>% unique()
      
    } else if (input$regions == 'Africa') {
      country_ls = AFRICA
      country_choices = AFRICA
      
    } else if (input$regions == 'North America'){
      country_ls = NORTH_AMERICA
      country_choices = NORTH_AMERICA
      
    } else if (input$regions == 'South America'){
      country_ls = SOUTH_AMERICA
      country_choices = SOUTH_AMERICA
      
    } else if (input$regions == 'Asia'){
      country_ls = ASIA
      country_choices = ASIA
      
    } else if (input$regions == 'Europe'){
      country_ls = EUROPE
      country_choices = EUROPE
      
    } else if (input$regions == 'Oceania'){
      country_ls = OCEANIA
      country_choices = OCEANIA
      
    } else if (input$regions == 'OECD'){
      country_ls = OECD
      country_choices = OECD
    }
    
    updateSelectInput(session, 'countries_lockdown', choices = country_choices, selected = country_ls)
  })

  observeEvent(input$clear_graph, {
    updateSelectInput(session, 'countries_lockdown', selected = '')
  })
  
  observeEvent(input$all_graph, {
    updateSelectInput(session, 'countries_lockdown', selected = unique(dat$country_name))
  })
  
  country_list <- reactive({
      input$countries_lockdown
  })
  
  selected_countries <- reactive({ 
    input$refresh
    isolate(country_list())
  })
  
  num_cols <- reactive({
    len <- length(selected_countries())
    ifelse(len > 10, 5, ifelse(len > 4, 4, len))
  })
  
  selected_plot <- reactive({
    
    if (length(selected_countries()) == 0) {
      
      plt <- plot.new()
      
    } else if (input$graph == 'New Deaths per Million') {
      
      plt <- plot_stringency_data_deaths_relative(dat, selected_countries(),
                                                  num_cols()) 
    
    } else {
      
      plt <- plot_stringency_data_cases_relative(dat, selected_countries(),
                                                 num_cols())
      
    } 
    
    plt
               
  })
  
  how_high <- reactive({
    len <- length(selected_countries())
    (((len - 1) %/% num_cols()) + 1) * 200
  })
  
  # height_of_box <- reactive({
  #   paste0(as.character(how_high() + 500),'px')
  # })
  
  output$lockdown_plot_lines_scales <- renderPlot({
    input$refresh
    isolate(selected_plot())
  }, height = how_high)
  
  # Reactive Elements for the Table
  selected_countries_table <- eventReactive(input$TableApply, { input$countries_table }, ignoreNULL = FALSE)

  observeEvent(input$TableClear, {
    updateSelectInput(session, 'countries_table', selected = '')
  })
  
  observeEvent(input$TableAll,{
    updateSelectInput(session, 'countries_table', selected = unique(dat$country_name))
  })
  
  observeEvent(input$continent_table, {
    
    if (input$continent_table == 'World'){
      
      sel_cont <- unique(dat$country_name)
      sel_cnt <- input$countries_table

     if (input$TableApply == 0) {
      sel_cnt <- unique(dat$country_name)
      
     }
    
    } else {
      
      sel_cont <- country_codes %>%
        mutate(continent = continent_list) %>% 
        filter(continent == input$continent_table) %>%
        select(country_name) %>% 
        filter(country_name %in% dat$country_name)
      
      sel_cont <- sel_cont[,1]
      sel_cnt <- input$countries_table
      
    }
    
    updateSelectInput(session, 'countries_table', choices = sel_cont, selected = sel_cnt)
  })


  output$heatmap <- renderPlotly({

    # create plot
    p <- plot_world_data(dat, selected_mapdate(), selected_variable(), selected_measure(), selected_region(), dat_us)

    p
    
  })
  
  output$table_legend <- renderPlot({
    swatchplot(
      'Table \nlegend' = sequential_hcl(
        n = 10, h = c(250, 90), c = c(40, NA, 22),
        l = c(68, 100), power = c(3, 3), rev = TRUE#, register =
      ), font = 3,cex = 0.9, line = 3
    )
  })

  output$countries_table <- DT::renderDT({
    rowCallback <- c(
      'function(row, data){',
      '  for(var i=2; i<data.length; i++){',
      '    if(data[i] === null){',
      "      $('td:eq('+i+')', row).html('Not Implemented')",
      "        .css({'color': 'rgb(0,0,0)', 'font-style': 'italic'});",
      '    } else if(data[i] < 0){',
      "      $('td:eq('+i+')', row).html('Lifted '+ Math.abs(data[i]) + ' Days Ago')",
      "        .css({'font-style': 'normal'});",
      '  }',
      '}',
      '}'
    )

    tab <- prepare_country_table(dat, selected_countries_table())
    tab <- datatable(
      tab, options = list(
        columnDefs = list(list(targets = 10, visible = FALSE)), rowCallback = JS(rowCallback)
        # columnDefs = list(list(targets = 10:17, visible = FALSE)), rowCallback = JS(rowCallback)
        )
      ) %>%
      formatString(2:9, 'In Place for ',' Days') %>%
      formatStyle(
        'roll', target = 'row',
        backgroundColor = styleInterval(
          seq(0, 1.1, length.out = 9),
          sequential_hcl(
            n = 10, h = c(250, 90), c = c(40, NA, 22), l = c(68, 100), power = c(3, 3), rev = TRUE#, register =
          )
        )
      )
    
    return(tab)
  })
  
    
    # TODO: add this when we have the flag variable for the stringency index!
      # formatStyle(
      #   'Mandatory school closing', valueColumns = 'school_closing_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # ) %>%
      # formatStyle(
      #   'Mandatory workplace closing',
      #   valueColumns = 'workplace_closing_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # ) %>%
      # formatStyle(
      #   'Mandatory cancellation of public events',
      #   valueColumns = 'cancel_events_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # ) %>%
      # formatStyle(
      #   'Mandatory public transport closing',
      #   valueColumns = 'transport_closing_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # ) %>%
      # formatStyle(
      #   'Gatherings restricted below 100 people',
      #   valueColumns = 'gatherings_restrictions_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # ) %>%
      # formatStyle(
      #   'Leaving home restricted by law (with minimal exceptions)',
      #   valueColumns = 'internal_movement_restrictions_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # ) %>%
      # formatStyle(
      #   'Mandatory restrictions of internal transport',
      #   valueColumns = 'international_movement_restrictions_flag',
      #   fontWeight = styleEqual(1, 'bold')
      # )
})