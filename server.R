library('DT')
source('helpers.R')


dat <- get_stringency_csv()
country_codes <- get_country_codes() %>% 
  select(-"CountryName")

dat <- dat %>% #select(-CountryName) %>% 
  left_join(country_codes, by = 'CountryCode')

country_codes <- get_country_codes()

africa_list <- (dat %>% filter(continent == 'Africa'))$CountryName %>%
  unique()
north_america_list <- c('Barbados', 'Belize', 'Canada', 'Costa Rica', 'Cuba',
                        'Dominica', 'Dominican Republic', 'El Salvador', 
                        'Greenland', 'Guatemala', 'Honduras', 'Jamaica', 'Mexico',
                        'Nicaragua', 'Panama', 'Trinidad and Tobago',
                        'United States')
south_america_list <- c('Argentina', 'Aruba', 'Bermuda', 'Bolivia', 'Brazil',
                        'Chile', 'Colombia', 'Ecuador', 'Guyana', 'Paraguay',
                        'Peru', 'Puerto Rico', 'Suriname', 'Uruguay', 
                        'Venezuela')
asia_list <- (dat %>% filter(continent == 'Asia'))$CountryName %>%
  unique()
europe_list <- (dat %>% filter(continent == 'Europe'))$CountryName %>%
  unique()
oceania_list <- (dat %>% filter(continent == 'Oceania'))$CountryName %>%
  unique()

oecd_list <- c('Australia', 'Austria', 'Belgium', 'Canada', 'Chile', 'Colombia',
               'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 
               'Germany','Greece', 'Hungary', 'Iceland', 'Ireland', 'Israel',
               'Italy', 'Japan', 'South Korea', # 'Latvia', -> not in the database
               'Lithuania', 
               'Luxembourg', 'Mexico', 'Netherlands', 'New Zealand', 'Norway',
               'Poland', 'Portugal', 'Slovak Republic', 'Slovenia','Spain', 
               'Sweden', 'Switzerland', 'Turkey', 'United Kingdom', 'United States')

shinyServer(function(session, input, output) {
  
  # Reactive Elements for the Map
  selected_mapdate <- reactive({ input$mapdate })
  selected_variable <- reactive({ input$variable_type })
  selected_measure <- reactive({ input$index_type })
  selected_continent <- reactive({ input$continent })
  
  # Reactive Elements for the Stringency Plot
  
  # countries_region <- reactive({
  #   if (input$regions == 'Africa'){
  #     country_ls = africa_list
  #   } else if (input$regions == 'North America'){
  #     country_ls = north_america_list
  #   } else if (input$regions == 'South America'){
  #     country_ls = south_america_list
  #   } else if (input$regions == 'Asia'){
  #     country_ls = asia_list
  #   } else if (input$regions == 'Europe'){
  #     country_ls = europe_list
  #   } else if (input$regions == 'Oceania'){
  #     country_ls = oceania_list
  #   } else if (input$regions == 'OECD'){
  #     country_ls = oecd_list
  #   }
  # 
  #   country_ls
  # 
  # })
  
  observeEvent(input$regions, {
    if (input$regions == 'I want to make my own selection'){
      country_ls = c('Germany', 'Netherlands', 'Romania',
                     'Serbia', 'United Kingdom')
      country_choices = dat$Country %>% unique()
    } else if (input$regions == 'Africa') {
      country_ls = africa_list
      country_choices = africa_list
    } else if (input$regions == 'North America'){
      country_ls = north_america_list
      country_choices = north_america_list
    } else if (input$regions == 'South America'){
      country_ls = south_america_list
      country_choices = south_america_list
    } else if (input$regions == 'Asia'){
      country_ls = asia_list
      country_choices = asia_list
    } else if (input$regions == 'Europe'){
      country_ls = europe_list
      country_choices = europe_list
    } else if (input$regions == 'Oceania'){
      country_ls = oceania_list
      country_choices = oceania_list
    } else if (input$regions == 'OECD'){
      country_ls = oecd_list
      country_choices = oecd_list
    }
    
    updateSelectInput(session, 'countries_lockdown', choices = country_choices, selected = country_ls)
  })
  # for drop down:
  # country_list <- reactive({
  #   if (input$grouping == 'show selected group') {
  #     country_ls = countries_region()
  #   } else {
  #     country_ls = input$countries_lockdown
  #   }
  #   country_ls
  # })
  # 
  
  # more intuitive:
  country_list <- reactive({
    # if (input$grouping == 'FALSE') {
    #   country_ls = countries_region()
    # } else {
    #   country_ls = 
        input$countries_lockdown
    # }
    # country_ls
  })
  
  selected_countries <- reactive({ 
    input$Refresh
    isolate(country_list()) })
  
  num_cols <- reactive({
    len <- length(selected_countries())
    ifelse(len > 10, 5, ifelse(len > 4, 4, len))
  })
  
  selected_plot <- reactive({
    if (input$graph == 'Daily deaths per 10 Million') {
      plt <- plot_stringency_data_deaths_relative(dat,
                                                  selected_countries(),
                                                  num_cols())
    } else if (input$graph == 'Daily deaths (absolute value)') {
      plt <- plot_stringency_data_deaths_total(dat,
                                               selected_countries(),
                                               num_cols())
    } else if (input$graph == 'New Cases per Million') {
      plt <- plot_stringency_data_cases_relative(dat,
                                                 selected_countries(),
                                                 num_cols())
    } else {
      plt <- plot_stringency_data_cases_total(dat,
                                              selected_countries(),
                                              num_cols())
    }
    
    plt
               
  })
  

  
  how_high <- reactive({
    len <- length(selected_countries())
    (((len - 1) %/% num_cols()) + 1) * 200
  })
  
  height_of_box = reactive({
    paste0(as.character(how_high() + 500),"px")
  })
  
  output$lockdown_plot_lines_scales <- renderPlot({
    input$Refresh
    isolate(selected_plot())
  }, height = how_high)
  
  
  # Reactive Elements for the Table
  selected_countries_table <- eventReactive(input$TableApply, { input$countries_table }, ignoreNULL = FALSE)

  observeEvent(input$TableClear, {
    updateSelectInput(session, 'countries_table', selected = '')
  })
  
  observeEvent(input$TableAll,{
    updateSelectInput(session,'countries_table',selected = unique(dat$Country))
  })
  
  observeEvent(input$continent_table, {
    if (input$continent_table == 'World') {
      
      sel_cont <- unique(dat$Country)
      sel_cnt <- input$countries_table
    
     if(input$TableApply == 0){
      sel_cnt <- unique(dat$Country)
     }
    
    } else{
      
      sel_cont <- country_codes %>% 
        filter(continent == input$continent_table) %>% 
        select(CountryName) %>%
        filter(CountryName %in% dat$Country)
      
      sel_cont <- sel_cont[,1]
      sel_cnt <- input$countries_table
    }
    
    updateSelectInput(session, 'countries_table', choices = sel_cont, selected = sel_cont)
  })


  
  # TODO: Make this a plotly figure
  output$heatmap <- renderPlotly({
    
    # maintain zoom level when changing dates (not working yet)
    #zoomer <- eventReactive(input$mapdate, {event_data("plotly_relayout", "heatmap")})
    # zoom <- zoomer()
    # lataxis <- list(range = c(zoom$lataxis$range[0], zoom$lataxis$range[1]))
    # lonaxis <- list(range = c(zoom$lonaxis$range[0], zoom$lonaxis$range[1]))
    
    # create plot
    p <- plot_world_data(dat, selected_mapdate(), selected_variable(), selected_measure(), selected_continent())
    #lataxis = lataxis, lonaxis = lonaxis)
    
    p
  })
  
  output$countries_table <- renderDataTable({
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=0; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('Not Implemented')",
      "        .css({'color': 'rgb(151,151,151)', 'font-style': 'italic'});",
      "    }",
      "  }",
      "}"  
    )
    tab <- prepare_country_table(dat, selected_countries_table())
    tab <- datatable(tab, options = list(rowCallback = JS(rowCallback))) %>% formatString(2:9,"Since "," Days")
    tab
  })
})
