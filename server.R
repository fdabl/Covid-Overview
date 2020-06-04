library('DT')
source('helpers.R')


NA_countries <- c(
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

dat <- get_stringency_csv()
country_codes <- get_country_codes()
continent_list <- case_when(
  country_codes$CountryName %in% NA_countries ~ 'North America', 
 (country_codes$continent=='Americas')&(!country_codes$CountryName %in% NA_countries ) ~ 'South America', 
 TRUE ~ as.character(country_codes$continent))


shinyServer(function(session, input, output) {
  
  # Reactive Elements for the Map
  selected_mapdate <- reactive({ input$mapdate })
  selected_variable <- reactive({ input$variable_type })
  selected_measure <- reactive({ input$index_type })
  selected_continent <- reactive({ input$continent })
  
  # Reactive Elements for the Stringency Plot
  selected_countries <- reactive({ 
    input$Countries
    isolate(input$countries_lockdown) })
  
  num_cols <- reactive({
    len <- length(selected_countries())
    ifelse(len > 10, 5, ifelse(len > 4, 4, len))
  })
  
  how_high <- reactive({
    len <- length(selected_countries())
    (((len - 1) %/% num_cols()) + 1) * 200
  })
  
  height_of_box = reactive({
    
    paste0(as.character(how_high()+500),"px")
    
  })
  
  # Reactive Elements for the Table

  selected_countries_table <- eventReactive(input$TableApply, {input$countries_table}, ignoreNULL = FALSE )

  observeEvent(input$TableClear, {
    updateSelectInput(session,'countries_table',selected = "")
  })
  observeEvent(input$TableAll,{
    updateSelectInput(session,'countries_table',selected = unique(dat$CountryName))
  })
  
  observeEvent(input$continent_table, {
    if (input$continent_table == 'World')
    {sel_cont<-unique(dat$CountryName)
    sel_cnt<-input$countries_table
     if(input$TableApply == 0){
      sel_cnt<-unique(dat$CountryName)
     }
    }
    else{
      sel_cont <- country_codes %>% mutate(continent=continent_list) %>% 
                  filter(continent==input$continent_table) %>% select(CountryName) %>% 
                  filter(CountryName %in% dat$CountryName)
      sel_cont<-sel_cont[,1]
      sel_cnt<-input$countries_table
    }
    updateSelectInput(session,'countries_table', choices = sel_cont,selected = sel_cnt)
  })
  
  output$lockdown_plot <- renderPlot({
    plot_stringency_data(dat, selected_countries(), num_cols())
  }, height = how_high)
  
  
  # TODO: Make this a plotly figure
  output$heatmap <- renderPlotly({
    p <- plot_world_data(dat, selected_mapdate(), selected_variable(), selected_measure(), selected_continent())
    p
  })
  
  output$table_legend <- renderPlot({
    swatchplot('Table \nlegend' = sequential_hcl(n = 10, h = c(250, 90), c = c(40, NA, 22), l = c(68, 100), power = c(3, 3), rev = TRUE, register = ),font=3,cex=0.9,line=3)
  })
  
  output$countries_table <- renderDataTable({
    rowCallback <- c(
      "function(row, data){",
      "  for(var i=2; i<data.length; i++){",
      "    if(data[i] === null){",
      "      $('td:eq('+i+')', row).html('Not Implemented')",
      "        .css({'color': 'rgb(0,0,0)', 'font-style': 'italic'});",
      "    } else if(data[i] < 0){",
      "      $('td:eq('+i+')', row).html('Lifted '+ Math.abs(data[i]) + ' Days Ago')",
      "        .css({'font-style': 'normal'});",
      "  }",
      "}",
      "}"
    )
    
    tab <- prepare_country_table(dat, selected_countries_table())
    tab <- datatable(tab, options = list(columnDefs = list(list(targets = 10:17, visible = FALSE)), rowCallback = JS(rowCallback))) %>% formatString(2:9,"In PLace for "," Days") %>%
      formatStyle('roll',target='row',
                  backgroundColor = styleInterval(seq(0,1.1,length.out = 9),
                  sequential_hcl(n = 10, h = c(250, 90), c = c(40, NA, 22), l = c(68, 100), power = c(3, 3), rev = TRUE, register = )
                  )) %>%
      formatStyle('Mandatory school closing',valueColumns = 'C1_Flag', fontWeight = styleEqual(1,"bold")) %>%
      formatStyle('Mandatory workplace closing',valueColumns = 'C2_Flag', fontWeight = styleEqual(1,"bold")) %>%
      formatStyle('Mandatory cancellation of public events',valueColumns = 'C3_Flag', fontWeight = styleEqual(1,"bold")) %>%
      formatStyle('Mandatory public transport closing',valueColumns = 'C4_Flag', fontWeight = styleEqual(1,"bold")) %>%
      formatStyle('Gatherings restricted below 100 people',valueColumns = 'C5_Flag', fontWeight = styleEqual(1,"bold")) %>%
      formatStyle('Leaving home restricrted by law (with minimal exceptions)',valueColumns = 'C6_Flag', fontWeight = styleEqual(1,"bold")) %>%
      formatStyle('Mandatory restrictions of internal transport',valueColumns = 'C7_Flag', fontWeight = styleEqual(1,"bold"))
    tab
  })
  
  
  
})
