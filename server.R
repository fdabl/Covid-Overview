library('DT')
source('helpers.R')


dat <- get_stringency_csv()
country_codes <- get_country_codes()


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

  selected_countries_table <- eventReactive(input$TableApply, { input$countries_table}, ignoreNULL = FALSE )

  observeEvent(input$continent_table, {
    if (input$continent_table=='World')
    {sel_cont<-unique(dat$CountryName)
    sel_cnt<-selected_countries_table()
     if(input$TableApply == 0){
      sel_cnt<-(c('Germany', 'Netherlands', 'Romania', 'United Kingdom'))
     }
    }
    else{
      sel_cont <- country_codes %>% filter(continent==input$continent_table) %>% select(CountryName)                    %>% filter(CountryName %in% dat$CountryName)
      sel_cont<-sel_cont[,1]
      sel_cnt<-selected_countries_table()
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
