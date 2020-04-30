library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(maps)
library(wbstats)
library(viridis)
library(shinyWidgets)



# shortlist a few codes to make the toy example, it should customized
# selected_countries <- list("AUS", "AUT", "BGR","CAN", "CHN", "CZE", "DNK", "SLV",
#                            "EST", "FIN", "FRA", "DEU", "GRC", "HKG", "HUN", "ISL",
#                            "IND", "IRN", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD",
#                            "NZL", "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "ZAF",
#                            "KOR", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")



data <- read_csv("data_04_24.csv") %>% 
  select(1:26)

data1 <- read_csv("data_processed.csv") 

world <- map_data('world') %>% data.frame() %>% filter(region != "Antarctica") # Antarctica excluded
# name adjustments
world$region <- recode(world$region, "USA" = "United States", "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                       "UK" = "United Kingdom", "Kyrgyzstan" = "Kyrgyz Republic", "Slovakia" = "Slovak Republic", 
                       "Swaziland" = "Eswatini", "Trinidad" = "Trinidad and Tobago", "Tobago" = "Trinidad and Tobago")
map_data <- left_join(data1, world, by = c("CountryName" = "region"))
pop_data <- wb(country = "all", indicator = "SP.POP.TOTL", startdate = 2019, enddate = 2019, mrv = 1, gapfill = TRUE) %>%
  dplyr::rename(population = value) %>%
  select(iso3c, population, date)

map_data <- left_join(map_data, pop_data, by = c("CountryCode" = "iso3c")) # Taiwan and Kosovo missing
map_data <- map_data %>% mutate(DeathsPerMillion = round((ConfirmedDeaths/population)*1000000),
                                CasesPerMillion = round((ConfirmedCases/population)*1000000))

shinyServer(function(input, output) {
  
  # use user-selected countries
  selected_countries = reactive({input$id}) 
  data_for_graph <- reactive({
    data1%>% 
    # select only data from some countries
    filter(CountryName %in% selected_countries())
  })
  
  
  


  # set the number of columns depending on number of countries selected
  num_cols = reactive({
    if (length(input$id) > 10 ) {
      x = 5
    }
    else if (length(input$id) > 4) {
      x = 4
    }
    else {
      x = length(input$id) 
    }
    return(x)
  })
  
  
  # set the height of the graph depending on number of countries selected
  how_high = reactive({
    (((length(input$id)-1) %/%  num_cols()) + 1)*150
  })
  
  output$first_plot <- renderPlotly({
    # make the graph
    graph <- ggplot(data = data_for_graph())+
      geom_line(aes(x = date_processed, y = StringencyIndexForDisplay, group=1,
                    text = paste(date_processed, "\n", StringencyIndexForDisplay)))+
      facet_wrap(vars(CountryName), ncol = num_cols()) +
      ggtitle("Stringency of measures in each selected country") +
      xlab("Date") +
      ylab("Stringency Index")
    
    
    # apply a plotly layer, look into customizing the tooltip
    graph_ly <- graph %>% 
      ggplotly(tooltip = "text", height = how_high(), width=750)
    
    graph_ly 
  })
  
  
  # make the heatmap
  selected_mapdate = reactive({input$mapdate})
  selected_infotype = reactive({input$infoType})
  # select data for chosen date
  data_for_map <- reactive({map_data %>%
      filter(date_processed == selected_mapdate())
  })
  
  output$heatmap <- renderPlotly({
    switch(selected_infotype(),
           "StringencyIndexForDisplay" = 
             ggplotly(
              ggplot(data_for_map(), aes(x = long, y = lat)) +
                geom_polygon(data = world, aes(group = group), fill = "lightgrey", colour = "black") +
                geom_polygon(aes(group = group, fill = StringencyIndexForDisplay,
                                 text = paste(CountryName, "\n", StringencyIndexForDisplay)),
                             colour = "black") +
                scale_fill_viridis_c(option = "plasma", limits=c(0, 100), # so we always get the same color for the same value
                                     name = "Stringency of measures") + # use colourblind-friendly palette
                scale_x_continuous(expand = c(0,0)) +
                scale_y_continuous(expand = c(0,0)) +
                ggtitle("Stringency of Lockdown Measures around the World") +
                theme_bw() +
                theme(axis.title = element_blank(), axis.text = element_blank(), 
                      axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
                      legend.position="top", panel.grid.minor = element_blank(), 
                      panel.grid.major = element_blank()),
              tooltip = "text"),
           "DeathsPerMillion" = 
             ggplotly(
               ggplot(data_for_map(), aes(x = long, y = lat)) +
                 geom_polygon(data = world, aes(group = group), fill = "lightgrey", colour = "black") +
                 geom_polygon(aes(group = group, fill = DeathsPerMillion,
                                  text = paste(CountryName, "\n", DeathsPerMillion)),
                              colour = "black") +
                 scale_fill_viridis_c(option = "plasma", 
                                      limits=c(0, ceiling(max(map_data$DeathsPerMillion, na.rm = TRUE)/100)*100), # dynamic upper limit
                                      name = "Deaths per million") + # use colourblind-friendly palette
                 scale_x_continuous(expand = c(0,0)) +
                 scale_y_continuous(expand = c(0,0)) +
                 ggtitle("Confirmed Deaths per Million Inhabitants") +
                 theme_bw() +
                 theme(axis.title = element_blank(), axis.text = element_blank(), 
                       axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
                       legend.position="top", panel.grid.minor = element_blank(), 
                       panel.grid.major = element_blank()),
               tooltip = "text"),
           "CasesPerMillion" = 
             ggplotly(
               ggplot(data_for_map(), aes(x = long, y = lat)) +
                 geom_polygon(data = world, aes(group = group), fill = "lightgrey", colour = "black") +
                 geom_polygon(aes(group = group, fill = CasesPerMillion,
                                  text = paste(CountryName, "\n", CasesPerMillion)),
                              colour = "black") +
                 scale_fill_viridis_c(option = "plasma", 
                                      limits=c(0, ceiling(max(map_data$CasesPerMillion, na.rm = TRUE)/1000)*1000), # dynamic upper limit
                                      name = "Cases per million") + # use colourblind-friendly palette
                 scale_x_continuous(expand = c(0,0)) +
                 scale_y_continuous(expand = c(0,0)) +
                 ggtitle("Confirmed Cases per Million Inhabitants") +
                 theme_bw() +
                 theme(axis.title = element_blank(), axis.text = element_blank(), 
                       axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
                       legend.position="top", panel.grid.minor = element_blank(), 
                       panel.grid.major = element_blank()),
               tooltip = "text")
    )
    
  # heatmap_ly <- heatmap %>%
  #   ggplotly(tooltip = "text")
  # 
  # heatmap_ly
  })
})
