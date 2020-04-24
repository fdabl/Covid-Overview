library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)


# shortlist a few codes to make the toy example, it should customized
selected_countries <- list("AUS", "AUT", "BGR","CAN", "CHN", "CZE", "DNK", "SLV",
                           "EST", "FIN", "FRA", "DEU", "GRC", "HKG", "HUN", "ISL",
                           "IND", "IRN", "IRL", "ISR", "ITA", "JPN", "LUX", "NLD",
                           "NZL", "NOR", "POL", "PRT", "ROU", "RUS", "SRB", "ZAF",
                           "KOR", "ESP", "SWE", "CHE", "TUR", "GBR", "USA")

data_for_graph <- read_csv("data_processed.csv") %>% 
  # select only data from some countries
  filter(CountryCode %in% selected_countries)




shinyServer(function(input, output) {
  
  # make the graph
  graph <- ggplot(data = data_for_graph, aes(x = date_processed, y = StringencyIndexForDisplay))+
    geom_line()+
    facet_wrap(vars(CountryName), ncol = 5) +
    ggtitle("Stringency of measures in each selected country") +
    xlab("Date") +
    ylab("Stringency Index")
  
  # apply a plotly layer, look into customizing the tooltip
  graph_ly <- graph %>% 
    ggplotly(height = 950, width=700)
  
  output$first_plot <- renderPlotly({
    graph_ly 
  })
  
  
})
