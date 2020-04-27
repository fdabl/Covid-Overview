library(tidyverse)
library(ggplot2)
library(lubridate)
library(plotly)
library(maps)
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


shinyServer(function(input, output) {
  
  # use user-selected countries
  selected_countries = reactive({input$id}) 
  data_for_graph <- reactive({
    data1%>% 
    # select only data from some countries
    filter(CountryName %in% selected_countries())
  })
  
  # set the height of the graph depending on number of countries selected
  how_high = reactive({
    (((length(input$id)-1) %/%  5) + 2)*120
  })

  
  output$first_plot <- renderPlotly({
    # make the graph
    graph <- ggplot(data = data_for_graph())+
      geom_line(aes(x = date_processed, y = StringencyIndexForDisplay, group=1,
                    text = paste(date_processed, "\n", StringencyIndexForDisplay)))+
      facet_wrap(vars(CountryName), ncol = 5) +
      ggtitle("Stringency of measures in each selected country") +
      xlab("Date") +
      ylab("Stringency Index")
    
    
    # apply a plotly layer, look into customizing the tooltip
    graph_ly <- graph %>% 
      ggplotly(tooltip = "text", height = how_high(), width=700)
    
    graph_ly 
  })
  
  
  # make the heatmap
  # temp example with only one day. We would want a slider so participants can slide through the dates.
  data_for_map <- left_join(data, world, by = c("CountryName" = "region")) %>% filter(Date == "20200423")
  
  heatmap <- ggplot(data_for_map, aes(x = long, y = lat)) +
    geom_polygon(data = world, aes(group = group), fill = "lightgrey", colour = "black") +
    geom_polygon(aes(group = group, fill = StringencyIndexForDisplay,
                     text = paste(CountryName, "\n", StringencyIndexForDisplay)),
                 colour = "black") +
    scale_fill_viridis_c(option = "plasma", limits=c(0, 100), # so we always get the same color for the same value
                         name = "Stringency of measures") + # use colourblind-friendly palette
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    ggtitle("Stringency of measures around the world") +
    theme_bw() +
    theme(axis.title = element_blank(), axis.text = element_blank(), 
          axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5),
          legend.position="bottom")
  
  heatmap_ly <- heatmap %>% 
    ggplotly(tooltip = "text")
  
  output$heatmap <- renderPlotly({
    heatmap_ly 
  })
})
