library(tidyverse)
library(lubridate)
library(maps)
library(viridis)

data <- read_csv("data_04_24.csv") %>% 
  select(1:26)

# get all country codes:
country_codes <- unique(data$CountryCode)
country_codes

data_for_graph <- data %>% 
  mutate(date_processed = as_date(as.character(Date), ymd)) %>% 
  mutate(country = as.factor(CountryName)) %>% 
  group_by(country) %>% 
  arrange(Date) %>% 
  
  # conservative decision: we fill in missings with the last recorded value
  fill(ConfirmedCases) %>% 
  fill(ConfirmedDeaths) %>%
  #for StringencyIndexForDisplay fill in with adjecent values
  fill(StringencyIndexForDisplay,.direction="downup") %>%
  
  # before the first record, we consider that the value is 0
  mutate(ConfirmedCases=replace_na(ConfirmedCases, 0)) %>% 
  mutate(ConfirmedDeaths=replace_na(ConfirmedDeaths, 0)) 

write_csv(data_for_graph, "data_processed.csv")

graph <- ggplot(data = data_for_graph, 
                aes(x = date_processed, y = StringencyIndexForDisplay))+
  geom_line()+
  facet_wrap(vars(CountryName), ncol = 5) +
  ggtitle("Stringency of measures in each selected country") +
  xlab("Date") +
  ylab("Stringency Index")

world <- map_data('world') %>% data.frame() %>% filter(region != "Antarctica") # Antarctica excluded
# name adjustments
world$region <- recode(world$region, "USA" = "United States", "Democratic Republic of the Congo" = "Democratic Republic of Congo",
                       "UK" = "United Kingdom", "Kyrgyzstan" = "Kyrgyz Republic", "Slovakia" = "Slovak Republic", 
                       "Swaziland" = "Eswatini", "Trinidad" = "Trinidad and Tobago", "Tobago" = "Trinidad and Tobago")

# worldmap <- ggplot(world, aes(x = long, y = lat, fill)) +
#   geom_polygon(aes(group = group), fill = "white", colour = "black") +
#   scale_x_continuous(expand = c(0,0)) +
#   scale_y_continuous(expand = c(0,0)) +
#   theme_bw()

# temp example with only one day. We would want a slider so participants can slide through the dates.
data_for_map <- left_join(data_for_graph, world, by = c("CountryName" = "region")) %>% filter(Date == "20200423")

heatmap <- ggplot(data_for_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = StringencyIndexForDisplay), colour = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Stringency of measures") + # use colourblind-friendly palette
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  ggtitle("Stringency of measures around the world") +
  theme_bw() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), plot.title = element_text(hjust = 0.5))
