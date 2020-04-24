library(tidyverse)
library(lubridate)

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


