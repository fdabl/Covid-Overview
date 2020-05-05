library('httr')
library('readxl')
library('jsonlite')
library('tidyverse')
library('lubridate')
library("plotly")



#' Gets the Stringency Index and Deaths / Confirmed from the Oxford as csv
#' 
#' @param force_download boolean indicating whether to force download new data
#' @returns data frame with Stringency information [this takes about 10 seconds!]
get_stringency_csv <- function(force_download = FALSE) {
  
  if (!file.exists('data/stringency_data.csv') || force_download) {
    url <- 'https://oxcgrtportal.azurewebsites.net/api/CSVDownload'
    d <- content(GET(url))
    
    # Add Population Data
    country_data <- read_csv('data/country_data.csv')
    
    # Preprocessing
    dat <- d %>% 
      left_join(country_data, by = c('CountryCode' = 'Code')) %>% 
      group_by(Country) %>% 
      fill(
        ConfirmedCases,
        ConfirmedDeaths,
        StringencyIndexForDisplay # TODO: Discuss this
      ) %>% 
      mutate(
        Country = CountryName,
        Date = as_date(as.character(Date)),
        
        ConfirmedCases = replace_na(ConfirmedCases, 0),
        ConfirmedDeaths = replace_na(ConfirmedDeaths, 0),
        CasesPerMillion = (ConfirmedCases / Population) * 1e6,
        DeathsPerMillion = (ConfirmedDeaths / Population) * 1e6,
        
        ConfirmedDailyCases = c(0, diff(ConfirmedCases)),
        ConfirmedDailyDeaths = c(0, diff(ConfirmedDeaths)),
        NewCasesPerMillion = (ConfirmedDailyCases / Population) * 1e6,
        NewDeathsPerMillion = (ConfirmedDailyDeaths / Population) * 1e6
      )
    
    write.csv(dat, 'data/stringency_data.csv', row.names = FALSE)
    
  } else {
    dat <- read_csv('data/stringency_data.csv')
  }
  
  dat
}



#' Prepares the world data
#' 
#' @returns prepared world data
get_world_data <- function() {
  
  if (!file.exists('data/world_data.csv')) {
    world <- ggplot2::map_data('world') %>% 
      filter(region != 'Antarctica') %>% 
      mutate(
        region = recode(
          region, 'USA' = 'United States', 'UK' = 'United Kingdom',
          'Democratic Republic of the Congo' = 'Democratic Republic of Congo',
          'Kyrgyzstan' = 'Kyrgyz Republic', 'Slovakia' = 'Slovak Republic', 
          'Swaziland' = 'Eswatini', 'Trinidad' = 'Trinidad and Tobago',
          'Tobago' = 'Trinidad and Tobago'
        )
      )
    
    write.csv(world, 'data/world_data.csv', row.names = FALSE)
    
  } else {
    world <- read.csv('data/world_data.csv')
  }
  
  world
  
}
  

#' Returns number of days since the intervention is in place
#' TODO: Think about how to handle NAs
#' 
#' @param indicator boolean vector indicating when the intervention was in place
#' @returns number of days since when the last intervention is in place
days_active <- function(indicator) {
  nr_days <- length(indicator)
  
  # indicator is boolean, so this yields 1 when FALSE switches to TRUE (or vice versa)
  last_change <- last(which(diff(indicator) == 1))
  
  nr_days - last_change
}


#' Prepares the table of lockdown lifts
#' 
#' @param dat stringency data
#' @param countries selected countries
#' @returns data frame
prepare_country_table <- function(dat, countries) {
  
  # New Stringency Index is computed using C1-8 and H1
  # Let's create a table for C1-8
  
  cnames <- c(
    'Country',
    'Schools closed',                # 2 is on some level, 3 is all levels closed
    'Workplaces closed',             # 2 is required for some, 3 is required for all but non-essential
    'Public events cancelled',       # 1 is recommended, 2 is required
    'Public transport closed',       # 1 is recommended, 2 is required
    'Gatherings restricted',         # 0 are no restrictions, up to 4 in severity
    'Stay at home',                  # 2 and 3 are restrictions
    'Internal movement restricted',  # 1 recommended, 2 is required
    'International travel controls'  # 1 screening, 2 is quarantine, 3 is ban on high-risk regions, 4 is border closures
  )
  
  d <- dat %>% 
    filter(Country %in% countries) %>% 
    
    mutate(
      # Closure and Containment Measures
      C1_schools_closed = `C1_School closing` > 2,
      C2_workplace_closed = `C2_Workplace closing` > 2,
      C3_public_events_cancelled = `C3_Cancel public events` > 1,
      C4_gatherings_restricted = `C4_Restrictions on gatherings` > 0,
      C5_public_transport = `C5_Close public transport` > 1,
      C6_stay_home = `C6_Stay at home requirements` > 1,
      C7_internal_movement = `C7_Restrictions on internal movement` > 1,
      C8_international_travel = `C8_International travel controls` > 1,
      
      # # Economic Measures
      # H1_info_campaigns = `H1_Public information campaigns`,
      # H2_testing = `H2_Testing policy`,
      # H3_contact_tracing = `H3_Contact tracing`,
      # H4_healthcare_investment = `H4_Emergency investment in healthcare`,
      # H5_vaccine_investment = `H4_Emergency investment in healthcare`,
      # 
      # # Public Health Measures
      # E1_income = `E1_Income support`,
      # E2_debt = `E2_Debt/contract relief`,
      # E3_fiscal = `E3_Fiscal measures`,
      # E4_international = `E4_International support`,
      # 
      # # Miscellaneous Measures
      # M1_other = M1_Wildcard
    ) %>% 
    
    group_by(Country) %>% 
    summarize(
      C1_days = days_active(C1_schools_closed),
      C2_days = days_active(C2_workplace_closed),
      C3_days = days_active(C3_public_events_cancelled),
      C4_days = days_active(C4_gatherings_restricted),
      C5_days = days_active(C5_public_transport),
      C6_days = days_active(C6_stay_home),
      C7_days = days_active(C7_internal_movement),
      C8_days = days_active(C8_international_travel)
    )
  
  colnames(d) <- cnames
  
  dat <- apply(d[, -1], c(1, 2), function(element) {
    ifelse(is.na(element), 'Not Implemented', paste0('Since ', element, ' Days'))
  })
  
  cbind(d[, 1], dat)
}


#' Returns ggplot of map filled according to variable
#' TODO: Change this to a fast plotly implementation (instead of calling ggplotly on the result)
#' TODO: Scale Deaths and Cases differently for sound visualisation
#' 
#' @param world world data
#' @param sdat stringency data
#' @param date date for the stringency index
#' @param variable variable which should be shown
#' @param measure type of measure that should be shown if StringencyIndex is selected as variable
#' @returns ggplot object
plot_world_data <- function(dat, date, variable, measure) {
  #d <- left_join(world, filter(dat, Date == date), by = c('region' = 'Country'))
  d <- filter(dat, Date == date)
  
  if (variable == 'StringencyIndex') {
    legend_title <- 'Stringency Index'
    if (measure == "Combined") {
      title <- 'Stringency of Lockdown Across the World'
      d$variable <- d$StringencyIndexForDisplay
      limits <- c(0,100)
    } else if(measure == "School") {
      title <- "Stringency of School Closing around the World"
      d$variable <- d$`C1_School closing`
      limits <- c(0,3)
    } else if(measure == "Workplace") {
      title <- "Stringency of Workplace Closing around the World"
      d$variable <- d$`C2_Workplace closing`
      limits <- c(0,3)
    } else if(measure == "PublicEvents") {
      title <- "Stringency in Cancelling Public Events around the World"
      d$variable <- d$`C3_Cancel public events`
      limits <- c(0,2)
    } else if(measure == "Gatherings") {
      title <- "Stringency in Restricting Gatherings around the World"
      d$variable <- d$`C4_Restrictions on gatherings`
      limits <- c(0,4)
    } else if(measure == "Transport") {
      title <- "Stringency in Closing Public Transport around the World"
      d$variable <- d$`C5_Close public transport`
      limits <- c(0,2)
    } else if(measure == "Home") {
      title <- "Stringency of Stay at Home Requirements around the World"
      d$variable <- d$`C6_Stay at home requirements`
      limits <- c(0,3)
    } else if(measure == "Movement") {
      title <- "Stringency of Internal Movement Restrictions around the World"
      d$variable <- d$`C7_Restrictions on internal movement`
      limits <- c(0,2)
    } else if(measure == "Travel") {
      title <- "Stringency of International Travel Controls around the World"
      d$variable <- d$`C8_International travel controls`
      limits <- c(0,4)
    } 
      
  } else if (variable == 'Deaths') {
    
    title <- 'Confirmed Deaths per Million Across the World'
    legend_title <- 'Deaths per Million'
    d$variable <- d$DeathsPerMillion
    limits = c(0, ceiling(quantile(dat$DeathsPerMillion, .99, na.rm = TRUE)*100)/100)
    
  } else if (variable == 'Cases') {
    
    title <- 'Confirmed Cases per Million Across the World'
    legend_title <- 'Cases per Million'
    d$variable <- d$CasesPerMillion
    limits = c(0, ceiling(quantile(dat$CasesPerMillion, .99, na.rm = TRUE)*1000)/1000)
    
  }
  plot_ly(data = d, type = "choropleth", locations = d$CountryCode, z = d$variable, stroke = I("black"), span = I(1),
          text = paste0(d$CountryName)) %>%
    colorbar(title = legend_title, limits = limits, y = 0.75) %>%#, x = 0.5, xanchor = "center", y = 1.5) %>%
    layout(title = list(text = title), geo = list(lataxis = list(range = c(-55, 80))))
  # layout(legend = list(orientation = "h",
  #                      xanchor = "center",
  #                      x = 0.5))
  #add_trace(color = ~variable, type = "choropleth", locations = unique(CountryName))
  
  # ggplot(d, aes(x = long, y = lat)) +
  #   geom_polygon(aes(group = group, fill = variable)) +
  #   scale_fill_viridis_c(option = 'plasma', limits = c(0, 100), name = legend_title) + 
  #   scale_x_continuous(expand = c(0, 0)) +
  #   scale_y_continuous(expand = c(0, 0)) +
  #   ggtitle(title) +
  #   theme_bw() +
  #   theme(
  #     legend.position = 'top',
  #     axis.title = element_blank(),
  #     axis.text = element_blank(),
  #     axis.ticks = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.grid.major = element_blank(),
  #     plot.title = element_text(hjust = 0.5)
  #   )
}


#' Returns ggplot of stringencyc across countries
#' 
#' @param dat stringency index data
#' @param countries list of countries to be shown
#' 
#' @returns ggplot object
plot_stringency_data <- function(dat, countries, nr_cols) {
  
  d <- filter(dat, Country %in% countries)
  
  ggplot(d, aes(x = Date)) +
    geom_line(aes(y = StringencyIndexForDisplay, color = 'Stringency Index')) +
    geom_bar(
      stat = 'identity',
      aes(y = NewDeathsPerMillion*10, color = 'New Deaths per 10 Million')
    ) +
    scale_colour_manual(values = c('#E41A1C', 'black')) +
    scale_y_continuous(
      sec.axis = sec_axis(~.*10, name = 'New Deaths per 10 Million'), limits = c(0, 100)
    ) +
    facet_wrap(~ Country, ncol = nr_cols) +
    ylab('Stringency Index') +
    ggtitle('Stringency of Measures and New Deaths per 10 Million') +
    theme_bw() +
    theme(
      legend.position = 'None',
      plot.title = element_text(hjust = 0.5, size = 16)
    )
}


#' Currently not used: JSON API does not give us information about individual Stringency elements
#' #' Gets the Stringency Index and Deaths / Confirmed from the Oxford API
#' #' @param start Start date for data
#' #' @param end End date for data
#' #' 
#' #' @returns data frame with the data between start and end
#' get_stringency_data <- function(start = '2020-01-02', end = today()) {
#'   url <- paste0('https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/', start, '/', end)
#'   json <- fromJSON(txt = url, simplifyDataFrame = TRUE)
#'   
#'   dat <- enframe(json$data, name = 'Date', value = 'value') 
#'   
#'   dat <- dat %>%
#'     unnest_longer(value, values_to = 'value', indices_to = 'Code') %>% 
#'     unnest_longer(value, values_to = 'value', indices_to = 'Index') %>% 
#'     mutate(
#'       value = as_vector(value),
#'       Date = ymd(Date),
#'       value = as.numeric(value)
#'     ) %>% 
#'     filter(Index != c('date_value', 'country_code'))
#'   
#'   country_codes <- read_csv(file = 'data/country_codes.csv')
#'   left_join(dat, country_codes, by = 'Code')
#' }
