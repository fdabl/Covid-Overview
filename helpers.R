library('httr')
library('readxl')
library('jsonlite')
library('tidyverse')
library('lubridate')
library("plotly")
library("countrycode")
library("RColorBrewer")
library("colorspace")

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
      mutate(Country = CountryName) %>%
      group_by(Country) %>% 
      fill(
        ConfirmedCases,
        ConfirmedDeaths,
        StringencyIndexForDisplay # TODO: Discuss this
      ) %>% 
      mutate(
        Date = as_date(as.character(Date)),
        
        ConfirmedCases = replace_na(ConfirmedCases, 0),
        ConfirmedDeaths = replace_na(ConfirmedDeaths, 0),
        CasesPerMillion = round((ConfirmedCases / Population) * 1e6, 2),
        DeathsPerMillion = round((ConfirmedDeaths / Population) * 1e6, 2),
        
        ConfirmedDailyCases = c(0, diff(ConfirmedCases)),
        ConfirmedDailyDeaths = c(0, diff(ConfirmedDeaths)),
        NewCasesPerMillion = (ConfirmedDailyCases / Population) * 1e6,
        NewDeathsPerMillion = (ConfirmedDailyDeaths / Population) * 1e6
      ) %>%
      fill(CasesPerMillion,
           DeathsPerMillion,
           `C1_School closing`,
           `C1_Flag`,
           `C2_Workplace closing`,
           `C2_Flag`,
           `C3_Cancel public events`,
           `C3_Flag`,
           `C4_Restrictions on gatherings`,
           `C4_Flag`,
           `C5_Close public transport`,
           `C5_Flag`,
           `C6_Stay at home requirements`,
           `C7_Flag`,
           `C7_Restrictions on internal movement`,
           `C7_Flag`,
           `C8_International travel controls`,
           `H1_Public information campaigns`,
           `H1_Flag`,
           `H2_Testing policy`,
           `H3_Contact tracing`
      ) %>%
      mutate(`rec_C1_School closing` = case_when(
        `C1_School closing` == 0 ~ "No measures",
        `C1_School closing` == 1 ~ "Recommend closing",
        `C1_School closing` == 2 ~ "Require closing on some levels",
        `C1_School closing` == 3 ~ "Require closing"),
        `rec_C2_Workplace closing` = factor(case_when(
          `C2_Workplace closing` == 0 ~ "No measures",
          `C2_Workplace closing` == 1 ~ "Recommend closing or work from home",
          `C2_Workplace closing` == 2 ~ "Require closing or work from home for some sectors",
          `C2_Workplace closing` == 3 ~ "Require closing or work from home except essential workplaces")),
        `rec_C3_Cancel public events` = case_when(
          `C3_Cancel public events` == 0 ~ "No measures",
          `C3_Cancel public events` == 1 ~ "Recommend cancelling",
          `C3_Cancel public events` == 2 ~ "Require cancelling"),
        `rec_C4_Restrictions on gatherings` = case_when(
          `C4_Restrictions on gatherings` == 0 ~ "No restrictions",
          `C4_Restrictions on gatherings` == 1 ~ "Restrictions on gatherings > 1000 people",
          `C4_Restrictions on gatherings` == 2 ~ "Restrictions on gatherings > 100 people",
          `C4_Restrictions on gatherings` == 3 ~ "Restrictions on gatherings > 10 people",
          `C4_Restrictions on gatherings` == 4 ~ "Restrictions on gatherings < 10 people"),
        `rec_C5_Close public transport` = case_when(
          `C5_Close public transport` == 0 ~ "No measures",
          `C5_Close public transport` == 1 ~ "Recommend closing or significantly reduce available transport",
          `C5_Close public transport` == 2 ~ "Require closing or restrict use for most citizens"),
        `rec_C6_Stay at home requirements` = case_when(
          `C6_Stay at home requirements` == 0 ~ "No measures",
          `C6_Stay at home requirements` == 1 ~ "Recommend not leaving house",
          `C6_Stay at home requirements` == 2 ~ "Require not leaving house except for exercise and essential trips",
          `C6_Stay at home requirements` == 3 ~ "Require not leaving house with minimal exceptions"),
        `rec_C7_Restrictions on internal movement` = case_when(
          `C7_Restrictions on internal movement` == 0 ~ "No measures",
          `C7_Restrictions on internal movement` == 1 ~ "Recommend closing or significantly reduce available transport",
          `C7_Restrictions on internal movement` == 2 ~ "Require closing or restrict use for most citizens"),
        `rec_C8_International travel controls` = case_when(
          `C8_International travel controls` == 0 ~ "No measures",
          `C8_International travel controls` == 1 ~ "Screening",
          `C8_International travel controls` == 2 ~ "Quarantine arrivals from high-risk regions",
          `C8_International travel controls` == 3 ~ "Ban on high-risk regions",
          `C8_International travel controls` == 4 ~ "Total border closure")
      )
    
    write.csv(dat, 'data/stringency_data.csv', row.names = FALSE)
    
  } else {
    dat <- read_csv('data/stringency_data.csv')
  }
  
  dat
}

#' @returns country codes and continents
get_country_codes <- function() {
  
  if (!file.exists('data/country_codes.csv')) {
    country_codes <- countrycode::codelist %>% 
      select(continent, CountryName = country.name.en, CountryCode = iso3c) %>%
      filter(!is.na(CountryCode))
    
    write.csv(country_codes, 'data/country_codes.csv', row.names = FALSE)
    
  } else {
    country_codes <- read.csv('data/country_codes.csv')
  }
  
  country_codes
  
}

#' Returns number of days since the intervention is in place
#' TODO: Think about how to handle NAs
#' 
#' @param indicator boolean vector indicating when the intervention was in place
#' @returns number of days since when the last intervention is in place. Negative values indicate the number of days since measure was lifted.
#' 
days_active <- function(indicator) {
  nr_days <- length(indicator)
  
  # indicator is boolean, so this yields 1 when FALSE switches to TRUE (or vice versa)
  last_change <- last(which(diff(indicator) == 1)) # last_change refers to implementation of measures
  lift_change <- last(which(diff(indicator) == -1)) # lift_change referes to lifting the measures
  if_else(is.na(lift_change), nr_days - last_change, -(nr_days - lift_change)) #return negative value if the measure is lifted
}

#' Calculates the rollback readiness for all countries
#' 
#' @param dat stringency data
#' @param countries selected countries
#' @returns data frame


rollback <- function(dat, countries) {
  d <-
    dat %>% filter(Country %in% countries) %>% group_by(Country) %>%
    filter(Date %in% (last(Date) - 7):last(Date)) %>%
    mutate(dgr = ifelse((lag(ConfirmedDailyCases) != 0),
                        (ConfirmedDailyCases - lag(ConfirmedDailyCases)) / lag(ConfirmedDailyCases),
                        0
    )) %>%
    summarise(
      dnc_rate = if_else(
        last(ConfirmedDailyCases) >= 50,
        0,
        0.5 - last(ConfirmedDailyCases) / 50
      ), # Ratio of daily cases / 50 scaled to 0 - 0.5
      dgr_down = if_else(last(dgr) < nth(dgr, 2), 0.5, 0), # Is daily growth rate less than week ago?
      trace = (last(`H2_Testing policy`) + last(`H3_Contact tracing`)) / 5,
      risk = last(`C8_International travel controls`) / 4,
      comm = (last(`H1_Public information campaigns`) + last(H1_Flag)) /
        3,
      roll = sum(c(dnc_rate, dgr_down, trace, risk, comm), na.rm = TRUE) /
        4
    ) %>% select(Country, roll)
  return(d)
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
    'Mandatory school closing',                # 2 is on some level, 3 is all levels closed
    'Mandatory workplace closing',             # 2 is required for some, 3 is required for all but non-essential
    'Mandatory cancellation of public events',       # 1 is recommended, 2 is required
    'Mandatory public transport closing',       # 1 is recommended, 2 is required
    'Gatherings restricted below 100 people',         # 0 are no restrictions, up to 4 in severity
    'Leaving home restricrted by law (with minimal exceptions)',                  # 2 and 3 are restrictions
    'Mandatory restrictions of internal transport',  # 1 recommended, 2 is required
    'Total border closure'  # 1 screening, 2 is quarantine, 3 is ban on high-risk regions, 4 is border closures
  )
  
  d <- dat %>% 
    filter(Country %in% countries) %>% 
    
    mutate(
      # Closure and Containment Measures
      C1_schools_closed = `C1_School closing` > 2,
      C2_workplace_closed = `C2_Workplace closing` > 2,
      C3_public_events_cancelled = `C3_Cancel public events` > 1,
      C4_gatherings_restricted = `C4_Restrictions on gatherings` > 2,
      C5_public_transport = `C5_Close public transport` > 1,
      C6_stay_home = `C6_Stay at home requirements` > 2,
      C7_internal_movement = `C7_Restrictions on internal movement` > 1,
      C8_international_travel = `C8_International travel controls` > 3,
      
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
      C8_days = days_active(C8_international_travel),
      C1_Flag = if_else(C1_days > 0,last(C1_Flag),0),
      C2_Flag = if_else(C2_days > 0,last(C2_Flag),0),
      C3_Flag = if_else(C3_days > 0,last(C3_Flag),0),
      C4_Flag = if_else(C4_days > 0,last(C4_Flag),0),
      C5_Flag = if_else(C5_days > 0,last(C5_Flag),0),
      C6_Flag = if_else(C6_days > 0,last(C6_Flag),0),
      C7_Flag = if_else(C7_days > 0,last(C7_Flag),0),
    )
  
  colnames(d)[1:9] <- cnames
  r <- rollback(dat,countries)
  d <- left_join(d,r)
  return(d)

}

#' Returns breakpoints for discrete color map fill
Z_Breaks = function(n){
  CUTS = seq(0,1,length.out=n+1)
  rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
}

#' Returns ggplot of map filled according to variable
#' TODO: Change this to a fast plotly implementation (instead of calling ggplotly on the result)
#' TODO: Scale Deaths and Cases differently for sound visualisation
#' 
#' 
#' @param world world data
#' @param sdat stringency data
#' @param date date for the stringency index
#' @param variable variable which should be shown
#' @param measure type of measure that should be shown if StringencyIndex is selected as variable
#' @returns ggplot object
plot_world_data <- function(dat, date, variable, measure, continent) {
  #d <- left_join(world, filter(dat, Date == date), by = c('region' = 'Country'))
  d <- dat %>%
    filter(!(CountryName %in% c("San Marino", "Andorra", "Monaco"))) %>%
    group_by(CountryCode) %>%
    filter(Date == date | Date == max(dat$Date[which(dat$Date < date)]))

  if (variable == 'StringencyIndex') {
    legend_title <- 'Stringency Index'
    if (measure == "Combined") {
      title <- 'Stringency of Lockdown Across the World'
      d$variable <- d$StringencyIndexForDisplay
      limits <- c(0,100)
      colorscale <- "Reds"
      colorbar <- list(title = legend_title, limits = limits, y = 0.75, tickvals = seq(0, 100, 20))
    } else if(measure == "School") {
      title <- "Stringency of School Closing around the World"
      d$variable <- d$`C1_School closing`
      tags <- dat$`rec_C1_School closing` %>% factor() %>% fct_reorder(dat$`C1_School closing`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "Workplace") {
      title <- "Stringency of Workplace Closing around the World"
      d$variable <- d$`C2_Workplace closing`
      tags <- dat$`rec_C2_Workplace closing` %>% factor() %>% fct_reorder(dat$`C2_Workplace closing`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "PublicEvents") {
      title <- "Stringency in Cancelling Public Events around the World"
      d$variable <- d$`C3_Cancel public events`
      tags <- dat$`rec_C3_Cancel public events` %>% factor() %>% fct_reorder(dat$`C3_Cancel public events`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "Gatherings") {
      title <- "Stringency in Restricting Gatherings around the World"
      d$variable <- d$`C4_Restrictions on gatherings`
      tags <- dat$`rec_C4_Restrictions on gatherings` %>% factor() %>% fct_reorder(dat$`C4_Restrictions on gatherings`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "Transport") {
      title <- "Stringency in Closing Public Transport around the World"
      d$variable <- d$`C5_Close public transport`
      tags <- dat$`rec_C5_Close public transport` %>% factor() %>% fct_reorder(dat$`C5_Close public transport`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "Home") {
      title <- "Stringency of Stay at Home Requirements around the World"
      d$variable <- d$`C6_Stay at home requirements`
      tags <- dat$`rec_C6_Stay at home requirements` %>% factor() %>% fct_reorder(dat$`C6_Stay at home requirements`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "Movement") {
      title <- "Stringency of Internal Movement Restrictions around the World"
      d$variable <- d$`C7_Restrictions on internal movement`
      tags <- dat$`rec_C7_Restrictions on internal movement` %>% factor() %>% fct_reorder(dat$`C7_Restrictions on internal movement`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } else if(measure == "Travel") {
      title <- "Stringency of International Travel Controls around the World"
      d$variable <- d$`C8_International travel controls`
      tags <- dat$`rec_C8_International travel controls` %>% factor() %>% fct_reorder(dat$`C8_International travel controls`) %>% levels()
      nFactor <- length(tags)
      colours <- brewer.pal(n = nFactor,name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "Measures", y = 0.75, tickvals=0:nFactor, ticktext=names(colours))
    } 
    
  } else if (variable == 'Deaths') {
    
    title <- 'Confirmed Deaths per Million Across the World'
    legend_title <- 'Deaths per Million'
    d$variable <- d$DeathsPerMillion
    limits = c(0, ceiling(quantile(dat$DeathsPerMillion, .99, na.rm = TRUE)*100)/100)
    colorscale <- "Reds"
    colorbar <- list(title = legend_title, limits = limits, y = 0.75)
    
  } else if (variable == 'Cases') {
    
    title <- 'Confirmed Cases per Million Across the World'
    legend_title <- 'Cases per Million'
    d$variable <- d$CasesPerMillion
    limits <- c(0, ceiling(quantile(dat$CasesPerMillion, .99, na.rm = TRUE)*1000)/1000)
    colorscale <- "Reds"
    colorbar <- list(title = legend_title, limits = limits, y = 0.75)
    
  }
  
  if (continent == "World") {
    
    lataxis <- list(range = (c(-60, 90)))
    lonaxis <- list(range = (c(-180, 180)))
    
  } else if (continent == "Europe"){
    
    lataxis <- list(range = (c(30, 90)))
    lonaxis <- list(range = (c(-30, 50)))
    
  }else if (continent == "NorthAmerica"){
    
    lataxis <- list(range = (c(10, 90)))
    lonaxis <- list(range = (c(-180, -30)))
    
  } else if (continent == "SouthAmerica"){
    
    lataxis <- list(range = (c(-60, 15)))
    lonaxis <- list(range = (c(-105, -30)))
    
  } else if (continent == "Asia"){
    
    lataxis <- list(range = (c(-20, 90)))
    lonaxis <- list(range = (c(30, 180)))
    
  } else if (continent == "Africa"){
    
    lataxis <- list(range = (c(-40, 40)))
    lonaxis <- list(range = (c(-20, 55)))
    
  } else if (continent == "Oceania"){
    
    lataxis <- list(range = (c(-60, 0)))
    lonaxis <- list(range = (c(90, 180)))
    
  }
  
  
  plot_ly(data = d, type = "choropleth", locations = d$CountryCode, z = d$variable, stroke = I("black"), span = I(1),
          text = paste0(d$CountryName), colorscale = colorscale, colorbar = colorbar) %>%
    layout(title = list(text = title), geo = list(showcountries = TRUE, lataxis = lataxis, lonaxis = lonaxis))
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
