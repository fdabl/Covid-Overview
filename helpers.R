library('httr')
library('lemon')
library('readxl')
library('plotly')
library('stringi')
library('COVID19')
library('forecast')
library('jsonlite')
library('tidyverse')
library('lubridate')
library('covid19us')
library('colorspace')
library('countrycode')
library('RColorBrewer')
#library('dashboardthemes')


#' Returns moving-averaged data
smooth <- function(x, order = 7) {
  n <- length(x)
  y <- rep(NA, n)
  
  for (i in seq(n)) {
    sel <- seq(i - order, i)
    sel <- sel[sel > 0]
    y[i] <- mean(x[sel], na.rm = TRUE)
  }
  
  y
}


#' Gets the Stringency Index and Deaths / Confirmed using COVID19
#' 
#' @returns data frame with Stringency information
get_stringency_data <- function(...) {
  
  school_labels <- list(
    '0' = 'None', '1' = 'Recommended',
    '2' = 'Required on some levels', '3' = 'Required'
  )
  
  workplace_labels <- list(
    '0' = 'None', '1' = 'Recommended',
    '2' = 'Required for some sectors',
    '3' = 'Required, except essential workplaces'
  )
  
  events_labels <- list(
    '0' = 'None', '1' = 'Recommended', '2' = 'Required'
  )
  
  gatherings_labels <- list(
    '0' = 'None', '1' = '> 1000 people',
    '2' = '> 100 people', '3' = '> 10 people', '4' = '< 10 people'
  )
  
  transport_labels <- list(
    '0' = 'None',
    '1' = 'Recommended or reduced availability',
    '2' = 'Required or severely restricted'
  )
  
  stay_home_labels <- list(
    '0' = 'None', '1' = 'Recommended',
    '2' = 'Required with some exceptions',
    '3' = 'Required with minimal exceptions'
  )
  
  internal_movement_labels <- list(
    '0' = 'None',
    '1' = 'Recommended or reduced availability',
    '2' = 'Required or severely restricted'
  )
  
  international_movement_labels <- list(
    '0' = 'None',
    '1' = 'Screening',
    '2' = 'Ban on high-risk regions',
    '3' = 'Quarantine arrivals from high-risk regions',
    '4' = 'Total border closure'
  )
  
  relabel <- function(xint, labels) unlist(labels[as.character(xint)])
  
  dat <- covid19(...) %>% 
    group_by(id) %>% 
    mutate(
      # Tests, Cases, and Deaths
      tests_per_million = (tests / population) * 1e6,
      cases_per_million = (confirmed / population) * 1e6,
      deaths_per_million = (deaths / population) * 1e6,
      
      tests_per_million_s = round(smooth(tests_per_million, order = 7), 2),
      cases_per_million_s = round(smooth(cases_per_million, order = 7), 2),
      deaths_per_million_s = round(smooth(deaths_per_million, order = 7), 2),
      
      tests_per_case = tests / confirmed,
      tests_per_case_s = round(smooth(tests_per_case, order = 7), 2),
      
      daily_tests = c(0, diff(tests)),
      daily_cases = c(0, diff(confirmed)),
      daily_deaths = c(0, diff(deaths)),
      daily_tests_per_case = c(0, diff(tests_per_case)),
      new_daily_tests_per_case_s = round(smooth(daily_tests_per_case, order = 7), 2),
      
      new_tests_per_million = (daily_tests / population) * 1e6,
      new_cases_per_million = (daily_cases / population) * 1e6,
      new_deaths_per_million = (daily_deaths / population) * 1e6,
      
      new_tests_per_million_s = round(smooth(new_tests_per_million, order = 7), 2),
      new_cases_per_million_s = round(smooth(new_cases_per_million, order = 7), 2),
      new_deaths_per_million_s = round(smooth(new_deaths_per_million, order = 7), 2),
      
      # Check whether that grouping is adequate!
      group_tests_per_case = case_when(
        tests_per_case_s >= 0 & tests_per_case_s <= 5 ~ 1,
        tests_per_case_s > 5 & tests_per_case_s <= 10 ~ 2,
        tests_per_case_s > 10 & tests_per_case_s <= 20 ~ 3,
        tests_per_case_s > 20 & tests_per_case_s <= 50 ~ 4,
        tests_per_case_s > 50 & tests_per_case_s <= 100 ~ 5,
        tests_per_case_s > 100 & tests_per_case_s <= 1000 ~ 5,
        tests_per_case_s > 1000 ~ 6
      ),
      
      group_deaths_per_million = case_when(
        deaths_per_million_s >= 0 & deaths_per_million_s <= 1 ~ 0,
        deaths_per_million_s > 1 & deaths_per_million_s <= 10 ~ 1,
        deaths_per_million_s > 10 & deaths_per_million_s <= 100 ~ 2,
        deaths_per_million_s > 100 & deaths_per_million_s <= 1000 ~ 3,
        deaths_per_million_s > 1000 ~ 4
      ),
      
      group_cases_per_million = case_when(
        cases_per_million_s >= 0 & cases_per_million_s <= 1 ~ 0,
        cases_per_million_s > 1 & cases_per_million_s <= 10 ~ 1,
        cases_per_million_s > 10 & cases_per_million_s <= 100 ~ 2,
        cases_per_million_s > 100 & cases_per_million_s <= 1000 ~ 3,
        cases_per_million_s > 1000 & cases_per_million_s <= 10000 ~ 4,
        cases_per_million_s > 10000 ~ 5
      ),
      
      # Stringency Data
      school_closing_labels = relabel(school_closing, school_labels),
      workplace_closing_labels = relabel(workplace_closing, workplace_labels),
      cancel_events_labels = relabel(cancel_events, events_labels),
      gatherings_restrictions_labels = relabel(gatherings_restrictions, gatherings_labels),
      transport_closing_labels = relabel(transport_closing, transport_labels),
      stay_home_restrictions_labels = relabel(stay_home_restrictions, stay_home_labels),
      internal_movement_restrictions_labels = relabel(internal_movement_restrictions, internal_movement_labels),
      international_movement_restrictions_labels = relabel(international_movement_restrictions, international_movement_labels)
    ) %>% 
    rename(
      country_code = id
    )
}


#' Get data from US states
get_us_data <- function() {
  dat <- covid19(country = 'USA', level = 2)
  
  dat <- dat %>% 
    rename(
      state_name = administrative_area_level_2,
      state = key_alpha_2
    ) %>%
    mutate(
      tests_per_million = (tests / population) * 1e6,
      cases_per_million = (confirmed / population) * 1e6,
      deaths_per_million = (deaths / population) * 1e6,
      
      tests_per_million_s = round(smooth(tests_per_million, order = 7), 2),
      cases_per_million_s = round(smooth(cases_per_million, order = 7), 2),
      deaths_per_million_s = round(smooth(deaths_per_million, order = 7), 2),
      
      tests_per_case = tests / confirmed,
      tests_per_case_s = round(smooth(tests_per_case, order = 7), 2),
      
      # Check whether that grouping is adequate!
      group_tests_per_case = case_when(
        tests_per_case_s >= 0 & tests_per_case_s <= 5 ~ 1,
        tests_per_case_s > 5 & tests_per_case_s <= 10 ~ 2,
        tests_per_case_s > 10 & tests_per_case_s <= 20 ~ 3,
        tests_per_case_s > 20 & tests_per_case_s <= 50 ~ 4,
        tests_per_case_s > 50 & tests_per_case_s <= 100 ~ 5,
        tests_per_case_s > 100 & tests_per_case_s <= 1000 ~ 5,
        tests_per_case_s > 1000 ~ 6
      ),
      
      group_deaths_per_million = case_when(
        deaths_per_million_s >= 0 & deaths_per_million_s <= 1 ~ 0,
        deaths_per_million_s > 1 & deaths_per_million_s <= 10 ~ 1,
        deaths_per_million_s > 10 & deaths_per_million_s <= 100 ~ 2,
        deaths_per_million_s > 100 & deaths_per_million_s <= 1000 ~ 3,
        deaths_per_million_s > 1000 ~ 4
      ),
      
      group_cases_per_million = case_when(
        cases_per_million_s >= 0 & cases_per_million_s <= 1 ~ 0,
        cases_per_million_s > 1 & cases_per_million_s <= 10 ~ 1,
        cases_per_million_s > 10 & cases_per_million_s <= 100 ~ 2,
        cases_per_million_s > 100 & cases_per_million_s <= 1000 ~ 3,
        cases_per_million_s > 1000 & cases_per_million_s <= 10000 ~ 4,
        cases_per_million_s > 10000 ~ 5
      ),
    )
  
  dat
}


#' @returns country codes and continents
get_country_codes <- function() {
  
  if (!file.exists('data/country_codes.csv')) {
    country_codes <- countrycode::codelist %>% 
      select(continent, country_name = country.name.en, country_code = iso3c) %>%
      filter(!is.na(country_code))
    
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
#' @returns number of days since when the last intervention is in place. Negative values
#' indicate the number of days since measure was lifted.
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
    dat %>% filter(country_name %in% countries) %>% group_by(country_name) %>%
    filter(date %in% (last(date) - 7):last(date)) %>%
    mutate(
      dgr = ifelse((lag(daily_cases) != 0), (daily_cases - lag(daily_cases)) / lag(daily_cases), 0)
    ) %>%
    summarise(
      dnc_rate = if_else(
        last(daily_cases) >= 50,
        0,
        0.5 - last(daily_cases) / 50
      ), # Ratio of daily cases / 50 scaled to 0 - 0.5
      dgr_down = if_else(last(dgr) < nth(dgr, 2), 0.5, 0), # Is daily growth rate less than week ago?
      trace = (last(testing_policy) + last(contact_tracing)) / 5,
      risk = last(international_movement_restrictions) / 4,
      comm = (last(information_campaigns)) / 2,
      roll = sum(c(dnc_rate, dgr_down, trace, risk, comm), na.rm = TRUE) / 4
    ) %>% select(country_name, roll)
  
  d
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
    'Leaving home restricted by law (with minimal exceptions)',                  # 2 and 3 are restrictions
    'Mandatory restrictions of internal transport',  # 1 recommended, 2 is required
    'Total border closure'  # 1 screening, 2 is quarantine, 3 is ban on high-risk regions, 4 is border closures
  )
  
  d <- dat %>% 
    filter(country_name %in% countries) %>% 
    
    mutate(
      # Closure and Containment Measures
      school_closing = school_closing > 2,
      workplace_closing = workplace_closing > 2,
      cancel_events = cancel_events > 1,
      gatherings_restrictions = gatherings_restrictions > 2,
      transport_closing = transport_closing > 1,
      stay_home_restrictions = stay_home_restrictions > 2,
      internal_movement_restrictions = internal_movement_restrictions > 1,
      international_movement_restrictions = international_movement_restrictions > 3,
      
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
    
    group_by(country_name) %>% 
    summarize(
      C1_days = as.double(days_active(school_closing)),
      C2_days = as.double(days_active(workplace_closing)),
      C3_days = as.double(days_active(cancel_events)),
      C4_days = as.double(days_active(gatherings_restrictions)),
      C5_days = as.double(days_active(transport_closing)),
      C6_days = as.double(days_active(stay_home_restrictions)),
      C7_days = as.double(days_active(internal_movement_restrictions)),
      C8_days = as.double(days_active(international_movement_restrictions))
      # C1_Flag = if_else(C1_days > 0,last(C1_Flag),0),
      # C2_Flag = if_else(C2_days > 0,last(C2_Flag),0),
      # C3_Flag = if_else(C3_days > 0,last(C3_Flag),0),
      # C4_Flag = if_else(C4_days > 0,last(C4_Flag),0),
      # C5_Flag = if_else(C5_days > 0,last(C5_Flag),0),
      # C6_Flag = if_else(C6_days > 0,last(C6_Flag),0),
      # C7_Flag = if_else(C7_days > 0,last(C7_Flag),0),
    )
  
  r <- rollback(dat, countries)
  d <- left_join(d, r)
  d <- d %>% mutate(nas = rowSums(is.na(.))) %>% mutate(roll = if_else(nas > 7, NA_real_,d$roll)) %>%
    mutate(C1_days = if_else(nas < 8, ifelse(is.na(d$C1_days),0,d$C1_days),NA_real_),
           C3_days = if_else(nas < 8, ifelse(is.na(d$C3_days),0,d$C3_days),NA_real_),
           C2_days = if_else(nas < 8, ifelse(is.na(d$C2_days),0,d$C2_days),NA_real_),
           C4_days = if_else(nas < 8, ifelse(is.na(d$C4_days),0,d$C4_days),NA_real_),
           C5_days = if_else(nas < 8, ifelse(is.na(d$C5_days),0,d$C5_days),NA_real_),
           C6_days = if_else(nas < 8, ifelse(is.na(d$C6_days),0,d$C6_days),NA_real_),
           C7_days = if_else(nas < 8, ifelse(is.na(d$C7_days),0,d$C7_days),NA_real_),
           C8_days = if_else(nas < 8, ifelse(is.na(d$C8_days),0,d$C8_days),NA_real_)) %>%
    select(-nas)
  colnames(d)[1:9] <- cnames
  d
}


#' Returns breakpoints for discrete color map fill
Z_Breaks <- function(n) {
  CUTS = seq(0, 1, length.out = n+1)
  rep(CUTS, ifelse(CUTS %in% 0:1, 1, 2))
}


#' Sets tick position for discrete color map fill
tickpos <- function(nFactor) {
  pos <- unique((head(Z_Breaks(nFactor), -1)) + head(Z_Breaks(nFactor))[2]/2)*(nFactor-1)
}


#' breaks tick labels into lines, each line being width characters long
ticklab <- function(tags, width = 30) {
  x <- gsub(paste0('(.{1,', width, '})(\\s|$)'), '\\1\n', tags) %>%
    str_split('\n')
  for(i in 1:length(x)) {
    x[[i]] <- stri_remove_empty(x[[i]])
    x[[i]] <- gsub('\\s', ' ', format(x[[i]], width = width)) %>%
      paste(collapse = '\n')
  }
  x <- unlist(x)
  x <- gsub(' ', '\U2000', x, fixed = TRUE)
  
  x
}


#' @param world world data
#' @param dat stringency data
#' @param selected_date date for the stringency index
#' @param variable variable which should be shown
#' @param measure type of measure that should be shown if StringencyIndex is selected as variable
#' @param region region which should be shown
#' @returns plotly object
plot_world_data <- function(dat, selected_date, variable, measure, region, us_data = NULL, lataxis = NULL, lonaxis = NULL) {
  d <- dat %>%
    group_by(country_code) %>%
    filter(date == selected_date | date == max(dat$date[which(dat$date < selected_date)]))
  
  if (region == 'OECD') {
    
    d <- filter(d, country_name %in% c('Australia', 'Austria', 'Belgium', 'Canada', 'Chile', 'Colombia',
                                      'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 
                                      'Germany','Greece', 'Hungary', 'Iceland', 'Ireland', 'Israel',
                                      'Italy', 'Japan', 'South Korea', 'Latvia','Lithuania', 
                                      'Luxembourg', 'Mexico', 'Netherlands', 'New Zealand', 'Norway',
                                      'Poland', 'Portugal', 'Slovak Republic', 'Slovenia','Spain', 
                                      'Sweden', 'Switzerland', 'Turkey', 'United Kingdom', 'United States'))
    scope <- 'world'
    lataxis <- list(range = (c(-60, 90))) 
    lonaxis <- list(range = (c(-180, 180)))
    
  }
  
  locations <- d$country_code
  locationmode <- NULL
  
  if (region == 'USA') {
    
    d <- us_data %>%
      group_by(state) %>%
      filter(date == selected_date | date == max(us_data$date[which(us_data$date < selected_date)]))
    scope <- 'usa'
    locationmode <- 'USA-states'
    locations <- d$state
    
  }
  
  hoverinfo <- 'text'
  tickfont <- list(family = 'Droid Sans Mono')
  zmin <- 0
  
  if (variable == 'StringencyIndex') {
    
    if (measure == 'Combined') {
      
      title <- 'Stringency Index Across the World'
      legend_title <- 'Stringency Index'
      text <- paste(d$stringency_index, d$country_name, sep = '\n')
      d$variable <- d$stringency_index
      zmax <- 100
      tags <- as.character(seq(0, 100, 20))
      tags <- gsub(' ', '\U2000', tags, fixed = TRUE)
      tags <- ticklab(tags)
      colorscale <- 'Reds'
      colorbar <- list(title = legend_title, y = 0.75, tickvals = seq(0, 100, 20), ticktext = tags, tickfont = tickfont)
      
    } else if (measure == 'School') {
      
      title <- 'Stringency of School Closing around the World'
      legend_title <- 'School closing'
      text <- paste(d$school_closing_labels, d$country_name, sep = '\n')
      d$variable <- d$school_closing
      tags <- dat$school_closing_labels %>% factor() %>% fct_reorder(dat$school_closing) %>% levels()

    } else if (measure == 'Workplace') {
      
      title <- 'Stringency of Workplace Closing around the World'
      legend_title <- 'Workplace closing'
      text <- paste(d$workplace_closing_labels, d$country_name, sep = '\n')
      d$variable <- d$workplace_closing
      tags <- dat$workplace_closing_labels %>% factor() %>% fct_reorder(dat$workplace_closing) %>% levels()

    } else if (measure == 'PublicEvents') {
      
      title <- 'Stringency in Cancelling Public Events around the World'
      legend_title <- 'Cancellation of public events'
      text <- paste(d$cancel_events_labels, d$country_name, sep = '\n')
      d$variable <- d$cancel_events
      tags <- dat$cancel_events_labels %>% factor() %>% fct_reorder(dat$cancel_events) %>% levels()

    } else if (measure == 'Gatherings') {
      
      title <- 'Stringency in Restricting Gatherings around the World'
      legend_title <- 'Restrictions on gatherings'
      text <- paste(d$gatherings_restrictions_labels, d$country_name, sep = '\n')
      d$variable <- d$gatherings_restrictions
      tags <- dat$gatherings_restrictions_labels %>% factor() %>% fct_reorder(dat$gatherings_restrictions) %>% levels()

    } else if (measure == 'Transport') {
      
      title <- 'Stringency in Closing Public Transport around the World'
      legend_title <- 'Closing of public transport'
      text <- paste(d$transport_closing_labels, d$country_name, sep = '\n')
      d$variable <- d$transport_closing
      tags <- dat$transport_closing_labels %>% factor() %>% fct_reorder(dat$transport_closing) %>% levels()

    } else if (measure == 'Home') {
      
      title <- 'Stringency of Stay at Home Requirements around the World'
      legend_title <- 'Stay at home requirements'
      text <- paste(d$stay_home_restrictions_labels, d$country_name, sep = '\n')
      d$variable <- d$stay_home_restrictions
      tags <- dat$stay_home_restrictions_labels %>% factor() %>% fct_reorder(dat$stay_home_restrictions) %>% levels()

    } else if (measure == 'Movement') {
      
      title <- 'Stringency of Internal Movement Restrictions around the World'
      legend_title <- 'Restrictions on internal movement'
      text <- paste(d$internal_movement_restrictions_labels, d$country_name, sep = '\n')
      d$variable <- d$internal_movement_restrictions
      tags <- dat$internal_movement_restrictions_labels %>% factor() %>% fct_reorder(dat$internal_movement_restrictions) %>% levels()

    } else if (measure == 'Travel') {
      
      title <- 'Stringency of International Travel Controls around the World'
      legend_title <- 'International travel controls'
      text <- paste(d$international_movement_restrictions_labels, d$country_name, sep = '\n')
      d$variable <- d$international_movement_restrictions
      tags <- dat$international_movement_restrictions_labels %>% factor() %>% fct_reorder(dat$international_movement_restrictions) %>% levels()

    } 
    
  } else if (variable == 'Deaths') {
    
    legend_title <- 'Total Deaths per Million'
    
    if (region != 'USA') {
      
      title <- 'Total Deaths per Million Across the World'
      text <- paste(d$deaths_per_million_s, d$country_name, sep = '\n')
      
    } else {
      
      title <- 'Total Deaths per Million in the USA'
      text <- paste(d$deaths_per_million_s, d$state_name, sep = '\n')
      
    }
    
    d$variable <- d$group_deaths_per_million
    tags <- c('0', '1', '10', '100', '1000')
    
  } else if (variable == 'Cases') {
    
    legend_title <- 'Total Cases per Million'
    
    if (region != 'USA') {
      
      title <- 'Total Confirmed Cases per Million Across the World'
      text <- paste(d$cases_per_million_s, d$country_name, sep = '\n')
      
    } else {
      
      title <- 'Total Confirmed Cases per Million in the USA'
      text <- paste(d$cases_per_million_s, d$state_name, sep = '\n')
      
    }
    
    d$variable <- d$group_cases_per_million
    tags <- c('0', '1', '10', '100', '1000', '10000')
    
    
  } else if (variable == 'Tests') {
    
    legend_title <- 'Tests per Confirmed Case'
    
    if (region != 'USA') {
      
      title <- 'Tests per Confirmed Case Across the World'
      text <- paste(d$tests_per_case_s, d$country_name, sep = '\n')
      
    } else {
      
      title <- 'Tests per Confirmed Case in the USA'
      text <- paste(d$tests_per_case_s, d$state_name, sep = '\n')
      
    }
    
    d$variable <- d$group_tests_per_case
    tags <- c('0', '5', '10', '20', '50', '100', '1000')
  }
  
  if (!(variable == 'StringencyIndex' & measure == 'Combined')) {
    
    tags <- ticklab(tags)
    nFactor <- length(tags)
    zmax <- nFactor-1
    colours <- brewer.pal(n = nFactor, name = 'Reds')
    names(colours) <- tags
    colorscale <- data.frame(z=Z_Breaks(nFactor),
                             col=rep(colours,each=2),stringsAsFactors=FALSE)
    colorbar <- list(title = legend_title, y = 0.75, tickvals = tickpos(nFactor), ticktext = tags, 
                     tickfont = tickfont)
    
  }
  
  if (region == 'World') {
    
    scope <- 'world'
    lataxis <- list(range = (c(-60, 90))) 
    lonaxis <- list(range = (c(-180, 180)))
    
  } else if (region == 'Europe') {
    
    scope <- 'europe'
    
  } else if (region == 'North America') {
    
    scope <- 'north america'
    
  } else if (region == 'South America') {
    
    scope <- 'south america'
    
  } else if (region == 'Asia') {
    
    scope <- 'asia'
    
  } else if (region == 'Africa') {
    
    scope <- 'africa'
    
  }
  
  plot_ly(data = d, type = 'choropleth', locations = locations, locationmode = locationmode, z = d$variable,
          zmin = zmin, zmax = zmax, stroke = I('black'), span = I(1),
          text = text, hoverinfo = hoverinfo, colorscale = colorscale, colorbar = colorbar) %>%
    
    layout(title = list(text = title), geo = list(showcountries = TRUE, scope = scope, lataxis = lataxis, 
                                                  lonaxis = lonaxis),
           dragmode = 'zoom', margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
    
    config(scrollZoom = FALSE)
}


#' Returns ggplot of stringency index across countries
#' 
#' @param dat stringency index data
#' @param countries list of countries to be shown
#' @param nr_cols number of columns for facet_wrap
#' 
#' @returns ggplot object
plot_stringency_data_deaths_relative <- function(dat, countries, nr_cols) {
  
  d <- filter(dat, country_name %in% countries)
  
  max_var <- max(d$new_deaths_per_million_s, na.rm = TRUE) 
  
  annotation_data <- d %>%
    filter(deaths != 0 | confirmed !=0) %>%
    mutate(date_for_reference = today()- days(3)) %>%
    filter(date == date_for_reference) %>%
    select(country_name,
           confirmed,
           deaths,
           population, daily_cases,
           daily_deaths, date_for_reference
    )
  
  ggplot(d, aes(x = date)) +
    geom_line(
      stat = 'identity',
      aes(y = new_deaths_per_million_s, color = 'New Deaths per Million'),
      size = 1
    ) +
    geom_line(aes(
      y = stringency_index /  (100 / max_var),
      color = 'Stringency Index')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~.*(100 / max_var),
                          name = 'Stringency Index')
    ) +
    facet_rep_wrap(~ country_name, ncol = nr_cols, repeat.tick.labels = 'bottom') + #
    ylab('New Deaths per Million') +
    ggtitle('Stringency of Measures and New Deaths per Million') +
    xlab('Date') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.ticks.y = element_blank(),
      strip.text.x = element_text(size = 16),
      panel.spacing.x = unit(-2, "lines"),
      panel.spacing.y = unit(0.75, "lines"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    ) +
    scale_colour_manual(
      name = '', values = c('Stringency Index' = 'black', 'New Deaths per Million' = '#E41A1C')
      #)
    ) +
    geom_text(
      data  = annotation_data,
      aes(x = as.Date("2020-01-10", "%Y-%m-%d"),
          y = max_var*0.86,
          label = paste0(
            'Total Deaths: ', deaths, '\n', 
            'New Deaths: ', daily_deaths
          )),
      hjust = 0, size = 3.5
    )
}



plot_stringency_data_cases_relative <- function(dat, countries, nr_cols) {
  
  d <- filter(dat, country_name %in% countries)
  
  max_var <- max(d$new_cases_per_million_s, na.rm = TRUE) 
  
  annotation_data <- d %>%
    filter(deaths != 0 | confirmed !=0) %>%
    mutate(date_for_reference = today()- days(3)) %>%
    filter(date == date_for_reference) %>%
    select(country_name,
           confirmed,
           deaths,
           population, daily_cases,
           daily_deaths, date_for_reference
    )
  ggplot(d, aes(x = date)) +
    geom_line(
      stat = 'identity',
      aes(y = new_cases_per_million_s, color = 'New Cases per Million'),
      size = 1
    ) +
    geom_line(aes(
      y = stringency_index / (100 / max_var),
      color = 'Stringency Index')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~.*(100 / max_var), name = 'Stringency Index'
      )
    ) +
    facet_rep_wrap(~ country_name, ncol = nr_cols, repeat.tick.labels = 'bottom') +
    ylab('New Cases per Million') +
    xlab('Date') +
    ggtitle('Stringency of Measures and New Cases per Million') +
    theme_minimal() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 18),
      axis.ticks.y = element_blank(),
      strip.text.x = element_text(size = 16),
      panel.spacing.x = unit(-2, "lines"),
      panel.spacing.y = unit(0.75, "lines"),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.75),
      legend.text = element_text(size = 12),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
    ) +
    scale_colour_manual(
      name = '', values = c('Stringency Index' = 'black', 'New Cases per Million' = '#E41A1C')
    ) +
    geom_text(
      data  = annotation_data,
      aes(x = as.Date("2020-01-10", "%Y-%m-%d"),
          y = max_var*0.86,
          label = paste0(
            'Total Cases: ', confirmed, '\n', 
            'New Cases: ', daily_cases
          )),
      hjust = 0, size = 3.5
    )
}


plot_stringency_data_tests <- function(dat, countries, nr_cols) {
  
  d <- filter(dat, country_name %in% countries)
  
  max_var <- max(d$new_daily_tests_per_case_s, na.rm = TRUE) 
  
  annotation_data <- d %>%
    filter(deaths != 0 | confirmed !=0) %>%
    mutate(date_for_reference = today()- days(3)) %>%
    filter(date == date_for_reference) %>%
    select(country_name,
           confirmed,
           deaths,
           tests,
           daily_tests,
           population, daily_cases,
           daily_deaths, date_for_reference
    )
  
  ggplot(d, aes(x = date)) +
    geom_line(
      stat = 'identity',
      aes(y = new_daily_tests_per_case_s, color = 'New Tests per Confirmed Case'),
      size = 1
    ) +
    geom_line(aes(
      y = stringency_index /  (100 / max_var),
      color = 'Stringency Index')
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(~.*(100 / max_var),
                          name = 'Stringency Index')
    ) +
    facet_rep_wrap(~ country_name, ncol = nr_cols, repeat.tick.labels = 'bottom') + #
    ylab('New Tests per Confirmed Case') +
    ggtitle('Stringency of Measures and New Tests per Confirmed Case') +
    xlab('Date') +
    theme_bw() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 16),
      axis.ticks.y = element_blank(),
      panel.spacing.x = unit(-1.5, "lines")
    ) +
    scale_colour_manual(
      name = '', values = c('Stringency Index' = 'black', 'New Tests per Confirmed Case' = '#E41A1C')
      #)
    ) +
    geom_text(
      data  = annotation_data,
      aes(x = as.Date("2020-01-10", "%Y-%m-%d"),
          y = max_var*0.86,
          label = paste0(
            'Total Tests: ', tests, '\n', 
            'New Tests: ', daily_tests
          )),
      hjust = 0, size = 3.5
    )
}
