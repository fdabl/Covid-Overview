library('httr')
library('readxl')
library('jsonlite')
library('tidyverse')
library('lubridate')
library("plotly")
library("countrycode")
library("RColorBrewer")
library("colorspace")
library("stringi")


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
        NewDeathsPerMillion = (ConfirmedDailyDeaths / Population) * 1e6,
        NewCasesPerMillionSmooth = (NewCasesPerMillion +
                                      lag(NewCasesPerMillion,1) +
                                      lag(NewCasesPerMillion,3) +
                                      lag(NewCasesPerMillion,4) +
                                      lag(NewCasesPerMillion,5) +
                                      lag(NewCasesPerMillion,6) +
                                      lag(NewCasesPerMillion,7) +
                                      lag(NewCasesPerMillion,8) +
                                      lag(NewCasesPerMillion,9)) /10,
        NewDeathsPerMillionSmooth = (NewDeathsPerMillion +
                                       lag(NewDeathsPerMillion, 1)+
                                       lag(NewDeathsPerMillion, 2)+
                                       lag(NewDeathsPerMillion, 3)+
                                       lag(NewDeathsPerMillion, 4)+
                                       lag(NewDeathsPerMillion, 5)+
                                       lag(NewDeathsPerMillion, 6)+
                                       lag(NewDeathsPerMillion, 7)+
                                       lag(NewDeathsPerMillion, 8)+
                                       lag(NewDeathsPerMillion, 9))/10
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
        `C1_School closing` == 0 ~ "None",
        `C1_School closing` == 1 ~ "Recommended",
        `C1_School closing` == 2 ~ "Required on some levels",
        `C1_School closing` == 3 ~ "Required"),
        `rec_C2_Workplace closing` = factor(case_when(
          `C2_Workplace closing` == 0 ~ "None",
          `C2_Workplace closing` == 1 ~ "Recommended",
          `C2_Workplace closing` == 2 ~ "Required for some sectors",
          `C2_Workplace closing` == 3 ~ "Required, except essential workplaces")),
        `rec_C3_Cancel public events` = case_when(
          `C3_Cancel public events` == 0 ~ "None",
          `C3_Cancel public events` == 1 ~ "Recommended",
          `C3_Cancel public events` == 2 ~ "Required"),
        `rec_C4_Restrictions on gatherings` = case_when(
          `C4_Restrictions on gatherings` == 0 ~ "None",
          `C4_Restrictions on gatherings` == 1 ~ "> 1000 people",
          `C4_Restrictions on gatherings` == 2 ~ "> 100 people",
          `C4_Restrictions on gatherings` == 3 ~ "> 10 people",
          `C4_Restrictions on gatherings` == 4 ~ "< 10 people"),
        `rec_C5_Close public transport` = case_when(
          `C5_Close public transport` == 0 ~ "None",
          `C5_Close public transport` == 1 ~ "Recommended or reduced availability",
          `C5_Close public transport` == 2 ~ "Required or severely restricted"),
        `rec_C6_Stay at home requirements` = case_when(
          `C6_Stay at home requirements` == 0 ~ "None",
          `C6_Stay at home requirements` == 1 ~ "Recommended",
          `C6_Stay at home requirements` == 2 ~ "Required with some exceptions",
          `C6_Stay at home requirements` == 3 ~ "Required with minimal exceptions"),
        `rec_C7_Restrictions on internal movement` = case_when(
          `C7_Restrictions on internal movement` == 0 ~ "None",
          `C7_Restrictions on internal movement` == 1 ~ "Recommended or reduced availability",
          `C7_Restrictions on internal movement` == 2 ~ "Required or severely restricted"),
        `rec_C8_International travel controls` = case_when(
          `C8_International travel controls` == 0 ~ "No measures",
          `C8_International travel controls` == 1 ~ "Screening",
          `C8_International travel controls` == 2 ~ "Quarantine arrivals from high-risk regions",
          `C8_International travel controls` == 3 ~ "Ban on high-risk regions",
          `C8_International travel controls` == 4 ~ "Total border closure")
      ) %>%
      mutate(group_DeathsPerMillion = case_when(
        DeathsPerMillion >= 0 & DeathsPerMillion <= 1 ~ 0,
        DeathsPerMillion > 1 & DeathsPerMillion <= 10 ~ 1,
        DeathsPerMillion > 10 & DeathsPerMillion <= 100 ~ 2,
        DeathsPerMillion > 100 & DeathsPerMillion <= 1000 ~ 3,
        DeathsPerMillion > 1000 ~ 4)
      ) %>%
      mutate(group_CasesPerMillion = case_when(
        CasesPerMillion >= 0 & CasesPerMillion <= 1 ~ 0,
        CasesPerMillion > 1 & CasesPerMillion <= 10 ~ 1,
        CasesPerMillion > 10 & CasesPerMillion <= 100 ~ 2,
        CasesPerMillion > 100 & CasesPerMillion <= 1000 ~ 3,
        CasesPerMillion > 1000 & CasesPerMillion <= 10000 ~ 4,
        CasesPerMillion > 10000 ~ 5)
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

#' @returns US-specific data
get_us_data <- function() {
  
  if(!file.exists("data/us_data.csv")) {
    cases <- read.csv("data/cases_usa.csv")
    cases <- pivot_longer(cases, cols = starts_with("X"), 
                          names_to = "Date", values_to = "ConfirmedCases")
    cases$Date <- gsub("X", "", cases$Date)
    cases$Date <- gsub("\\.", "/", cases$Date)
    cases$Date <- mdy(cases$Date)
    cases <- cases %>%
      group_by(State, Date) %>%
      mutate(ConfirmedCases = sum(ConfirmedCases)) %>%
      select(State, Date, ConfirmedCases)
    
    deaths <- read.csv("data/deaths_usa.csv")
    deaths <- pivot_longer(deaths, cols = starts_with("X"), 
                          names_to = "Date", values_to = "ConfirmedDeaths")
    deaths$Date <- gsub("X", "", deaths$Date)
    deaths$Date <- gsub("\\.", "/", deaths$Date)
    deaths$Date <- mdy(deaths$Date)
    deaths <- deaths %>%
      group_by(State, Date) %>%
      mutate(ConfirmedDeaths = sum(ConfirmedDeaths)) %>%
      select(State, Date, ConfirmedDeaths)
    
    pop <- read.csv("data/population_usa.csv") %>%
      group_by(State) %>%
      mutate(population = sum(population)) %>%
      select(State, population)
    pop <- pop[!duplicated(pop),]
    
    us_data <- cases
    us_data$ConfirmedDeaths <- deaths$ConfirmedDeaths
    us_data <- full_join(us_data, pop, by = "State") %>%
      mutate(CasesPerMillion = round((ConfirmedCases / population) * 1e6, 2),
             DeathsPerMillion = round((ConfirmedDeaths / population) * 1e6, 2)) %>%
      mutate(group_DeathsPerMillion = case_when(
        DeathsPerMillion >= 0 & DeathsPerMillion <= 1 ~ 0,
        DeathsPerMillion > 1 & DeathsPerMillion <= 10 ~ 1,
        DeathsPerMillion > 10 & DeathsPerMillion <= 100 ~ 2,
        DeathsPerMillion > 100 & DeathsPerMillion <= 1000 ~ 3,
        DeathsPerMillion > 1000 ~ 4)
      ) %>%
      mutate(group_CasesPerMillion = case_when(
        CasesPerMillion >= 0 & CasesPerMillion <= 1 ~ 0,
        CasesPerMillion > 1 & CasesPerMillion <= 10 ~ 1,
        CasesPerMillion > 10 & CasesPerMillion <= 100 ~ 2,
        CasesPerMillion > 100 & CasesPerMillion <= 1000 ~ 3,
        CasesPerMillion > 1000 & CasesPerMillion <= 10000 ~ 4,
        CasesPerMillion > 10000 ~ 5)
      )
      
    
    write.csv(us_data, 'data/us_data.csv', row.names = FALSE)
    
  } else {
    
    us_data <- read.csv("data/us_data.csv")
    
  }
  
  us_data
  
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
Z_Breaks <- function(n) {
  CUTS = seq(0,1,length.out=n+1)
  rep(CUTS,ifelse(CUTS %in% 0:1,1,2))
}

#' Sets tick position for discrete color map fill
tickpos <- function(nFactor) {
  pos <- unique((head(Z_Breaks(nFactor), -1)) + head(Z_Breaks(nFactor))[2]/2)*(nFactor-1)
}

#' breaks tick labels into lines, each line being width characters long
ticklab <- function(tags, width = 25) {
  x <- gsub(paste0("(.{1,", width, "})(\\s|$)"), "\\1\n", tags) %>%
    str_split("\n")
  for(i in 1:length(x)) {
    x[[i]] <- stri_remove_empty(x[[i]])
    x[[i]] <- gsub("\\s", " ", format(x[[i]], width = width)) %>%
      paste(collapse = "\n")
  }
  x <- unlist(x)
  x <- gsub(" ", "\U2000", x, fixed = TRUE)
  return(x)
}

#' @param world world data
#' @param dat stringency data
#' @param date date for the stringency index
#' @param variable variable which should be shown
#' @param measure type of measure that should be shown if StringencyIndex is selected as variable
#' @param region region which should be shown
#' @returns plotly object
plot_world_data <- function(dat, date, variable, measure, region, us_data = NULL, lataxis = NULL, lonaxis = NULL) {
  d <- dat %>%
    group_by(CountryCode) %>%
    filter(Date == date | Date == max(dat$Date[which(dat$Date < date)]))
  
  if(region == "OECD") {
    
    d <- filter(d, CountryName %in% c("Australia", "Austria", "Belgium", "Canada", "Chile", "Colombia",
                                      "Czech Republic", "Denmark", "Estonia", "Finland", "France", 
                                      "Germany","Greece", "Hungary", "Iceland", "Ireland", "Israel",
                                      "Italy", "Japan", "South Korea", "Latvia","Lithuania", 
                                      "Luxembourg", "Mexico", "Netherlands", "New Zealand", "Norway",
                                      "Poland", "Portugal", "Slovak Republic", "Slovenia","Spain", 
                                      "Sweden", "Switzerland", "Turkey", "United Kingdom", "United States"))
    scope <- "world"
    lataxis <- list(range = (c(-60, 90))) 
    lonaxis <- list(range = (c(-180, 180)))
    
  }
  
  locations <- d$CountryCode
  locationmode <- NULL
  
  if (region == "USA") {
    us_data$Date <- as.Date(us_data$Date)
    d <- us_data %>%
      group_by(State) %>%
      filter(Date == date | Date == max(us_data$Date[which(us_data$Date < date)]))
    scope <- "usa"
    locationmode <- "USA-states"
    locations <- d$State
  }
  
  # lev <- c(unique(dat$`rec_C1_School closing`), unique(dat$`rec_C2_Workplace closing`), 
  #          unique(dat$`rec_C3_Cancel public events`), unique(dat$`rec_C4_Restrictions on gatherings`),
  #          unique(dat$`rec_C5_Close public transport`), unique(dat$`rec_C6_Stay at home requirements`),
  #          unique(dat$`rec_C7_Restrictions on internal movement`), unique(dat$`rec_C8_International travel controls`))
  # lev <- lev[!is.na(lev)]
  # lev <- gsub(" ", "\U2000", lev, fixed = TRUE)
  
  hoverinfo <- "text"
  tickfont <- list(family = "Droid Sans Mono")
  
  if (variable == 'StringencyIndex') {
    if (measure == "Combined") {
      title <- "Stringency of Lockdown Across the World"
      legend_title <- "Stringency Index"
      text <- paste(d$StringencyIndexForDisplay, d$CountryName, sep = "\n")
      d$variable <- d$StringencyIndexForDisplay
      zmin <- 0
      zmax <- 100
      tags <- as.character(seq(0, 100, 20))
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- ticklab(tags)
      #tags <- gsub("\\s", "\U2000", format(tags, width = max(nchar(lev, type = "width"), na.rm = TRUE)))
      colorscale <- "Reds"
      colorbar <- list(title = legend_title, y = 0.75, tickvals = seq(0, 100, 20), ticktext = tags, tickfont = tickfont)
    } else if(measure == "School") {
      title <- "Stringency of School Closing around the World"
      text <- paste(d$`rec_C1_School closing`, d$CountryName, sep = "\n")
      hoverinfo <- "text"
      d$variable <- d$`C1_School closing`
      tags <- dat$`rec_C1_School closing` %>% factor() %>% fct_reorder(dat$`C1_School closing`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor),  ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Workplace") {
      title <- "Stringency of Workplace Closing around the World"
      text <- paste(d$`rec_C2_Workplace closing`, d$CountryName, sep = "\n")
      d$variable <- d$`C2_Workplace closing`
      tags <- dat$`rec_C2_Workplace closing` %>% factor() %>% fct_reorder(dat$`C2_Workplace closing`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } else if(measure == "PublicEvents") {
      title <- "Stringency in Cancelling Public Events around the World"
      text <- paste(d$`rec_C3_Cancel public events`, d$CountryName, sep = "\n")
      d$variable <- d$`C3_Cancel public events`
      tags <- dat$`rec_C3_Cancel public events` %>% factor() %>% fct_reorder(dat$`C3_Cancel public events`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } else if(measure == "Gatherings") {
      title <- "Stringency in Restricting Gatherings around the World"
      text <- paste(d$`rec_C4_Restrictions on gatherings`, d$CountryName, sep = "\n")
      d$variable <- d$`C4_Restrictions on gatherings`
      tags <- dat$`rec_C4_Restrictions on gatherings` %>% factor() %>% fct_reorder(dat$`C4_Restrictions on gatherings`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } else if(measure == "Transport") {
      title <- "Stringency in Closing Public Transport around the World"
      text <- paste(d$`rec_C5_Close public transport`, d$CountryName, sep = "\n")
      d$variable <- d$`C5_Close public transport`
      tags <- dat$`rec_C5_Close public transport` %>% factor() %>% fct_reorder(dat$`C5_Close public transport`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } else if(measure == "Home") {
      title <- "Stringency of Stay at Home Requirements around the World"
      text <- paste(d$`rec_C6_Stay at home requirements`, d$CountryName, sep = "\n")
      d$variable <- d$`C6_Stay at home requirements`
      tags <- dat$`rec_C6_Stay at home requirements` %>% factor() %>% fct_reorder(dat$`C6_Stay at home requirements`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } else if(measure == "Movement") {
      title <- "Stringency of Internal Movement Restrictions around the World"
      text <- paste(d$`rec_C7_Restrictions on internal movement`, d$CountryName, sep = "\n")
      d$variable <- d$`C7_Restrictions on internal movement`
      tags <- dat$`rec_C7_Restrictions on internal movement` %>% factor() %>% fct_reorder(dat$`C7_Restrictions on internal movement`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } else if(measure == "Travel") {
      title <- "Stringency of International Travel Controls around the World"
      text <- paste(d$`rec_C8_International travel controls`, d$CountryName, sep = "\n")
      d$variable <- d$`C8_International travel controls`
      tags <- dat$`rec_C8_International travel controls` %>% factor() %>% fct_reorder(dat$`C8_International travel controls`) %>% levels()
      tags <- ticklab(tags)
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), tickfont = tickfont)
    } 
    
  } else if (variable == 'Deaths') {
    
    legend_title <- "Deaths per Million"
    if(region != "USA") {
      title <- "Confirmed Deaths per Million Across the World"
      text <- paste(d$DeathsPerMillion, d$CountryName, sep = "\n")
    } else {
      title <- "Confirmed Deaths per Million in the USA"
      text <- paste(d$DeathsPerMillion, d$State, sep = "\n")
    }
    d$variable <- d$group_DeathsPerMillion
    tags <- c("0", "> 1", "> 10", "> 100", "> 1000")
    tags <- ticklab(tags)
    nFactor <- length(tags)
    zmin <- 0
    zmax <- nFactor-1
    colours <- brewer.pal(n = nFactor, name = "Reds")
    names(colours) <- tags
    colorscale <- data.frame(z=Z_Breaks(nFactor),
                             col=rep(colours,each=2),stringsAsFactors=FALSE)
    colorbar <- list(title = legend_title, y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), 
                     tickfont = tickfont)
    
  } else if (variable == 'Cases') {
    
    legend_title <- "Cases per Million"
    if(region != "USA") {
      title <- "Confirmed Cases per Million Across the World"
      text <- paste(d$CasesPerMillion, d$CountryName, sep = "\n")
    } else {
      title <- "Confirmed Cases per Million in the USA"
      text <- paste(d$CasesPerMillion, d$State, sep = "\n")
    }
    d$variable <- d$group_CasesPerMillion
    tags <- c("0", "> 1", "> 10", "> 100", "> 1000", "> 10000")
    tags <- ticklab(tags)
    nFactor <- length(tags)
    zmin <- 0
    zmax <- nFactor-1
    colours <- brewer.pal(n = nFactor, name = "Reds")
    names(colours) <- tags
    colorscale <- data.frame(z=Z_Breaks(nFactor),
                             col=rep(colours,each=2),stringsAsFactors=FALSE)
    colorbar <- list(title = legend_title, y = 0.75, tickvals = tickpos(nFactor), ticktext = names(colours), 
                     tickfont = tickfont)
    
  }
  
  if (region == "World") {
    scope <- "world"
    lataxis <- list(range = (c(-60, 90))) 
    lonaxis <- list(range = (c(-180, 180)))
    
  } else if (region == "Europe") {
    scope <- "europe"
    
  }else if (region == "NorthAmerica") {
    scope <- "north america"
    
  } else if (region == "SouthAmerica") {
    scope <- "south america"
    
  } else if (region == "Asia") {
    scope <- "asia"
    
  } else if (region == "Africa") {
    scope <- "africa"
    
  }
  
  plot_ly(data = d, type = "choropleth", locations = locations, locationmode = locationmode, z = d$variable,
          zmin = zmin, zmax = zmax, stroke = I("black"), span = I(1),
          text = text, hoverinfo = hoverinfo, colorscale = colorscale, colorbar = colorbar) %>%
    layout(title = list(text = title), geo = list(showcountries = TRUE, scope = scope, lataxis = lataxis, 
                                                  lonaxis = lonaxis),
           dragmode = "zoom", uirevision = "date", margin = list(l = 0, r = 0, b = 0, t = 30)) %>% 
    config(scrollZoom = FALSE)
}


#' Returns ggplot of stringencyc across countries
#' 
#' @param dat stringency index data
#' @param countries list of countries to be shown
#' 
#' @returns ggplot object
plot_stringency_data_deaths_relative <- function(dat, countries, nr_cols) {
  
  d <- filter(dat, Country %in% countries)
  
  ggplot(d, aes(x = Date)) +
    geom_line(
      stat = 'identity',
      aes(y = NewDeathsPerMillionSmooth*10, color = 'New Deaths per 10 Million'),
      size = 1
    ) +
    geom_line(aes(y = StringencyIndexForDisplay / 
                    (10 / max(d$NewDeathsPerMillionSmooth, na.rm=T)),
                  color = 'Stringency Index')) +
    scale_y_continuous(
      sec.axis = sec_axis(~.*(10/max(d$NewDeathsPerMillionSmooth, na.rm=T)),
                          name = 'Stringency Index')
    ) +
    facet_rep_wrap(~ Country, ncol = nr_cols, 
                   repeat.tick.labels = 'TRUE') +
    ylab('New Deaths per 10 Million') +
    ggtitle('Stringency of Measures and New Deaths per 10 Million') +
    theme_bw() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    scale_colour_manual(name = ' ', 
                        values =c('Stringency Index' = 'black',
                                  'New Deaths per 10 Million' = '#E41A1C')
    )
}

plot_stringency_data_deaths_total<- function(dat, countries, nr_cols) {
  
  d <- filter(dat, Country %in% countries)
  
  ggplot(d, aes(x = Date)) +
    geom_line(
      #stat = 'identity',
      aes(y = ConfirmedDailyDeaths, color = 'New Deaths'),
      size = 1
    ) +
    geom_line(aes(y = StringencyIndexForDisplay / 
                    (100/max(d$ConfirmedDailyDeaths, na.rm=T)),
                  color = 'Stringency Index')) +
    #scale_colour_manual(values = c('#E41A1C', 'black')) +
    scale_y_continuous(
      # alx: removed the limits because they were the reason data wasn't displayed
      sec.axis = sec_axis(~.*(100/max(d$ConfirmedDailyDeaths, na.rm=T)),
                          name = 'Stringency Index')#, limits = c(0, 100)
    ) +
    facet_rep_wrap(~ Country, ncol = nr_cols, 
                   repeat.tick.labels = 'bottom') +
    ylab('New Deaths ') +
    ggtitle('Stringency of Measures and New Deaths (absolute value)') +
    theme_bw() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    scale_colour_manual(name = ' ', 
                        #breaks = c('black', 'red'),
                        values =c('Stringency Index' = 'black',
                                  'New Deaths' = '#E41A1C')
    )
}

plot_stringency_data_cases_relative <- function(dat, countries, nr_cols) {
  
  d <- filter(dat, Country %in% countries)
  
  ggplot(d, aes(x = Date)) +
    geom_line(
      stat = 'identity',
      aes(y = NewCasesPerMillionSmooth, color = 'New Cases per Million'),
      size = 1
    ) +
    geom_line(aes(y = StringencyIndexForDisplay / 
                    (100/max(d$NewCasesPerMillionSmooth, na.rm=T)),
                  color = 'Stringency Index')) +
    #scale_colour_manual(values = c('#E41A1C', 'black')) +
    scale_y_continuous(
      # alx: removed the limits because they were the reason data wasn't displayed
      sec.axis = sec_axis(~.*(100/max(d$NewCasesPerMillionSmooth, na.rm=T)),
                          name = 'Stringency Index')#, limits = c(0, 100)
    ) +
    facet_rep_wrap(~ Country, ncol = nr_cols, 
                   repeat.tick.labels = c('right', 'left')) +
    ylab('New Cases per  Million') +
    ggtitle('Stringency of Measures and New Cases per Million') +
    theme_bw() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    scale_colour_manual(name = ' ', 
                        #breaks = c('black', 'red'),
                        values =c('Stringency Index' = 'black',
                                  'New Cases per Million' = '#E41A1C')
    )
}

plot_stringency_data_cases_total<- function(dat, countries, nr_cols) {
  
  d <- filter(dat, Country %in% countries)
  
  ggplot(d, aes(x = Date)) +
    geom_line(
      stat = 'identity',
      aes(y = ConfirmedDailyCases, color = 'New Cases'),
      size = 1
    ) +
    geom_line(aes(y = StringencyIndexForDisplay / 
                    (100/max(d$ConfirmedDailyCases, na.rm=T)),
                  color = 'Stringency Index')) +
    #scale_colour_manual(values = c('#E41A1C', 'black')) +
    scale_y_continuous(
      # alx: removed the limits because they were the reason data wasn't displayed
      sec.axis = sec_axis(~.*(100/max(d$ConfirmedDailyCases, na.rm=T)),
                          name = 'Stringency Index')#, limits = c(0, 100)
    ) +
    facet_wrap(~ Country, ncol = nr_cols) +
    ylab('New Cases ') +
    ggtitle('Stringency of Measures and New Cases (absolute value)') +
    theme_bw() +
    theme(
      legend.position = 'top',
      plot.title = element_text(hjust = 0.5, size = 16)
    ) +
    scale_colour_manual(name = ' ', 
                        #breaks = c('black', 'red'),
                        values =c('Stringency Index' = 'black',
                                  'New Cases' = '#E41A1C')
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
