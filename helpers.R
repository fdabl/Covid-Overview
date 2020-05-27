library('httr')
library('readxl')
library('jsonlite')
library('tidyverse')
library('lubridate')
library("plotly")
library("countrycode")
library("RColorBrewer")


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
           `C2_Workplace closing`,
           `C3_Cancel public events`,
           `C4_Restrictions on gatherings`,
           `C5_Close public transport`,
           `C6_Stay at home requirements`,
           `C7_Restrictions on internal movement`,
           `C8_International travel controls`
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
      C8_days = days_active(C8_international_travel)
    )
  
  colnames(d) <- cnames
  return(d)
}


#' Returns breakpoints for discrete color map fill
Z_Breaks <- function(n){
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
plot_world_data <- function(dat, date, variable, measure, continent, lataxis = list(range = (c(-60, 90))), lonaxis = lonaxis <- list(range = (c(-180, 180)))) {
  d <- dat %>%
    group_by(CountryCode) %>%
    filter(Date == date | Date == max(dat$Date[which(dat$Date < date)]))
  
  lev <- c(unique(dat$`rec_C1_School closing`), unique(dat$`rec_C2_Workplace closing`), 
           unique(dat$`rec_C3_Cancel public events`), unique(dat$`rec_C4_Restrictions on gatherings`),
           unique(dat$`rec_C5_Close public transport`), unique(dat$`rec_C6_Stay at home requirements`),
           unique(dat$`rec_C7_Restrictions on internal movement`), unique(dat$`rec_C8_International travel controls`))
  lev <- lev[!is.na(lev)]
  lev <- gsub(" ", "\U2000", lev, fixed = TRUE)
  
  hoverinfo <- "text"
  tickfont <- list(family = "Droid Sans Mono")
  
  if (variable == 'StringencyIndex') {
    if (measure == "Combined") {
      title <- 'Stringency of Lockdown Across the World'
      legend_title <- 'Stringency Index'
      text <- paste(d$StringencyIndexForDisplay, d$CountryName, sep = "\n")
      d$variable <- d$StringencyIndexForDisplay
      zmin <- 0
      zmax <- 100
      tags <- as.character(seq(0, 100, 20))
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width")+1, na.rm = TRUE)))
      colorscale <- "Reds"
      colorbar <- list(title = legend_title, y = 0.75, tickvals = seq(0, 100, 20), ticktext = tags)
    } else if(measure == "School") {
      title <- "Stringency of School Closing around the World"
      text <- paste(d$`rec_C1_School closing`, d$CountryName, sep = "\n")
      hoverinfo <- "text"
      d$variable <- d$`C1_School closing`
      tags <- dat$`rec_C1_School closing` %>% factor() %>% fct_reorder(dat$`C1_School closing`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals = 0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Workplace") {
      title <- "Stringency of Workplace Closing around the World"
      text <- paste(d$`rec_C2_Workplace closing`, d$CountryName, sep = "\n")
      d$variable <- d$`C2_Workplace closing`
      tags <- dat$`rec_C2_Workplace closing` %>% factor() %>% fct_reorder(dat$`C2_Workplace closing`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "PublicEvents") {
      title <- "Stringency in Cancelling Public Events around the World"
      text <- paste(d$`rec_C3_Cancel public events`, d$CountryName, sep = "\n")
      d$variable <- d$`C3_Cancel public events`
      tags <- dat$`rec_C3_Cancel public events` %>% factor() %>% fct_reorder(dat$`C3_Cancel public events`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Gatherings") {
      title <- "Stringency in Restricting Gatherings around the World"
      text <- paste(d$`rec_C4_Restrictions on gatherings`, d$CountryName, sep = "\n")
      d$variable <- d$`C4_Restrictions on gatherings`
      tags <- dat$`rec_C4_Restrictions on gatherings` %>% factor() %>% fct_reorder(dat$`C4_Restrictions on gatherings`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Transport") {
      title <- "Stringency in Closing Public Transport around the World"
      text <- paste(d$`rec_C5_Close public transport`, d$CountryName, sep = "\n")
      d$variable <- d$`C5_Close public transport`
      tags <- dat$`rec_C5_Close public transport` %>% factor() %>% fct_reorder(dat$`C5_Close public transport`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Home") {
      title <- "Stringency of Stay at Home Requirements around the World"
      text <- paste(d$`rec_C6_Stay at home requirements`, d$CountryName, sep = "\n")
      d$variable <- d$`C6_Stay at home requirements`
      tags <- dat$`rec_C6_Stay at home requirements` %>% factor() %>% fct_reorder(dat$`C6_Stay at home requirements`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Movement") {
      title <- "Stringency of Internal Movement Restrictions around the World"
      text <- paste(d$`rec_C7_Restrictions on internal movement`, d$CountryName, sep = "\n")
      d$variable <- d$`C7_Restrictions on internal movement`
      tags <- dat$`rec_C7_Restrictions on internal movement` %>% factor() %>% fct_reorder(dat$`C7_Restrictions on internal movement`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } else if(measure == "Travel") {
      title <- "Stringency of International Travel Controls around the World"
      text <- paste(d$`rec_C8_International travel controls`, d$CountryName, sep = "\n")
      d$variable <- d$`C8_International travel controls`
      tags <- dat$`rec_C8_International travel controls` %>% factor() %>% fct_reorder(dat$`C8_International travel controls`) %>% levels()
      tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
      tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
      nFactor <- length(tags)
      zmin <- 0
      zmax <- nFactor-1
      colours <- brewer.pal(n = nFactor, name = "Reds")
      names(colours) <- tags
      colorscale <- data.frame(z=Z_Breaks(nFactor),
                               col=rep(colours,each=2),stringsAsFactors=FALSE)
      colorbar <- list(title = "", y = 0.75, tickvals=0:nFactor, ticktext=names(colours), tickfont = tickfont)
    } 
    
  } else if (variable == 'Deaths') {
    
    title <- 'Confirmed Deaths per Million Across the World'
    legend_title <- 'Deaths per Million'
    text <- paste(d$DeathsPerMillion, d$CountryName, sep = "\n")
    d$variable <- d$group_DeathsPerMillion
    tags <- c("0", "> 1", "> 10", "> 100", "> 1000")
    tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
    tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
    nFactor <- length(tags)
    zmin <- 0
    zmax <- nFactor-1
    colours <- brewer.pal(n = nFactor, name = "Reds")
    names(colours) <- tags
    colorscale <- data.frame(z=Z_Breaks(nFactor),
                             col=rep(colours,each=2),stringsAsFactors=FALSE)
    colorbar <- list(title = legend_title, y = 0.75, tickvals = 0:nFactor, ticktext = names(colours), tickfont = tickfont)
    
  } else if (variable == 'Cases') {
    
    title <- 'Confirmed Cases per Million Across the World'
    legend_title <- 'Cases per Million'
    text <- paste(d$CasesPerMillion, d$CountryName, sep = "\n")
    d$variable <- d$group_CasesPerMillion
    tags <- c("0", "> 1", "> 10", "> 100", "> 1000", "> 10000")
    tags <- gsub(" ", "\U2000", tags, fixed = TRUE)
    tags <- gsub("\\s", "\U2000", format(tags, width=max(nchar(lev, type = "width"), na.rm = TRUE)))
    nFactor <- length(tags)
    zmin <- 0
    zmax <- nFactor-1
    colours <- brewer.pal(n = nFactor, name = "Reds")
    names(colours) <- tags
    colorscale <- data.frame(z=Z_Breaks(nFactor),
                             col=rep(colours,each=2),stringsAsFactors=FALSE)
    colorbar <- list(title = legend_title, y = 0.75, tickvals = 0:nFactor, ticktext = names(colours), tickfont = tickfont)
    
  }
  
  if (continent == "World") {
    scope <- "world"
    lataxis <- NULL
    lonaxis <- NULL
    #lataxis <- list(range = (c(-60, 90)))
    #lonaxis <- list(range = (c(-180, 180)))
    
  } else if (continent == "Europe"){
    scope <- "europe"
    lataxis <- NULL
    lonaxis <- NULL
    #lataxis <- list(range = (c(30, 90)))
    #lonaxis <- list(range = (c(-30, 50)))
    
  }else if (continent == "NorthAmerica"){
    scope <- "north america"
    lataxis <- NULL
    lonaxis <- NULL
    #lataxis <- list(range = (c(10, 90)))
    #lonaxis <- list(range = (c(-180, -30)))
    
  } else if (continent == "SouthAmerica"){
    scope <- "south america"
    lataxis <- NULL
    lonaxis <- NULL
    #lataxis <- list(range = (c(-60, 15)))
    #lonaxis <- list(range = (c(-105, -30)))
    
  } else if (continent == "Asia"){
    scope <- "asia"
    lataxis <- NULL
    lonaxis <- NULL
    #lataxis <- list(range = (c(-20, 90)))
    #lonaxis <- list(range = (c(30, 180)))
    
  } else if (continent == "Africa"){
    scope <- "africa"
    lataxis <- NULL
    lonaxis <- NULL
    #lataxis <- list(range = (c(-40, 40)))
    #lonaxis <- list(range = (c(-20, 55)))
    
  }
  
  plot_ly(data = d, type = "choropleth", locations = d$CountryCode, z = d$variable,
          zmin = zmin, zmax = zmax, stroke = I("black"), span = I(1),
          text = text, hoverinfo = hoverinfo, colorscale = colorscale, colorbar = colorbar, source =  "heatmap") %>%
    layout(title = list(text = title), geo = list(showcountries = TRUE, scope = scope, lataxis = lataxis, 
                                                  lonaxis = lonaxis),
           dragmode = "zoom", uirevision = "date", margin = list(l = 0, r = 0, b = 0, t = 30)) %>% # uirevision?
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
    facet_wrap(~ Country, ncol = nr_cols) +
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
    facet_wrap(~ Country, ncol = nr_cols) +
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
    facet_wrap(~ Country, ncol = nr_cols) +
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
