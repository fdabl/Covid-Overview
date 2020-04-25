getAPIData <- function(startDate = "2020-01-01", endDate = Sys.Date()) {
  require("httr")
  require("jsonlite")
  base <- "https://covidtrackerapi.bsg.ox.ac.uk/api/stringency/date-range"
  startDate <- startDate
  call <- paste(base, startDate, endDate, sep = "/")
  getData <- GET(call)
  getDataText <- content(getData, "text")
  getDataJson <- fromJSON(getDataText, flatten = TRUE) # returns list with 3 levels
  data <- NULL
  for(i in 1:length(getDataJson$data)){
    for(j in 1:length(getDataJson$data[[i]])){
      for(k in 1:length(getDataJson$data[[i]][[j]])){
        if(!is.null(getDataJson$data[[i]][[j]][[k]]) == FALSE){
          getDataJson$data[[i]][[j]][[k]] <- NA # subsitutes NULL values in getDataJason that would cause problems with data frame creation by NA
        }
      }
      x <- as.data.frame(getDataJson$data[[i]][[j]])
      data <- rbind(data, x)
    }
  }
  return(data)
}
#data <- getAPIData()
