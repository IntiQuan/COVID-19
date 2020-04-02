# Only load once a day
LOAD <- TRUE
# if (file.exists("Resources/dateLoaded.txt")) {
#   if (aux_fileread("Resources/dateLoaded.txt") == Sys.Date()) {
#     LOAD <- FALSE
#   }
# } 
# # Store date
# aux_filewrite(as.character(Sys.Date()),filename = "Resources/dateLoaded.txt")

if (LOAD) {
  # Load data
  dConfirmed <- IQRtools::IQRloadCSVdata("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  dDeaths <-IQRtools::IQRloadCSVdata("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  dRecovered <-IQRtools::IQRloadCSVdata("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  
  data <- rbind(
    cbind(TYPE="Confirmed",dConfirmed),
    cbind(TYPE="Deaths",dDeaths),
    cbind(TYPE="Recovered",dRecovered)
  )
  data$Province.State <- NULL
  data$Lat <- NULL
  data$Long <- NULL
  
  # Combine by Country and Type
  dS <- split(data,data$Country.Region)
  data <- do.call(rbind,lapply(seq_along(dS), function (k) {
    d <- dS[[k]]
    dS2 <- split(d,d$TYPE)
    do.call(rbind,lapply(seq_along(dS2), function (k2) {
      d2 <- dS2[[k2]]
      val <- d2[,c(-1,-2)]
      row <- d2[1,]
      row[,c(-1,-2)] <- apply(val,2,sum)
      row
    }))
  }))
  
  # Need to gather by date and change the date formatting
  data <- tidyr::gather(data,DATE,VALUE,-TYPE,-Country.Region)
  data$DATE <- gsub("X","",data$DATE)
  data$DATE <- as.POSIXct(data$DATE,format="%m.%d.%y")
  
  names(data)[2] <- "COUNTRY"
  
  
  # Format data differently
  data <- tidyr::spread(data,TYPE,VALUE)
  
  # Add population numbers per country
  pop <- IQRtools::IQRloadCSVdata("Resources/population.csv")
  
  # Rename countries
  data$COUNTRY[data$COUNTRY=="Korea, South"] <- "South Korea"
  data$COUNTRY[data$COUNTRY=="US"] <- "United States"
  pop$COUNTRY[pop$COUNTRY=="Czech Republic (Czechia)"] <- "Czechia"

  # Join
  x <- dplyr::left_join(data,pop,by="COUNTRY")
  x <- x[!is.na(x$POPULATION),]
  
  IQRtools::IQRsaveCSVdata(x,"Resources/data.csv")
}