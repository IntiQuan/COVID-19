
genplots <- function(countries) {
  data <- IQRloadCSVdata("Resources/data.csv")
  
  # Add additional metrics
  data$PERCENT_DEATH_CONFIRMED <- 100*data$Deaths/data$Confirmed
  data$CONFIRMED_PER_MILLION_INHABITANTS <- data$Confirmed/(1e-6*data$POPULATION)
  data$DEATHS_PER_MILLION_INHABITANTS <- data$Deaths/(1e-6*data$POPULATION)
  
  dataSave <- data # Save for later analysis with shifted time axis
  
  # Long again
  data <- tidyr::gather(data,TYPE,VALUE,-COUNTRY,-DATE)

  # Plot 1 - Standard stuff absolute values
  dataPlot <- data[data$COUNTRY %in% countries & data$TYPE %in% c("Confirmed","Deaths"),]
  dataPlot <- dataPlot[dataPlot$VALUE>0,]
  dataPlot$DATE <- as.POSIXct(dataPlot$DATE)
  dataPlot$TYPE <- factor(dataPlot$TYPE,levels=c("Existing","Confirmed","Recovered","Deaths"))
  dataPlot <- dataPlot[!is.na(dataPlot$COUNTRY),]
  p1 <- IQRggplot(dataPlot,aes(x=DATE,y=VALUE,color=COUNTRY)) + 
    geom_point(size=1) + 
    geom_line() +
    scale_y_log10(breaks=c(1,10,100,1000,10000,100000,1000000,10000000),labels = scales::comma) + 
    scale_color_IQRtools() + 
    facet_wrap(.~TYPE,ncol=2,scales = "free_y") + 
    xlab("Date") + 
    ylab("Absolute Number of Cases")
  
  
  # Plot 2 - Relative cases
  dataPlot <- data[data$COUNTRY %in% countries & data$TYPE %in% c("PERCENT_DEATH_CONFIRMED"),]
  dataPlot <- dataPlot[dataPlot$VALUE>0,]
  dataPlot$DATE <- as.POSIXct(dataPlot$DATE)
  dataPlot$TYPE <- factor(dataPlot$TYPE,levels=c("PERCENT_DEATH_CONFIRMED"))
  dataPlot <- dataPlot[!is.na(dataPlot$COUNTRY),]
  p2 <- IQRggplot(dataPlot,aes(x=DATE,y=VALUE,color=COUNTRY)) + 
    geom_point(size=1) + 
    geom_line() +
    scale_y_log10(breaks=c(0.1,0.2,0.5,1,2,5,10,20,50,100)) + 
    coord_cartesian(ylim=c(0.1,100)) +
    scale_color_IQRtools() + 
    facet_wrap(.~TYPE,ncol=2,scale="free_y") + 
    xlab("Date") + 
    ylab("Percent")
  
  
  # Plot 3 - Relative to population
  labels <- c(0.01,0.1,1,10,100,1000,10000,100000,1000000,10000000)
  labels <- sort(labels+3*labels)
  dataPlot <- data[data$COUNTRY %in% countries & data$TYPE %in% c("CONFIRMED_PER_MILLION_INHABITANTS","DEATHS_PER_MILLION_INHABITANTS"),]
  dataPlot <- dataPlot[dataPlot$VALUE>0,]
  dataPlot$DATE <- as.POSIXct(dataPlot$DATE)
  dataPlot$TYPE <- factor(dataPlot$TYPE,levels=c("CONFIRMED_PER_MILLION_INHABITANTS","DEATHS_PER_MILLION_INHABITANTS"))
  dataPlot <- dataPlot[!is.na(dataPlot$COUNTRY),]
  p3 <- IQRggplot(dataPlot,aes(x=DATE,y=VALUE,color=COUNTRY)) + 
    geom_point(size=1) + 
    geom_line() +
    scale_y_log10(breaks=labels,labels = labels) + 
    scale_color_IQRtools() + 
    facet_wrap(.~TYPE,ncol=2,scales="free_y") + 
    xlab("Date") + 
    ylab("Cases per Million Inhabitants")
  
  
  # Gen data for bar plots for most recent day of data
  datax <- data
  datax$POPULATION <- NULL
  dataWide <- tidyr::spread(datax,COUNTRY,VALUE)
  dataLast <- do.call(rbind,lapply(split(dataWide,dataWide$TYPE), function (d) {
    d[nrow(d),]
  }))
  dataLast <- tidyr::gather(dataLast,COUNTRY,VALUE,-DATE,-TYPE)
  
  
  # Plot 4 - Relative cases
  dataPlot <- dataLast[dataLast$COUNTRY %in% countries,]
  dataPlot <- dataPlot[dataPlot$VALUE>=0,]
  dataPlot <- dataPlot[!is.na(dataPlot$VALUE),]
  dataPlot <- dataPlot[dataPlot$TYPE %in% c("PERCENT_DEATH_CONFIRMED"),]
  p4 <- IQRggplot(dataPlot,aes(x=COUNTRY,y=VALUE, fill=COUNTRY)) + 
    geom_bar(stat="identity") + 
    scale_fill_IQRtools() +
    facet_wrap(.~TYPE,scales = "free_y",ncol=2) +
    theme(axis.text.x=element_text(angle=60, hjust=1)) + 
    ylab("Percent")
  
  
  # Plot 5 - Relative to population
  dataPlot <- dataLast[dataLast$COUNTRY %in% countries,]
  dataPlot <- dataPlot[dataPlot$VALUE>=0,]
  dataPlot <- dataPlot[!is.na(dataPlot$VALUE),]
  dataPlot <- dataPlot[dataPlot$TYPE %in% c("CONFIRMED_PER_MILLION_INHABITANTS","DEATHS_PER_MILLION_INHABITANTS"),]
  p5 <- IQRggplot(dataPlot,aes(x=COUNTRY,y=VALUE, fill=COUNTRY)) + 
    geom_bar(stat="identity") + 
    scale_fill_IQRtools() +
    facet_wrap(.~TYPE,scales = "free_y",ncol=2) +
    theme(axis.text.x=element_text(angle=60, hjust=1)) + 
    ylab("Percent of Population")
  
  ####################################################################################
  # Generate time-normalized plot
  # Idea: - remove data with less than 10 confirmed cases per million inhabitants
  #       - shift time axis to call Day 0 the first day with >100 cases
  ####################################################################################

  dataTimeShifted <- do.call(rbind,lapply(split(dataSave, dataSave$COUNTRY), function (d) {
    d <- d[d$CONFIRMED_PER_MILLION_INHABITANTS>5,]
    if (nrow(d)==0) return(NULL)
    d$DAYSgt100 <- 0:(nrow(d)-1)
    d
  }))
  
  # Long again
  dataTimeShifted <- tidyr::gather(dataTimeShifted,TYPE,VALUE,-COUNTRY,-DAYSgt100,-DATE)
  
  labels <- c(0.01,0.1,1,10,100,1000,10000,100000,1000000,10000000)
  labels <- sort(labels+3*labels)
  dataPlot <- dataTimeShifted[dataTimeShifted$COUNTRY %in% countries & dataTimeShifted$TYPE %in% c("CONFIRMED_PER_MILLION_INHABITANTS","DEATHS_PER_MILLION_INHABITANTS"),]
  dataPlot <- dataPlot[dataPlot$VALUE>0,]
  dataPlot$TYPE <- factor(dataPlot$TYPE,levels=c("CONFIRMED_PER_MILLION_INHABITANTS","DEATHS_PER_MILLION_INHABITANTS"))
  dataPlot <- dataPlot[!is.na(dataPlot$COUNTRY),]
  p6 <- IQRggplot(dataPlot,aes(x=DAYSgt100,y=VALUE,color=COUNTRY)) + 
    geom_point(size=1) + 
    geom_line() +
    scale_y_log10(breaks=labels,labels = labels) + 
    scale_color_IQRtools() + 
    facet_wrap(.~TYPE,ncol=2,scales="free_y") + 
    xlab("Days since confirmed cases > 5 per million inhabitants") + 
    ylab("Cases per Million Inhabitants")
  
  p6
  
  ####################################################################################
  # Daily cases for last 30
  ####################################################################################
  
  dataPlot <- dataSave[dataSave$COUNTRY %in% countries,]
  
  Ndays <- 30
  dataPlot <- do.call(rbind,lapply(split(dataPlot,dataPlot$COUNTRY), function (d) {
    d$diffConfirmed <- c(NA,diff(d$Confirmed))
    d$diffDeath <- c(NA,diff(d$Death))
    d$diffRecovered <- c(NA,diff(d$Recovered))
    ixend <- nrow(d)
    d <- d[(ixend-Ndays):ixend,]
    d$Days <- -Ndays:0
    d
  }))
  
 p7 <- IQRggplot(dataPlot,aes(x = Days,y=diffConfirmed,fill=COUNTRY)) + 
    geom_bar(stat="identity",alpha=0.5) + 
    geom_smooth(method="loess",aes(color=COUNTRY),se=FALSE) +
    facet_wrap(.~COUNTRY,scales = "free",ncol = 2) + 
    scale_fill_IQRtools() + 
    scale_color_IQRtools() + 
    xlab("Days (0 indicates most recent data point)") + 
    ylab("Daily cases - Confirmed")
  
  p8 <- IQRggplot(dataPlot,aes(x = Days,y=diffDeath,fill=COUNTRY)) + 
    geom_bar(stat="identity",alpha=0.5) + 
    geom_smooth(method="loess",aes(color=COUNTRY),se=FALSE) +
    facet_wrap(.~COUNTRY,scales = "free",ncol = 2) + 
    scale_fill_IQRtools() + 
    scale_color_IQRtools() + 
    xlab("Days (0 indicates most recent data point)") + 
    ylab("Daily cases - Death")
  
  p9 <- IQRggplot(dataPlot,aes(x = Days,y=diffRecovered,fill=COUNTRY)) + 
    geom_bar(stat="identity",alpha=0.5) + 
    geom_smooth(method="loess",aes(color=COUNTRY),se=FALSE) +
    facet_wrap(.~COUNTRY,scales = "free",ncol = 2) + 
    scale_fill_IQRtools() + 
    scale_color_IQRtools() + 
    xlab("Days (0 indicates most recent data point)") + 
    ylab("Daily cases - Recovered")
  
  
  ####################################################################################
  # Percetage increase
  ####################################################################################
  
  dataPlot <- dataSave[dataSave$COUNTRY %in% countries,]
  
  Ndays <- 40
  dataPlot <- do.call(rbind,lapply(split(dataPlot,dataPlot$COUNTRY), function (d) {
    d$diffConfirmed <- c(NA,diff(d$Confirmed))
    d$Confirmed <- d$Confirmed
    d$Death <- d$Death
    d$Recovered <- d$Recovered
    ixend <- nrow(d)
    d <- d[(ixend-Ndays):ixend,]
    d$Days <- -Ndays:0
    d
  }))
  
  p10 <- IQRggplot(dataPlot,aes(x = Days,y=100*diffConfirmed/(Confirmed-Death-Recovered),fill=COUNTRY)) + 
    geom_point(alpha=0.5) + 
    geom_smooth(method="loess",aes(color=COUNTRY),se=FALSE) +
    facet_wrap(.~COUNTRY,scales = "free",ncol = 2) + 
    scale_fill_IQRtools() + 
    scale_color_IQRtools() + 
    xlab("Days (0 indicates most recent data point)") + 
    ylab("Daily confirmed cases in % of active cases")
  
  p11 <- IQRggplot(dataPlot,aes(x = Days,y=100*diffConfirmed/(Confirmed-Death-Recovered),fill=COUNTRY)) + 
    geom_point(alpha=0.5) + 
    geom_smooth(method="loess",aes(color=COUNTRY),se=FALSE) +
    facet_wrap(.~COUNTRY,scales = "free",ncol = 2) + 
    scale_fill_IQRtools() + 
    scale_color_IQRtools() + 
    scale_y_log10_IQRtools() +
    xlab("Days (0 indicates most recent data point)") + 
    ylab("Daily confirmed cases in % of active cases")
  
  
  list(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11)
}
