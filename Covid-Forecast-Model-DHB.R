  # This basically forks off the by country forecast model to work on NZ MoH's by DHB dataset.
  # This data is weekly so it a bit rougher. You can grab DHB name and population here https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-data-and-statistics/covid-19-vaccine-data
  #************Provide input parameters here**********
  dhb_name <- "Northland"
  dhb_population <- 161320 #Manually entered population of DHB region
  

  # load the package
  #library('COVID19') # We don't use this library as the vaccination data is too coarse grained.
  library(jsonlite)
  library(prophet)
  library(data.table)
  library(lubridate)
  library(wpp2019)
  library(dplyr)
  library(dygraphs)
  
  data(pop)

  growth_cap<-0.90
  threshold_line<-0.90
  first_dose_model <- TRUE #Models jut the first does. Interesting when looking at hesitancy/access as countries progress

  
  cty_chr<-'NZL'
  cty_iso<- 554
  popn_manual<- 4208338  # Population over 12 from NZ MoH HSU Population projection in spreadsheet
  
  dhb_data <- fread("https://raw.githubusercontent.com/cauldnz/nz-covid19-data-auto/main/vaccinations/Group%20by%20DHBOfService.csv",col.names=c("date","group","dhb","dose_num","doses","notes"), colClasses = c("Date","factor","factor","integer","integer","factor"))
  dhb_data$notes<-NULL
  dhb_data <- dhb_data[dose_num==1 & dhb==dhb_name]
  dhb_data <- dhb_data[, .(date=date, first_dose=sum(doses)), by=list(date,dhb,dose_num)]
  dhb_series_dt <- dhb_data[,.(ds=date,y=cumsum(first_dose))]
  #country_data <- fread("https://raw.githubusercontent.com/UoA-eResearch/nz-covid19-data-auto/main/vaccinations/Date.csv",col.names=c("date","first_dose","second_dose"), colClasses = c("Date","integer","integer"))
  country_series_dt <- country_data[,.(ds=date,y=cumsum(first_dose))]
  country_series_dt<-dhb_series_dt
  popn_manual<- dhb_population 
  
 
  #*****************************************************
  
  
  
  # install the package
  #install.packages("COVID19")
  #install.packages('jsonlite')
  #install.packages('prophet')
  #install.packages('data.table')
  #install.packages('lubridate')
  #install.packages('wpp2019')
  #install.packages("rmarkdown", repos="https://cran.r-project.org/") #I use MRO... so force install latest from CRAN
  

  
  if(popn_manual>0)  {
    proj_popn<-popn_manual
  } else {
    popn <-data.table(pop) #Forecast population
    proj_popn<- as.integer(popn[country_code==cty_iso]$'2020'  * 1000 )
  }

  #Retrieve raw 'Our World in Data' vaccination dataset
  vax_data_json <- fromJSON('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.json')
  #vax_data_json <- fromJSON('https://raw.githubusercontent.com/cauldnz/covid-19-data/cauld/public/data/vaccinations/vaccinations.json')
  vax_data <- data.table(vax_data_json$data[which(vax_data_json$iso_code==cty_chr)][[1]])
  
  if (!first_dose_model){
    #Model double dose and assume two-dose regime for all
    proj_popn <- proj_popn * 2 #Moved to using total vaccinations so assume 2 dose reigime and just double popn. 
    series_dt <- vax_data[,.(ds=date,y=total_vaccinations)]
  } else {
    series_dt <- country_series_dt
  }
  series_dt[,ds:=as_date(ds)]
  series_dt[,cap:=proj_popn*growth_cap] # Set growth capacity at 90% of total population... 
  
  model <- prophet(series_dt,growth = 'logistic', weekly.seasonality = FALSE, daily.seasonality = FALSE, yearly.seasonality = FALSE)
  
  ds_future <- data.table(make_future_dataframe(model, periods = 180))
  ds_future[,cap:=proj_popn*growth_cap]
  forecast <- predict(model, ds_future)
  
  #Static Plot
  plot(model, forecast)
  
  #Dynamic Plot
  dyplot.prophet(model, forecast, graphTitle=paste("Forecast for: ", dhb_name, " Latest data: ", max(series_dt$ds), ifelse(first_dose_model," First Dose Only", " All Doses")), limitLine=proj_popn*threshold_line, limitLabel=paste(threshold_line*100,"%"))
  
  #Nicked from https://stackoverflow.com/questions/53947623/how-to-change-type-of-line-in-prophet-plot
  dyplot.prophet <- function(x, fcst, uncertainty=TRUE, graphTitle, limitLine, limitLabel, ...) 
  {
    forecast.label='Predicted'
    actual.label='Actual'
    # create data.frame for plotting
    df <- prophet:::df_for_plotting(x, fcst)
    
    # build variables to include, or not, the uncertainty data
    if(uncertainty && exists("yhat_lower", where = df))
    {
      colsToKeep <- c('y', 'yhat', 'yhat_lower', 'yhat_upper')
      forecastCols <- c('yhat_lower', 'yhat', 'yhat_upper')
    } else
    {
      colsToKeep <- c('y', 'yhat')
      forecastCols <- c('yhat')
    }
    # convert to xts for easier date handling by dygraph
    dfTS <- xts::xts(df %>% dplyr::select_(.dots=colsToKeep), order.by = df$ds)
    
    # base plot
    dyBase <- dygraphs::dygraph(dfTS, main=graphTitle)
    
    presAnnotation <- function(dygraph, x, text) {
      dygraph %>%
        dygraphs::dyAnnotation(x, text, text, attachAtBottom = TRUE)
    }
    
    dyBase <- dyBase %>%
      # plot actual values
      dygraphs::dySeries(
        'y', label=actual.label, color='black',stepPlot = TRUE, strokeWidth=1
      ) %>%
      # plot forecast and ribbon
      dygraphs::dySeries(forecastCols, label=forecast.label, color='blue') %>%
      # allow zooming
      dygraphs::dyRangeSelector() %>% 
      # make unzoom button
      dygraphs::dyUnzoom() %>%
      #Add Crosshairs
      dyCrosshair(direction = "both") %>%
      #Add threshold line and label
      dyLimit(as.numeric(limitLine), color = "red", label=limitLabel)
    if (!is.null(x$holidays)) {
      for (i in 1:nrow(x$holidays)) {
        # make a gray line
        dyBase <- dyBase %>% dygraphs::dyEvent(
          x$holidays$ds[i],color = "rgb(200,200,200)", strokePattern = "solid")
        dyBase <- dyBase %>% dygraphs::dyAnnotation(
          x$holidays$ds[i], x$holidays$holiday[i], x$holidays$holiday[i],
          attachAtBottom = TRUE)
      }
    }
    return(dyBase)
  }
