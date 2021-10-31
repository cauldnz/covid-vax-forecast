  #************Provide input parameters here**********
  growth_cap<-0.90
  threshold_line<-0.90
  first_dose_model <- TRUE #Models jut the first does. Interesting when looking at hesitancy/access as countries progress


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

  
  cty_chr<-'NZL'
  cty_iso<- 554
  
  #Dates - From: https://en.wikipedia.org/wiki/COVID-19_vaccination_in_New_Zealand
  change_points <- c(
    date('2021-03-10') #Official Roll out Start
    ,date('2021-05-10') #GP Clinics start rollout
    ,date('2021-07-19') #19 July Vaccine Shipment Arrival
    ,date('2021-08-17') #Suspended two days for lock down
    ,date('2021-08-19') #PM announces 12+ can book
    ,date('2021-10-16') #Super Saturday
    ,date('2021-10-26') #Mandate Announced
    )
  
  #Holidays
  # holiday_dates <- c(
  #   date('2021-04-2') #Good Friday
  #   ,date('2021-04-5') #Easter Monday
  #   ,date('2021-06-7') #Queens Birthday
  #   ,date('2021-08-18') #Suspended two days for lock down
  #   ,date('2021-08-19') #Suspended two days for lock down
  #   ,date('2021-10-25') #Labour Day
  # )
  
  popn_16_plus <- 3948520 #Population 16+ from MoH HSU data
  popn_manual<- 4208338  # Population over 12 from NZ MoH HSU Population projection in spreadsheet
  #popn_manual<- 4208338 + (791346/2) #As above but add in 50% of the 0-11 age group
  #country_data <- fread("https://raw.githubusercontent.com/cauldnz/nz-covid19-data-auto/main/vaccinations/Date.csv",col.names=c("date","first_dose","second_dose"), colClasses = c("Date","integer","integer"))
  country_data <- fread("https://raw.githubusercontent.com/UoA-eResearch/nz-covid19-data-auto/main/vaccinations/Date.csv",col.names=c("date","first_dose","second_dose"), colClasses = c("Date","integer","integer"))
  if (first_dose_model){
    country_series_dt <- country_data[,.(ds=date,y=cumsum(first_dose))]
  } else {
    country_series_dt <- country_data[,.(ds=date,y=cumsum(second_dose))]
  }
  
  
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
  
  series_dt <- country_series_dt
  
  series_dt[,ds:=as_date(ds)]
  
  
  series_dt[ds < date('2021-08-19'),cap:=popn_16_plus*growth_cap] # Set growth capacity at 90% of total population... 
  series_dt[ds >= date('2021-08-19'),cap:=proj_popn*growth_cap]
  
  
  model <- prophet(series_dt,growth = 'logistic', weekly.seasonality = TRUE, 
                   daily.seasonality = FALSE, yearly.seasonality = FALSE,
                   changepoints=change_points)
  
  ds_future <- data.table(make_future_dataframe(model, periods = 180))
  
  ds_future[,cap:=proj_popn*growth_cap]
  
  forecast <- predict(model, ds_future)
  
  #Static Plot
  plot(model, forecast)
  
  prophet_plot_components(model, forecast)
  
  
  #Dynamic Plot
  dyplot.prophet(model, forecast, graphTitle=paste("Forecast for: ", cty_chr, " Latest data: ", max(series_dt$ds), ifelse(first_dose_model," First Dose Only", " Second Doses")), limitLine=proj_popn*threshold_line, limitLabel=paste(threshold_line*100,"%"))
  
  
  
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
