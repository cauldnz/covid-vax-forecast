# I am a fan of the http://covidvax.live site. But, from what I can tell it's just straight line projecting
# recent growth out in order to build its forecast for when countries reach 70%
# I'm not convinced this is quite as realistic as we can be given it's a constrained growth (logistic growth) model
# So here's my attempt at using the same data set for a quick and dirty whip-up of a Prophet based growth model
#************Provide input parameters here**********
#cty_chr<-'AUS' #Provide as code from here https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#cty_iso<- 36 #Provide as Integer (no leading 0) from here https://en.wikipedia.org/wiki/ISO_3166-1_numeric
#cty_chr<-'JPN' #Provide as code from here https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
#cty_iso<- 392 #Provide as Integer (no leading 0) from here https://en.wikipedia.org/wiki/ISO_3166-1_numeric
cty_chr<-'NZL' #Provide as code from here https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
cty_iso<- 554 #Provide as Integer (no leading 0) from here https://en.wikipedia.org/wiki/ISO_3166-1_numeric
#*
#*
#*****************************************************



# install the package
#install.packages("COVID19")
#install.packages('jsonlite')
#install.packages('prophet')
#install.packages('data.table')
#install.packages('lubridate')
#install.packages('wpp2019')
#install.packages("rmarkdown", repos="https://cran.r-project.org/") #I use MRO... so force install latest from CRAN

# load the package
#library('COVID19') # We don't use this library as the vaccination data is too coarse grained.
library(jsonlite)
library(prophet)
library(data.table)
library(lubridate)
library(wpp2019)
library(dplyr)

data(pop)
popn <-data.table(pop) #Forecast population

proj_popn<- as.integer(popn[country_code==cty_iso]$'2020'  * 1000 )

#Retrieve raw 'Our World in Data' vaccination dataset
vax_data_json <- fromJSON('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.json')
vax_data <- data.table(vax_data_json$data[which(vax_data_json$iso_code==cty_chr)][[1]])
series_dt <- vax_data[,.(ds=date,y=people_fully_vaccinated)]
series_dt[,ds:=as_date(ds)]
series_dt[,cap:=proj_popn*.9] # Set growth capacity at 90% of total population... 

model <- prophet(series_dt,growth = 'logistic')
ds_future <- data.table(make_future_dataframe(model, periods = 180))
ds_future[,cap:=proj_popn*.9]
forecast <- predict(model, ds_future)

#Static Plot
plot(model, forecast)

#Dynamic Plot
dyplot.prophet(model, forecast, graphTitle=paste("Forecast for: ", cty_chr))


#Nicked from https://stackoverflow.com/questions/53947623/how-to-change-type-of-line-in-prophet-plot
dyplot.prophet <- function(x, fcst, uncertainty=TRUE, graphTitle, ...) 
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
    dygraphs::dyUnzoom()
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