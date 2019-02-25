########################### Load Libraries and Set Working Directory ##################################
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(leaflet)
library(rgdal)
library(leaflet.extras)
library(jsonlite)
library(httr)
library(lubridate)
library(caret)
library(glmnet)
setwd('~/Google Drive/GithubProject/Shiny_Baltimore_Crime/')

################# Get Real-time Forecast Weather Data Using OpenWeatherMap API #########################
url = 'api.openweathermap.org/data/2.5/forecast?id=4347778&APPID=381091a1732cfe53efc11a419db2e93d'
Res = GET(url)
data = fromJSON(rawToChar(Res$content))

# convert time zone form UTC to EST
Date = as_datetime(data$list$dt-5*60*60)
temp_hump = data$list$main %>% 
  dplyr::select(temp,humidity)
wind <- data$list$wind$speed

# Combine hourly data to daily data
weather_daily <- data.frame(Date,temp_hump,wind) %>% 
  mutate(Date = date(Date)) %>% 
  group_by(Date) %>% 
  summarize(count = n(), temp = mean(temp), humidity = mean(humidity), wind_speed = max(wind)) %>% 
  filter(count == 8)

################################ Prepare dataset for prediction #################################
# Load Baltimore Neighborhood, and cluster info
Neighborhood = readRDS('Neighborhood.rds')
cluster = readRDS('KmeansNeighborhood.rds')

# Generate every possible combination of date and neighborhood
prepare_pred <- expand.grid(Date = unique(weather_daily$Date),Neighborhood = Neighborhood) %>% 
    mutate(Month = month(Date), Year = year(Date),
           Day = day(Date),  
           Weekday = weekdays(Date, abbreviate=T), Week = week(Date)) %>% 
    mutate(Weekday = factor(Weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
# Combine with weather data
prepare_daily <- prepare_pred %>% 
    left_join(weather_daily) %>% 
    left_join(cluster) %>% 
    mutate(Day = as.factor(Day),
           Month = as.factor(Month))

######################################### Prediction #####################################################
# Load models for different kinds of crimes
load('glmnet_model_assault.rda')
load('glmnet_model_homicide.rda')
load('glmnet_model_property.rda')
load('glmnet_model_robbery.rda')
load('glmnet_model_shooting.rda')
load('glmnet_model_rape.rda')

# Predict crime probability
prepare_daily$Property = predict(glmnet_model_property,prepare_daily,type = 'prob')$Yes
prepare_daily$Assault = predict(glmnet_model_assault,prepare_daily, type = 'prob')$Yes
prepare_daily$Robbery = predict(glmnet_model_robbery,prepare_daily, type = 'prob')$Yes
prepare_daily$Shooting = predict(glmnet_model_shooting,prepare_daily, type = 'prob')$Yes
prepare_daily$Homicide = predict(glmnet_model_homicide,prepare_daily, type = 'prob')$Yes
prepare_daily$Rape = predict(glmnet_model_rape,prepare_daily, type = 'prob')$Yes

# Data transformation for later display
crime_prediction = prepare_daily %>% 
    dplyr::select(Date,Neighborhood,Property:Rape) %>% 
    ### gather into long format
    gather(Type,Probability, -Date,-Neighborhood) %>% 
    ### spread into crime probability with date in the columns
    spread(Date,Probability)
 
################################### Shiny application ############################################
########################### Building user interface for shiny application ####################################
ui <- fluidPage(
  pageWithSidebar(
    # header of the application
    headerPanel('Baltimore Crime Prediction'),
    sidebarPanel(
      # choose a date for display
      selectInput('date', 'Please choose a date', names(crime_prediction[3:dim(crime_prediction)[2]]),
                  selected = names(crime_prediction)[3]),
      # choose a crime type for display
      selectInput('type','Please choose a type',unique(crime_prediction$Type),
                  selected = 'Property'),
      br(),
      br(),
      br(),
      p('The output crime chance is calculated based on date and weather forcast of future days with a logistic regression model built by Xu Yang using 
        historical crime combined with weather data.'),
      
      p('For a detailed process of how the model is built, please visit my',a("Github.", 
                                                                       href = "https://github.com/xyang57/BaltimoreCrime"))),
    # Map output
    mainPanel ( 
      leafletOutput("mymap",height = 750)
    )
    )
)

################################# Server function for Shiny ######################################
server <- function(input, output, session){
  # preapare map for visualizing crime probability
  shapefile <- readOGR(path.expand("./Neighborhood_202010/"), "nhood_2010")
  
  # Generate display based on user's input
  observe({
    type = input$type
    # get the right type from user's input
    crime_prediction <- crime_prediction %>% 
      filter(Type == type)
    
    # Join the data to the shapefile of the map for later display
    merge_data <- shapefile@data %>% 
      left_join(crime_prediction,by = c('LABEL' = 'Neighborhood'))
    neighborhoods_not_match = merge_data$LABEL[is.na(merge_data$Type)]
    # Crime_prediction$Neighborhood, some neighborhood are shrinked because of too long,
    # need to replace the short neighborhood to the original neighborhood so they can match
    for (i in 1:length(neighborhoods_not_match)) {
      for (j in 1:length(crime_prediction$Neighborhood)){
        if (str_detect(neighborhoods_not_match[i],crime_prediction$Neighborhood[j])
            & str_length(neighborhoods_not_match[i])-str_length(crime_prediction$Neighborhood[j]) < 9){
          crime_prediction$Neighborhood[j] = neighborhoods_not_match[i]
        }
      }
    }
    
    # Rejoin the data with right neighborhood
    shapefile@data <- shapefile@data %>% 
      left_join(crime_prediction,by = c('LABEL' = 'Neighborhood'))
    
    # Transform coordinate system for right display
    shapefile2 <- spTransform(shapefile, CRS("+init=epsg:4326"))
    
    # Color gradient based on probability of crime
    colorBy = input$date
    pal = colorNumeric('RdYlGn',domain = (shapefile2@data[[colorBy]]*100), reverse = T)
    
    # Output the map
    output$mymap <- renderLeaflet({
    shapefile2 %>% 
      leaflet(options = 
                leafletOptions(Zoom = 13)) %>% 
      # add search function for the map
      addSearchOSM() %>% 
      # add map reset function
      addResetMapButton() %>% 
      addProviderTiles(provider = 'CartoDB') %>% 
      # add display when hover over the neighborhood
      addPolygons(weight = 0.5, color = ~pal(shapefile2@data[[colorBy]]*100),label = ~paste0(LABEL,', Crime Chance: ',round(shapefile2@data[[colorBy]],2)*100,'%'),
                  highlight = highlightOptions(weight = 5, color = "white",
                                               bringToFront = TRUE)) %>% 
      # add legend
      addLegend(pal = pal, values = range(shapefile2@data[[colorBy]])*100, title = 'Chance',
                position = 'bottomleft')
  })
  })
}

################################## Running the application ###########################################
shinyApp(server = server, ui = ui)
