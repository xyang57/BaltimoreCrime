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

url = 'api.openweathermap.org/data/2.5/forecast?id=4347778&APPID=381091a1732cfe53efc11a419db2e93d'
Neighborhood = readRDS('Neighborhood.rds')
cluster = readRDS('KmeansNeighborhood.rds')

Res = GET(url)
# Res = reactive({
#   invalidateLater(3600000)
#   httr::GET(url)
# })


  data = fromJSON(rawToChar(Res$content))
  Date = as_datetime(data$list$dt-5*60*60)
  temp_hump = data$list$main %>% 
    select(temp,humidity)
  wind <- data$list$wind$speed
  weather_daily <- data.frame(Date,temp_hump,wind) %>% 
    mutate(Date = date(Date)) %>% 
    group_by(Date) %>% 
    summarize(count = n(), temp = mean(temp), humidity = mean(humidity), wind_speed = max(wind)) %>% 
    filter(count == 8)
  prepare_pred <- expand.grid(Date = unique(weather_daily$Date),Neighborhood = Neighborhood) %>% 
    mutate(Month = month(Date), Year = year(Date),
           Day = day(Date),  
           Weekday = weekdays(Date, abbreviate=T), Week = week(Date)) %>% 
    mutate(Weekday = factor(Weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
  prepare_daily <- prepare_pred %>% 
    left_join(weather_daily) %>% 
    left_join(cluster) %>% 
    mutate(Day = as.factor(Day),
           Month = as.factor(Month))
  load('glmnet_model_assault.rda')
  load('glmnet_model_homicide.rda')
  load('glmnet_model_property.rda')
  load('glmnet_model_robbery.rda')
  load('glmnet_model_shooting.rda')
  load('glmnet_model_rape.rda')
  prepare_daily$Property = predict(glmnet_model_property,prepare_daily,type = 'prob')$Yes
  prepare_daily$Assault = predict(glmnet_model_assault,prepare_daily, type = 'prob')$Yes
  prepare_daily$Robbery = predict(glmnet_model_robbery,prepare_daily, type = 'prob')$Yes
  prepare_daily$Shooting = predict(glmnet_model_shooting,prepare_daily, type = 'prob')$Yes
  prepare_daily$Homicide = predict(glmnet_model_homicide,prepare_daily, type = 'prob')$Yes
  prepare_daily$Rape = predict(glmnet_model_rape,prepare_daily, type = 'prob')$Yes
  crime_prediction = prepare_daily %>% 
    select(Date,Neighborhood,Property:Rape) %>% 
    gather(Type,Probability, -Date,-Neighborhood) %>% 
    spread(Date,Probability)
 
#crime_prediction = read_csv('crime_prediction.csv')
ui <- fluidPage(
  pageWithSidebar(
    headerPanel('Baltimore Crime Prediction'),
    sidebarPanel(
      selectInput('date', 'Please choose a date', names(crime_prediction[3:dim(crime_prediction)[2]]),
                  selected = names(crime_prediction)[3]),
      selectInput('type','Please choose a type',unique(crime_prediction$Type),
                  selected = 'Property'),
      br(),
      br(),
      br(),
      p('The output crime chance is calculated based on date and weather forcast of future days with a logistic regression model I built using 
        historical crime combined with weather data.'),
      
      p('For a detailed process of how the model is built, please visit my',a("Github.", 
                                                                       href = "https://github.com/xyang57/Baltimore-Crime"))),
    mainPanel ( 
      leafletOutput("mymap",height = 750)
    )
    
      
    )
  
)



server <- function(input, output, session){
  ### preapare map for visualizing crime probability
  shapefile <- readOGR(path.expand("./Neighborhood_202010/"), "nhood_2010")
  
  
  #pal = reactive({colorNumeric('RdYlGn',domain = (shapefile2$input$date),reverse = T)})
  observe({
    type = input$type
    crime_prediction <- crime_prediction %>% 
      filter(Type == type)
    merge_data <- shapefile@data %>% 
      left_join(crime_prediction,by = c('LABEL' = 'Neighborhood'))
    neighborhoods_not_match = merge_data$LABEL[is.na(merge_data$Type)]
    # ### in crime_by_neighborhood$Neighborhood, some neighborhood are shrinked because of too long,
    # ### need to replace the short neighborhood to the original neighborhood so they can match
    
    
    for (i in 1:length(neighborhoods_not_match)) {
      for (j in 1:length(crime_prediction$Neighborhood)){
        if (str_detect(neighborhoods_not_match[i],crime_prediction$Neighborhood[j])
            & str_length(neighborhoods_not_match[i])-str_length(crime_prediction$Neighborhood[j]) < 9){
          crime_prediction$Neighborhood[j] = neighborhoods_not_match[i]
        }
      }
      
    }
    
    shapefile@data <- shapefile@data %>% 
      left_join(crime_prediction,by = c('LABEL' = 'Neighborhood'))
    shapefile2 <- spTransform(shapefile, CRS("+init=epsg:4326"))
  colorBy = input$date
  #pale = colorRampPalette(c("green", "red"))(10)
  pal = colorNumeric('RdYlGn',domain = (shapefile2@data[[colorBy]]*100), reverse = T)
  output$mymap <- renderLeaflet({
   
    shapefile2 %>% 
      leaflet(options = 
                leafletOptions(Zoom = 13)) %>% 
      addSearchOSM() %>% 
      #addReverseSearchOSM() %>% 
      addResetMapButton() %>% 
      addProviderTiles(provider = 'CartoDB') %>% 
      addPolygons(weight = 0.5, color = ~pal(shapefile2@data[[colorBy]]*100),label = ~paste0(LABEL,', Crime Chance: ',round(shapefile2@data[[colorBy]],2)*100,'%'),
                  highlight = highlightOptions(weight = 5, color = "white",
                                               bringToFront = TRUE)) %>% 
      addLegend(pal = pal, values = range(shapefile2@data[[colorBy]])*100, title = 'Chance',
                position = 'bottomleft')
  })
  })
}

shinyApp(server = server, ui = ui)
