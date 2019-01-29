# Load libraries
library(tidyverse)
library(lubridate)
library(caret)
library(ranger)
library(forcats)
library(stringr)
### set working directory
setwd('~/Google Drive/GithubProject/Shiny_Application/')
# read data
crime <- read_csv('BPD_Part_1_Victim_Based_Crime_Data.csv') %>% 
  distinct() # get distinct rows of data with no duplicates

## see how large is the dataset
dim(crime)
sum(is.na(crime$Neighborhood))

## define look up table for crime types, combine crime type into 6 big categories
lut <- c("COMMON ASSAULT" = "ASSAULT",
         "LARCENY FROM AUTO" = "PROPERTY",
         "AGG. ASSAULT" = "ASSAULT",
         "ROBBERY - STREET" = "ROBBERY",
         "LARCENY" = "PROPERTY",
         "ASSAULT BY THREAT" = "ASSAULT",
         "ROBBERY - CARJACKING" = "ROBBERY",
         "AUTO THEFT" = "PROPERTY",
         "SHOOTING" = "SHOOTING",
         "HOMICIDE" = "HOMICIDE",
         "BURGLARY" = "PROPERTY",
         "ROBBERY - COMMERCIAL" = "ROBBERY",
         "ROBBERY - RESIDENCE" = "ROBBERY",
         "RAPE" = "RAPE",
         "ARSON" = "PROPERTY"
)
crime$Type <- lut[crime$Description]

##### calculate Month, Year, Day of Month, Hour of crime and Day of weeks for crime
crime <- crime %>% 
  select(CrimeDate,Neighborhood, Longitude, Latitude,  Type) %>% 
  mutate(CrimeDate = mdy(CrimeDate)) %>%  
  filter(!is.na(Type))

## collapse crime count to daily count
crime_daily <- crime %>% 
  group_by(CrimeDate,  Neighborhood, Type) %>% 
  summarize(count = n())
names(crime_daily)[1]='Date'

## prepare the crime dataset to set count to 0 when no certain crime happens
Neighborhood = unique(crime_daily$Neighborhood[!is.na(crime_daily$Neighborhood)])
saveRDS(Neighborhood,file = 'Neighborhood.rds')
Type = unique(crime_daily$Type)
Date = seq(ymd('2012-01-01'),ymd('2019-01-12'), by = '1 day')
prepare <- expand.grid(Date = Date,Neighborhood = Neighborhood,Type = Type) %>% 
  mutate(Month = month(Date), Year = year(Date),
         Day = day(Date),  
         Weekday = weekdays(Date, abbreviate=T), Week = week(Date)) %>% 
  mutate(Weekday = factor(Weekday, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")))
crime_daily <- prepare %>% 
  left_join(crime_daily)
crime_daily$count[is.na(crime_daily$count)] = 0
table(crime_daily$count)

crime_daily$Crime = if_else(crime_daily$count == 0, 'No', 'Yes')

table(crime_daily$Crime,crime_daily$Type)

# read weather data
weather <- read_csv('Weather.csv') 
weather <- weather %>% 
  select(dt_iso,temp,temp_min,temp_max,pressure,humidity,wind_speed,clouds_all, weather_description) %>% 
  mutate(count = 1, weather_description = str_replace_all(weather_description,' ','_')) %>% 
  mutate(DateTime = ymd_hms(dt_iso,tz = "America/New_York"), Date = date(DateTime)) %>% 
  distinct(dt_iso,.keep_all = T) %>% 
  spread(weather_description, count, fill = 0) %>% 
  select(-dt_iso)

weather_daily <- weather %>% 
  group_by(Date) %>% 
  summarize(temp = mean(temp, na.rm = T), temp_min = min(temp_min,na.rm = T), temp_max = max(temp_max),
            pressure = mean(pressure, na.rm = T), humidity = mean(humidity, na.rm = T), wind_speed = max(wind_speed, na.rm  = T),
            clouds_all = mean(clouds_all,na.rm=T),broken_clouds=sum(broken_clouds),drizzle = sum(drizzle),
            few_clouds = sum(few_clouds), fog=sum(fog), freezing_rain = sum(freezing_rain),haze=sum(haze),
            heavy_intensity_drizzle = sum(heavy_intensity_drizzle),heavy_intensity_rain=sum(heavy_intensity_rain),
            heavy_intensity_shower_rain=sum(heavy_intensity_shower_rain),heavy_snow=sum(heavy_snow),
            light_intensity_drizzle=sum(light_intensity_drizzle),light_intensity_shower_rain=sum(light_intensity_shower_rain),
            light_rain=sum(light_rain),light_rain_and_snow=sum(light_rain_and_snow),light_shower_snow=sum(light_shower_snow),
            light_snow=sum(light_snow),mist=sum(mist),moderate_rain=sum(moderate_rain),overcast_clouds=sum(overcast_clouds),
            proximity_shower_rain=sum(proximity_shower_rain),proximity_thunderstorm=sum(proximity_thunderstorm),
            proximity_thunderstorm_with_rain=sum(proximity_thunderstorm_with_rain),scattered_clouds=sum(scattered_clouds),
            shower_rain=sum(shower_rain),shower_snow=sum(shower_snow),sky_is_clear=sum(sky_is_clear+Sky_is_Clear),
            smoke=sum(smoke),snow=sum(snow),SQUALLS=sum(SQUALLS),thunderstorm=sum(thunderstorm),
            thunderstorm_with_heavy_rain=sum(thunderstorm_with_heavy_rain),thunderstorm_with_light_drizzle=sum(thunderstorm_with_light_drizzle),
            thunderstorm_with_light_rain=sum(thunderstorm_with_light_rain),thunderstorm_with_rain=sum(thunderstorm_with_rain),
            very_heavy_rain=sum(very_heavy_rain))

weather_crime_daily <- crime_daily %>% 
  left_join(weather_daily) %>% 
  filter(!is.na(temp))
### cluster neighborhood into different clusters based on different kinds of crime
crime_by_neighborhood <- crime %>% 
  group_by(Neighborhood,Type) %>% 
  summarize(count = n()) %>% 
  drop_na() %>% 
  spread(Type,count,fill = 0)
scaled = scale(crime_by_neighborhood[,2:7])

### select k clustering based on total within cluster sum of squared
wss = 0
for (i in 1:20) {
  km.out = kmeans(scaled, centers = i, nstart = 20, iter.max = 50)
  wss[i] = km.out$tot.withinss
}
plot(wss)
### It looks like 10 should be enough
km.out = kmeans(scaled, centers = 10, nstart = 20, iter.max = 50)
crime_by_neighborhood$cluster = km.out$cluster
neighborhood_cluster <- crime_by_neighborhood %>% 
  select(Neighborhood,cluster) %>% 
  mutate(cluster = as.factor(cluster))
saveRDS(neighborhood_cluster,'KmeansNeighborhood.rds')
### add neighborhood cluster to the dataset
weather_crime_daily <- weather_crime_daily %>% 
  left_join(neighborhood_cluster)

weather_crime_daily$Month = as.factor(weather_crime_daily$Month)
weather_crime_daily$Day = as.factor(weather_crime_daily$Day)
train_property <- weather_crime_daily %>% filter(Date <ymd(20181001),Type == 'PROPERTY')
train_assault <- weather_crime_daily %>% filter(Date <ymd(20181001),Type == 'ASSAULT')
train_robbery <- weather_crime_daily %>% filter(Date <ymd(20181001),Type == 'ROBBERY')
train_shooting <- weather_crime_daily %>% filter(Date <ymd(20181001),Type == 'SHOOTING')
train_homicide <- weather_crime_daily %>% filter(Date <ymd(20181001),Type == 'HOMICIDE')
train_rape <- weather_crime_daily %>% filter(Date <ymd(20181001),Type == 'RAPE')


test_property <- weather_crime_daily %>% filter(Date >= ymd(20181001),Type == 'PROPERTY')
test_assault <- weather_crime_daily %>% filter(Date >=ymd(20181001),Type == 'ASSAULT')
test_robbery <- weather_crime_daily %>% filter(Date >=ymd(20181001),Type == 'ROBBERY')
test_shooting <- weather_crime_daily %>% filter(Date >=ymd(20181001),Type == 'SHOOTING')
test_homicide <- weather_crime_daily %>% filter(Date >=ymd(20181001),Type == 'HOMICIDE')
test_rape <- weather_crime_daily %>% filter(Date >=ymd(20181001),Type == 'RAPE')


glm_model <- glm(as.factor(Crime) ~ Neighborhood + Day + Month + Weekday + temp + humidity + wind_speed + thunderstorm + sky_is_clear,
                 data = train_property, family = binomial)
lda_model <- lda(as.factor(Crime) ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                 data = train_property)
qda_model <- qda(as.factor(Crime) ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                 data = train_property)
rf_model <- ranger(as.factor(Crime)~cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                   data = train_property,num.trees = 500,probability = T)

roc_obj_glm <- roc(test_property$Crime, predict(glm_model,test_property, type = 'response'))
auc_glm = auc(roc_obj_glm)
roc_obj_lda <- roc(test_property$Crime, predict(lda_model,test_property)$posterior[,2])
auc_lda = auc(roc_obj_lda)
roc_obj_qda <- roc(test_property$Crime, predict(qda_model,test_property)$posterior[,2])
auc_qda = auc(roc_obj_qda)
roc_obj_rf <- roc(test_property$Crime, predict(rf_model,test_property)$predictions[,2])
auc_rf = auc(roc_obj_rf)

### create folds for cross validation
set.seed(32)
myFolds <- createFolds(train_property$Crime, k = 3)
myControl = trainControl(summaryFunction = twoClassSummary, classProbs = T,verboseIter = T,
                          savePredictions = F,
                          index = myFolds, trim = T, returnData = F)
rf_model = train(as.factor(Crime)~cluster+Day+Month+Weekday+temp+humidity+wind_speed,
                  data = train_property,
                  metric = 'ROC',
                  method = "ranger",
                  tuneGrid = expand.grid(mtry = c(2,5,10),
                                         splitrule = 'gini',
                                         min.node.size = 1:2),
                  trControl = myControl)
glmnet_model_property = train(Crime ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                     data = train_property,
                     metric = 'ROC',
                     method = "glmnet",
                     tuneGrid = expand.grid(
                       alpha = 1,
                       lambda = 0
                     ),
                     trControl = myControl)
glmnet_model_assault = train(Crime ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                              data = train_assault,
                              metric = 'ROC',
                              method = "glmnet",
                              tuneGrid = expand.grid(
                                alpha = 1,
                                lambda = 0
                              ),
                              trControl = myControl)

glmnet_model_robbery = train(Crime ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                             data = train_robbery,
                             metric = 'ROC',
                             method = "glmnet",
                             tuneGrid = expand.grid(
                               alpha = 1,
                               lambda = 0
                             ),
                             trControl = myControl)
glmnet_model_shooting = train(Crime ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                             data = train_shooting,
                             metric = 'ROC',
                             method = "glmnet",
                             tuneGrid = expand.grid(
                               alpha = 1,
                               lambda = 0
                             ),
                             trControl = myControl)
glmnet_model_homicide = train(Crime ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                             data = train_homicide,
                             metric = 'ROC',
                             method = "glmnet",
                             tuneGrid = expand.grid(
                               alpha = 1,
                               lambda = 0
                             ),
                             trControl = myControl)
glmnet_model_rape = train(Crime ~ cluster + Day + Month + Weekday + temp + humidity + wind_speed,
                             data = train_rape,
                             metric = 'ROC',
                             method = "glmnet",
                             tuneGrid = expand.grid(
                               alpha = 1,
                               lambda = 0
                             ),
                             trControl = myControl)

model_list <- list(
  glmnet = glmnet_model,
  rf = rf_model
)

#### Compare the two models
resamps <- resamples(model_list)
summary(resamps)
bwplot(resamps, metric = "ROC")

### Calculate AUC with test set 
roc_obj_glm <- roc(test_property$Crime, predict(glmnet_model,test_property, type = 'prob')$Yes)
auc_glm = auc(roc_obj_glm)
roc_obj_rf <- roc(test_property$Crime, predict(rf_model,test_property, type = 'prob')$Yes)
auc_rf = auc(roc_obj_rf)
auc_glm
auc_rf

save(glmnet_model,file = 'glmnet_model.rda')
save(rf_model,file = 'rf_model.rda')
save(glmnet_model_property, file = 'glmnet_model_property.rda')
save(glmnet_model_assault, file = 'glmnet_model_assault.rda')
save(glmnet_model_robbery, file = 'glmnet_model_robbery.rda')
save(glmnet_model_homicide, file = 'glmnet_model_homicide.rda')
save(glmnet_model_shooting, file = 'glmnet_model_shooting.rda')
save(glmnet_model_rape, file = 'glmnet_model_rape.rda')
