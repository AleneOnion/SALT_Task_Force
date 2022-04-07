library(ggmap)
library(ggrepel)
library(rgdal)
library(sp)
library(spatialEco)
library(tidyverse)
sites<-newdata %>% 
  filter(SAMPLE_TYPE=="WATER COLUMN") %>% 
  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,.keep_all = TRUE)
wipwl<-readOGR("C:/Users/leneo/Dropbox/Alene/Rscripts/SALT_Task_Force","Indian_Reservations")

#change coords to web mercator for the map
#all of our layers are NAD83 so have to convert to web mercator
wipwl<-sp::spTransform(wipwl, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

#convert sites to a points layer and then do left join with wipwls layer
coordinates(sites)=~LOCATION_X_COORDINATE+LOCATION_Y_COORDINATE
proj4string(sites)<-CRS("+proj=longlat")
wipwl_merge<-point.in.poly(sites,wipwl)

#convert attribute table to a data frame
wipwl_data<-fortify(wipwl_merge@data)
#generate sites list again
sites2<-newdata %>% 
  filter(SAMPLE_TYPE=="WATER COLUMN") %>% 
  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,.keep_all = TRUE)
sites2<-merge(sites2,wipwl_data,by=c('LOCATION_HISTORY_ID','LAKE_HISTORY_ID'),all=TRUE)
sites2<-sites2 %>% 
  filter(!is.na(ENTITY)) %>% 
  select(LAKE_HISTORY_ID,ENTITY) %>% distinct()

junk<-merge(sites2,newdata,by=c('LAKE_HISTORY_ID'),all.x=TRUE)

junk<-junk %>% filter(is.na(RSLT_VALIDATOR_QUALIFIER)|RSLT_VALIDATOR_QUALIFIER!="R") %>% 
  select(LAKE_WATERBODY_NAME,SAMPLE_DATE,SAMPLE_TIME,INFORMATION_TYPE,
         CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_VALUE,HS_HAB_STATUS,HS_HAB_STATUS_DATE,HS_HAB_STATUS_REMARK) %>% distinct() %>% 
  mutate(INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="BS","Bottom Sample",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="DP","Depth Profile",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="OW","Open Water",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="SD","Secchi Depth",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="RT","HAB Report",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="SB","Shore Bloom",INFORMATION_TYPE))
junk1<-junk %>% filter(is.na(RSLT_PROFILE_DEPTH)) %>% distinct() %>% select(-RSLT_PROFILE_DEPTH) %>% 
  filter(!is.na(RSLT_RESULT_VALUE)) %>% distinct() %>% 
  arrange(LAKE_WATERBODY_NAME,SAMPLE_DATE,INFORMATION_TYPE,CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION)
junk2<-junk %>% filter(!is.na(RSLT_PROFILE_DEPTH),!is.na(RSLT_RESULT_VALUE)) %>% distinct() %>% arrange(RSLT_PROFILE_DEPTH,CHARACTERISTIC_NAME) 
junk3<-junk %>% select(LAKE_WATERBODY_NAME,INFORMATION_TYPE,HS_HAB_STATUS,HS_HAB_STATUS_DATE,HS_HAB_STATUS_REMARK) %>% distinct()
write.csv(junk1,file="data.request/tribal.chemistry.csv",row.names=FALSE)
write.csv(junk2,file="data.request/tribal.profile.csv",row.names=FALSE)
write.csv(junk3,file="data.request/tribal.hab.csv",row.names=FALSE)


#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
#checking 2022 sampling locations
sites<-read.csv("sites_for_map.csv")
sites<-sites %>% 
  rename(LOCATION_X_COORDINATE=X_Coordinate,
         LOCATION_Y_COORDINATE=Y_Coordinate,
         LOCATION_HISTORY_ID=LOCATION_ID) %>% 
  select(LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
wipwl<-readOGR("C:/Users/leneo/Dropbox/Alene/Rscripts/SALT_Task_Force","Indian_Reservations")

#change coords to web mercator for the map
#all of our layers are NAD83 so have to convert to web mercator
wipwl<-sp::spTransform(wipwl, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

#convert sites to a points layer and then do left join with wipwls layer
coordinates(sites)=~LOCATION_X_COORDINATE+LOCATION_Y_COORDINATE
proj4string(sites)<-CRS("+proj=longlat")
wipwl_merge<-point.in.poly(sites,wipwl)

#convert attribute table to a data frame
wipwl_data<-fortify(wipwl_merge@data)
#generate sites list again
sites2<-read.csv("sites_for_map.csv")
sites2<-sites2 %>% 
  rename(LOCATION_X_COORDINATE=X_Coordinate,
         LOCATION_Y_COORDINATE=Y_Coordinate,
         LOCATION_HISTORY_ID=LOCATION_ID) %>% 
  select(LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
sites2<-merge(sites2,wipwl_data,by=c('LOCATION_HISTORY_ID'),all=TRUE)
sites2<-sites2 %>% 
  filter(!is.na(ENTITY)) %>% 
  select(LOCATION_HISTORY_ID,ENTITY) %>% distinct()
