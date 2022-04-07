junk<-newdata %>% 
  filter(DATA_PROVIDER=="LCI",substr(SAMPLE_DATE,1,4)=='2021') %>% 
  select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME) %>% distinct() %>% arrange(LAKE_WATERBODY_NAME)

junk<-newdata %>% filter(LAKE_HISTORY_ID=="0601ARN0362",
                         substr(SAMPLE_DATE,1,4)=='2021',
                         is.na(RSLT_VALIDATOR_QUALIFIER)|RSLT_VALIDATOR_QUALIFIER!="R") %>% 
  select(LAKE_WATERBODY_NAME,SAMPLE_DATE,SAMPLE_TIME,INFORMATION_TYPE,
         CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_VALUE) %>% distinct() %>% 
  mutate(INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="BS","Bottom Sample",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="DP","Depth Profile",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="OW","Open Water",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="SD","Secchi Depth",INFORMATION_TYPE))
junk1<-junk %>% filter(is.na(RSLT_PROFILE_DEPTH)) %>% distinct() %>% select(-RSLT_PROFILE_DEPTH)
junk2<-junk %>% filter(!is.na(RSLT_PROFILE_DEPTH)) %>% distinct() %>% arrange(RSLT_PROFILE_DEPTH,CHARACTERISTIC_NAME)
write.csv(junk1,file="arnold.lake.chemistry.csv",row.names=FALSE)
write.csv(junk2,file="arnold.lake.profile.csv",row.names=FALSE)


rmarkdown::render("C:/Users/leneo/Dropbox/Alene/Rscripts/SALT_Task_Force/Chloride_analysis_Charles_Stoll_paper.Rmd")

rmarkdown::render("C:/Users/leneo/Dropbox/Alene/Rscripts/SALT_Task_Force/Salt_task_force_flowing_ponded_ground_water_summary.Rmd")

newdata %>% 
  filter(!is.na(LOCATION_WATERBODY_CLASSIFICATION)) %>% 
  mutate(LOCATION_WATERBODY_CLASSIFICATION=ifelse(grepl("A",LOCATION_WATERBODY_CLASSIFICATION),"A","non-A")) %>% 
  select(LAKE_HISTORY_ID,LOCATION_WATERBODY_CLASSIFICATION)  %>% 
  distinct() %>% 
  group_by(LOCATION_WATERBODY_CLASSIFICATION) %>% 
  summarize(n=n()) %>% 
  ungroup()
  

draft<-newdata %>% 
  filter(LAKE_HISTORY_ID=="0702ONO0154",
         SAMPLE_DATE>'2011-01-01',
         RSLT_VALIDATOR_QUALIFIER!="R"|is.na(RSLT_VALIDATOR_QUALIFIER)) %>% 
  mutate(RSLT_RESULT_VALUE=ifelse(RSLT_VALIDATOR_QUALIFIER=="U",RSLT_METHOD_DETECTION_LIMIT/2,RSLT_RESULT_VALUE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="OW","Integrated Surfaces Sample",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="DP","Depth Profile",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="BS","Bottom Sample",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="SD","Secchi Disk Depth",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="SB","Shore Bloom Sample",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="RT","HAB Report",INFORMATION_TYPE)) %>% 
  filter(!is.na(RSLT_RESULT_VALUE)) %>% distinct() %>% 
  select(LAKE_HISTORY_ID,LAKE_WATERBODY_NAME,LOCATION_HISTORY_ID,LOCATION_NAME,
         LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,SAMPLE_DATE,INFORMATION_TYPE,RSLT_PROFILE_DEPTH,
         CHARACTERISTIC_NAME,RSLT_RESULT_VALUE,RSLT_RESULT_UNIT) %>% distinct()
write.csv(draft,file="Oneida.csv",row.names=FALSE)