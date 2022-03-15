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
