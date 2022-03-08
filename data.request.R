junk<-newdata %>% filter(LAKE_HISTORY_ID %in% c("1301LON0428","1301SEC0427",
                                                "1301MIL0426","1301SHA0424"),
                         #substr(SAMPLE_DATE,1,4)=='2021',
                         is.na(RSLT_VALIDATOR_QUALIFIER)|RSLT_VALIDATOR_QUALIFIER!="R") %>% 
  select(LAKE_WATERBODY_NAME,SAMPLE_DATE,SAMPLE_TIME,INFORMATION_TYPE,
         CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION,RSLT_RESULT_UNIT,RSLT_PROFILE_DEPTH,
         RSLT_RESULT_VALUE) %>% distinct() %>% 
  mutate(INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="BS","Bottom Sample",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="DP","Depth Profile",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="OW","Open Water",INFORMATION_TYPE),
         INFORMATION_TYPE=ifelse(INFORMATION_TYPE=="SD","Secchi Depth",INFORMATION_TYPE))
junk1<-junk %>% filter(is.na(RSLT_PROFILE_DEPTH)) %>% distinct() %>% select(-RSLT_PROFILE_DEPTH) %>% 
  filter(!is.na(RSLT_RESULT_VALUE)) %>% distinct() %>% 
  arrange(LAKE_WATERBODY_NAME,SAMPLE_DATE,INFORMATION_TYPE,CHARACTERISTIC_NAME,RSLT_RESULT_SAMPLE_FRACTION)
junk2<-junk %>% filter(!is.na(RSLT_PROFILE_DEPTH),!is.na(RSLT_RESULT_VALUE)) %>% distinct() %>% arrange(RSLT_PROFILE_DEPTH,CHARACTERISTIC_NAME) 
write.csv(junk1,file="data.request/GraftonSP.chemistry.csv",row.names=FALSE)
write.csv(junk2,file="data.request/GraftonSP.profile.csv",row.names=FALSE)