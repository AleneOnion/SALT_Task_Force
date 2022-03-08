#Alene Onion
#December 2019
#Script to generate a clean Location table

#general guidance from the ITS staff:
#remove special characters:/n, &
#  NA should be blank
#double quote output

rm(list=setdiff(ls(), c("lab","data")))

library(tidyverse)
setwd("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data")


#LAKE table
location<-read.csv("2019/Open.Refine/Location-Clean-10-29-2019.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)


#scripting from 2019 R script
location$LakeID<-gsub("\\n*","",location$LakeID)
location$LocationID<-gsub("\\n*","",location$LocationID)
#renaming columns so they match
names(location)[names(location)=="LocationID"]<-"LOCATION_ID"
names(location)[names(location)=="LakeID"]<-"LAKE_ID"
#restrict to the necessary fields
location<-unique(location[c('LAKE_ID','LOCATION_ID','LocationName','County','Y_Coordinate','X_Coordinate','Type','DEC.Region',
                            'Horz_Method','Horz_Datum')])

#remove NA location ids because I determined that these don't have information associated with them
location<-location[!is.na(location$LOCATION_ID),]

#remove fields which can be drawn from GIS data layers
#remove:
#County, DEC.Region
location<-unique(location[c('LAKE_ID','LOCATION_ID','LocationName','Type','X_Coordinate','Y_Coordinate','Horz_Method','Horz_Datum')])

#remove carriage returns and what follows in coordinate fields
#remove white spaces
location<-location %>% 
  mutate(X_Coordinate = gsub("\\n*","",X_Coordinate), 
    Y_Coordinate = gsub("\\n*","",Y_Coordinate),
    LAKE_ID = gsub("\\n*","",LAKE_ID),
    LOCATION_ID = gsub("\\n*","",LOCATION_ID), 
    X_Coordinate = trimws(X_Coordinate),
    Y_Coordinate = trimws(Y_Coordinate),
    LAKE_ID = trimws(LAKE_ID),
    LOCATION_ID = trimws(LOCATION_ID),
    LAKE_ID = toupper(LAKE_ID),
    LOCATION_ID = toupper(LOCATION_ID))
#test if it works:
#junk<-location[grepl("\\n",location$X_Coordinate),]

#remove duplicates with one of the pair with NA values for all other fields
location<-location %>% 
  filter(!((LOCATION_ID=='0602EAR0146_E' & is.na(LocationName))|
            (LOCATION_ID=='1101BON0115_P' & is.na(LocationName))| 
            (LOCATION_ID=='1104THI0540_DH' & is.na(LocationName))|
            (LOCATION_ID=='1201KLO0708_O' & is.na(LocationName))|
            (LOCATION_ID=='1301HAR1036_W' & is.na(LocationName))|
            (LOCATION_ID=='1311DUA0276A_C' & is.na(LocationName))|
            (LOCATION_ID=='1501WEE1005_C' & is.na(LocationName))))

#fix type that don't match the name field
location$deephole<-gsub(".*PARKS DEEP HOLE",NA,location$LocationName)
location$deephole<-gsub(".*DEEP HOLE","DEEP HOLE",location$deephole)
location$deephole<-gsub(".*CENTROID","CENTROID",location$deephole)
#make changes
location<-location %>% 
  mutate(Type=ifelse(deephole=='CENTROID','CENTROID',Type),
         Type=ifelse(deephole=='DEEP HOLE','DEEP HOLE',Type))
location$deephole<-NULL


#Convert coordinates to numeric field
#switch coordinates if x is latitude
#round coordinates to nearest 4 digits
#first fix errors
location<-location %>% 
  mutate(X_Coordinate=ifelse(X_Coordinate=="-72.83480070000000240.806429","-72.834800700000002",X_Coordinate),
         Y_Coordinate=ifelse(X_Coordinate=="-72.83480070000000240.806429","40.808200800000002",Y_Coordinate),
         X_Coordinate=ifelse(X_Coordinate=="-74.197991 -74.197827","-74.197991",X_Coordinate),
         X_Coordinate=ifelse(X_Coordinate=="-74.712799099999899-74.713404","-74.712799099999899",X_Coordinate),
         Y_Coordinate=ifelse(X_Coordinate=="42.50170140000000242.498112","42.501701400000002",Y_Coordinate),
         X_Coordinate=ifelse(X_Coordinate=="-75.93548-76.33887","-75.93548",X_Coordinate),
         X_Coordinate=ifelse(X_Coordinate=="-74.574096699999899-74.574554-74.574590","-74.574096699999899",X_Coordinate),
         Y_Coordinate=ifelse(X_Coordinate=="43.35499949999989943.36076943.360850","43.354999499999899",Y_Coordinate),
         X_Coordinate=ifelse(X_Coordinate=="-75.63826600000000244.488576000000002","-75.638266000000002",X_Coordinate),
         Y_Coordinate=ifelse(Y_Coordinate=="40.808200800000002-72.833352","40.808200800000002",Y_Coordinate),
         Y_Coordinate=ifelse(Y_Coordinate=="41.462605,","41.462605",Y_Coordinate),
         Y_Coordinate=ifelse(Y_Coordinate=="42.50170140000000242.498112","42.501701400000002",Y_Coordinate),
         Y_Coordinate=ifelse(Y_Coordinate=="43.339872, -76.703385","43.339872",Y_Coordinate),
         Y_Coordinate=ifelse(Y_Coordinate=="43.35499949999989943.36076943.360850","43.354999499999899",Y_Coordinate))
location$X_Coordinate<-as.numeric(location$X_Coordinate)
location$Y_Coordinate<-as.numeric(location$Y_Coordinate)
location$xbackup<-location$X_Coordinate

location<-location %>% 
  mutate(X_Coordinate=ifelse(X_Coordinate>0,Y_Coordinate,X_Coordinate),
         Y_Coordinate=ifelse(Y_Coordinate<0,xbackup,Y_Coordinate)) %>% 
  group_by(LAKE_ID,LOCATION_ID,LocationName,Type) %>% 
  mutate(X_Coordinate=ifelse(is.numeric(X_Coordinate),signif(X_Coordinate,8),X_Coordinate)) %>% 
  mutate(Y_Coordinate=ifelse(is.numeric(Y_Coordinate),signif(Y_Coordinate,8),Y_Coordinate)) %>% 
  ungroup()
location$xbackup<-NULL

#fix location names that vary slightly
location<-location %>% 
  mutate(LocationName=ifelse(LOCATION_ID=='0503UWB5183_DH','LEVI POND DEEP HOLE',LocationName),
         LocationName=ifelse(LOCATION_ID=='0705SEN0369_3009','SENECA LAKE ZONE 3009',LocationName),
         LocationName=ifelse(LOCATION_ID=='0705SEN0369_3057','SENECA LAKE ZONE 3057',LocationName),
         Type=ifelse(LOCATION_ID=='0706OWA0212_3124','SHORE',Type),
         LocationName=ifelse(LOCATION_ID=='1000CHA0001_BEG','BEGGS PARK BEACH, ESSEX, NY',LocationName),
         LocationName=ifelse(LOCATION_ID=='1000CHA0001_WLS','Cedar Lane, Farrell Bay, and Willsboro Boat Launch',LocationName),
         Type=ifelse(LOCATION_ID=='1201ANN0496_TMDL','CENTROID',Type),
         LocationName=ifelse(LOCATION_ID=='1301HAR1036_C','HARLEM MEER CENTROID',LocationName),
         LocationName=ifelse(LOCATION_ID=='1301OLA0963_P','OLANA POND AT OLANA STATE PARK',LocationName),
         LocationName=ifelse(LOCATION_ID=='1301PUD5194_DH','PUDDING ST. POND DEEP HOLE',LocationName),
         LocationName=ifelse(LOCATION_ID=='1301UWB0420B_DH','FALL KILL POND DEEP HOLE',LocationName),
         LocationName=ifelse(LOCATION_ID=='1306UWB6107_DH','WINDING HILLS PARK LAKE DEEP HOLE',LocationName),
         LocationName=ifelse(LOCATION_ID=='1306WALXXX1_WAL','WALLKILL RIVER ABOVE WALDEN',LocationName),
         Type=ifelse(LOCATION_ID=='1601RUD1134_BH','OTHER',Type),
         LocationName=ifelse(LOCATION_ID=='1701PEC0555_DH','PECONIC LAKE DEEP HOLE',LocationName),
         Type=ifelse(LOCATION_ID=='0402BLAXXX1_C','RIVER/STREAM',Type),
         LocationName=ifelse(LOCATION_ID=='0702OTI0175_3315','OTISCO LAKE ZONE 3315',LocationName),
         LocationName=ifelse(LOCATION_ID=='0702OTI0175_3316','OTISCO LAKE ZONE 3316',LocationName),
         LocationName=ifelse(LOCATION_ID=='1000CHA0001_PER','South Peru and Boat Launch',LocationName),
         LocationName=ifelse(LOCATION_ID=='0705CAY0296_MS','CAYUGA LAKE CSL SITE 3 (MS) (PRE2017)',LocationName),
         LOCATION_ID=ifelse((LOCATION_ID=='1302CRO0059_C' & Type=='OTHER'),'1302CRO0059_NBC',LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=='0705COM0333_W0705COM03330705COM0333','0705COM0333_W',LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=='0201HAR0110A_DH','0201HAR0110A_C',LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=='0201HAR0110A_C'&Type=="DEEP HOLE",'0201HAR0110A_DH',LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=='1702BEA0150A_C'&Type=="OUTLET",'1702BEA0150A_O',LOCATION_ID),
         LAKE_ID= ifelse(LOCATION_ID == "1201STUP498_C","1201STUP498",LAKE_ID),
         LAKE_ID= ifelse(LOCATION_ID == "1702KEN1063_C","1702KEN1063",LAKE_ID))
location<-location[location$LOCATION_ID!='1301BUR0386_NA',]
location<-location[location$LAKE_ID!='0903HIGPS - UNSPECIFIED147',]

#Change type when there are duplicate centroids or deepholes
location<-location %>% 
  mutate(Type=ifelse(LOCATION_ID=='0201CUB0115_CSL_02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0201CUB0115_CSL_03','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0301LON0154_IL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0301LON0154_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0402CON0067_CSL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0402CON0067_CSL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0402CON0067_VIT','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0601GIL0287_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0602BOW5660_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0703CAZ0153_CSL_02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705CAY0296_MN','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705CAY0296_MS','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705CAY0296_N','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705CAY0296_S','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705KEU0388_CSL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705KEU0388_CSL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705KEU0388_CSL3','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705PUN0378A_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705SEN0369_3015','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705SEN0369_CSL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705SEN0369_CSL3','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705SEN0369_CSL4','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705SEN0369_N','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705SEN0369_CSL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0705UWB0378B_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_3107','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_CSL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_CSL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_WIP1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_WIP2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_WIP3','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_WIP4','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_WIP5','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0706OWA0212_WIP6','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0707SKA0193_CSL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0707SKA0193_CSL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0906BLA0001_CSL02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0906BON0024_CSL_02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='0906BUT0054_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1003USA0114_NCSL','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1003USA0114_SCSL','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1004PLA0254_CSL_02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_BB_04','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_CSL01','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_CSL05','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_CSL07','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_CSL08','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_CSL10','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_CSL22','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_GB_23','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_HB_03','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_HEART_24','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_HEART_24','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1101ROU1089_IL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1101ROU1089_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1102BAB1109_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1201ANN0496_TMDL','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1201MAR0570_CSL_02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301CAN0168A_UP','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301HAR1036_NC','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301LAW0207_IL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301LAW0207_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301OLA0963_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0377_Z01','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0379_Z03','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0380_Z04','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0381_Z05','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0382_Z06','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0384_Z07','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0385_Z08','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0386_Z09','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0387_Z10','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0388_Z11','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SNY0389_Z12','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301SWA0030A_PT2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1302CRO0059_N','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1302LIN0057A_CSL_02','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1302WAC0117_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1310NAS0034_IL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1310NAS0034_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1311UWB6877_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1404LAU0382_NNW','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1501ROC0985_IL1','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1501ROC0985_IL2','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1501ROC0985_PDH','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1501STA1011_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1701GEO0780_GPB','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1701HEM1012_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1006GEO0367_NWB_11','OTHER',Type))

#create distinct file but ignore unique coordinates or horiz method which isn't a field we care particularly about
location<-location %>% 
  arrange(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct(LAKE_ID,LOCATION_ID,LocationName,Type,.keep_all = TRUE)

#############################################################################################
#NEXT:  add locations for new additions to lake table

#Generate list of lakes which we need to make lake ids
#find lakes not in the location table
lake<-read.csv("2020/ITS.fixed.tables.comprehensive/L_LAKE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
lake<-lake %>% 
  mutate(LAKE_HISTORY_ID = toupper(LAKE_HISTORY_ID)) %>% 
  rename(LAKE_ID=LAKE_HISTORY_ID)
#missing<-anti_join(lake,location,by=c('LAKE_ID'))



#now enter the list of lakes we generated above and then outside of this program pulled centroids for
missing<-read.csv("2019/Missing_Centroids.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
missing<-missing %>% 
  select(LAKE_ID,Lat,Long) %>% 
  rename(X_Coordinate=Long) %>% 
  rename(Y_Coordinate=Lat) %>% 
  mutate(LAKE_ID = toupper(LAKE_ID))
missing$LOCATION_ID<-NA
missing$LOCATION_ID<-paste(missing$LAKE_ID,'_C',sep="")
missing$LocationName<-"CENTROID"
missing$Type<-"CENTROID"
missing$Horz_Method<-"INTERPOLATION_OTHER"
missing$Horz_Datum<-"NAD83"
missing<-unique(missing[c('LAKE_ID','LOCATION_ID','LocationName','Type','X_Coordinate','Y_Coordinate','Horz_Method','Horz_Datum')])
missing<-distinct(missing)
location<-merge(location,missing,all=TRUE)

############################################################################################################
#Add Locations
############################################################################################################
new<-data.frame(
  LAKE_ID=c('0703ONE0026','0703ONE0026','0703ONE0026'),
  LOCATION_ID=c('0703ONE0026_B109','0703ONE0026_Shack','0703ONE0026_3mile'),
  LocationName=c('Oneida Lake Buoy 109','Oneida Lake Shackelton Point','Oneida Lake Three Mile Bay'),
  Type=c('OTHER','OTHER','OTHER'),
  X_Coordinate=c('-75.7704823651125','-75.9267018456719','-76.0448720120602'),
  Y_Coordinate=c('43.1862035952911','43.1806318508863','43.2212388609007'),
  Horz_Method=c('INTERPOLATION_OTHER','INTERPOLATION_OTHER','INTERPOLATION_OTHER'),
  Horz_Datum=c('NAD83','NAD83','NAD83'))
location<-merge(location,new,all=TRUE)
rm(new)

############################################################################################################
#fix messed up locations
############################################################################################################
#replace NA in horzonatal datum and method to these
location<-location %>% 
  mutate(Horz_Method=ifelse(is.na(Horz_Method),'INTERPOLATION-OTHER',Horz_Method),
         Horz_Datum=ifelse(is.na(Horz_Datum),'NAD83',Horz_Datum))

#old forge lake and third lake
location<-location %>% 
  mutate(LAKE_ID=ifelse(LOCATION_ID=='0801UWB0782_C','0801UWB0782C',LAKE_ID),
        LAKE_ID=ifelse(LOCATION_ID=='0801UWB0782_BEACH_3','0801UWB0782C',LAKE_ID),
        LAKE_ID=ifelse(LOCATION_ID=='0801UWB0782_BEACH_5','0801UWB0782C',LAKE_ID),
        LOCATION_ID=ifelse(LOCATION_ID=='0801UWB0782_C','0801UWB0782C_C',LOCATION_ID),
        LOCATION_ID=ifelse(LOCATION_ID=='0801UWB0782_BEACH_3','0801UWB0782C_BEACH_1',LOCATION_ID),
        LOCATION_ID=ifelse(LOCATION_ID=='0801UWB0782_BEACH_5','0801UWB0782C_BEACH_2',LOCATION_ID),
        LOCATION_ID=ifelse(LOCATION_ID=='0801UWB0782_BEACH_4','0801UWB0782_BEACH_3',LOCATION_ID))
#Other location fixes
location<-location %>% 
  mutate(LAKE_ID=ifelse(LOCATION_ID=='1309SOU0921_DH','1309NOR6398',LAKE_ID),
         LOCATION_ID=ifelse(LOCATION_ID=='1309SOU0921_DH','1309NOR6398_DHS',LOCATION_ID),
         LAKE_ID=ifelse(LOCATION_ID=='1702TAP1601Z_DH','1702TAP1061Z',LAKE_ID),#doesn't impact downstream
         LOCATION_ID=ifelse(LOCATION_ID=='1702TAP1601Z_DH','1702TAP1061Z_DH',LOCATION_ID),#doesn't impact downstream
         LAKE_ID=ifelse(LOCATION_ID=='1201STU0498_C','1201STUP498',LAKE_ID),#doesn't impact downstream
         LOCATION_ID=ifelse(LOCATION_ID=='1201STU0498_C','1201STUP498_C',LOCATION_ID),#doesn't impact downstream
         LAKE_ID=ifelse(LOCATION_ID=='1301BUR0386_BEACH','1301BUR0386C',LAKE_ID),#doesn't impact downstream
         LOCATION_ID=ifelse(LOCATION_ID=='1301BUR0386_BEACH','1301BUR0386C_BEACH',LOCATION_ID),#doesn't impact downstream
         LAKE_ID=ifelse(LOCATION_ID=='0201BEEXXX1_RHL','0201RED0095B',LAKE_ID),#doesn't impact downstream
         LOCATION_ID=ifelse(LOCATION_ID=='0201BEEXXX1_RHL','0201RED0095B_BC',LOCATION_ID),#doesn't impact downstream
         LAKE_ID=ifelse(LOCATION_ID=='0201MCIXXX1_RHL','0201RED0095B',LAKE_ID),#doesn't impact downstream
         LOCATION_ID=ifelse(LOCATION_ID=='0201MCIXXX1_RHL','0201RED0095B_MCC',LOCATION_ID), #doesn't impact downstream
         LocationName=ifelse(LocationName=='CAYUGA LAKE CSL4Â LP (2017-)','CAYUGA LAKE CSLAP 4',LocationName),
         LocationName=ifelse(LOCATION_ID=='0404UWB5318_C','CENTROID',LocationName),
         Type=ifelse(LOCATION_ID=='0404UWB5318_C','CENTROID',Type),
         Type=ifelse(LOCATION_ID=='1101MOR0101_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1301BAR0192B_PDH','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1306AWO0781_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1306MIN0775_P','OTHER',Type),
         Type=ifelse(LOCATION_ID=='1601RUD1134_P','OTHER',Type),
         X_Coordinate=ifelse(LOCATION_ID=='1701WMI0850_OUT','-72.8348007',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1701WMI0850_OUT','40.8082008',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1501WEE1005_SW','-74.19799174',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1501WEE1005_SW','41.21307',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1306MASXXX1_BCR','-74.387089',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1306MASXXX1_BCR','41.462605',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0403AND0164_DH','-77.793602',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0403AND0164_DH','42.1656',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0601TIT0320_OUT','-74.7127991',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0601TIT0320_OUT','42.5017014',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0701GRE5021_DH','-75.93548763',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0701GRE5021_DH','43.21021',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0302LSO0076_DH','-76.7033',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0302LSO0076_DH','43.33987277',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1104SAN0225_OUT','-74.5740967',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1104SAN0225_OUT','43.3549995',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0906BLA0001_BEACH_2','-75.638266',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0906BLA0001_BEACH_2','44.488576',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0201CUB0115_CSL_02','-78.2928',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0201CUB0115_CSL_02','42.2534',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0201CUB0115_CSL_03','-78.2928',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0201CUB0115_CSL_03','42.2534',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0602MEL0039_TRB','-75.8741',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0602MEL0039_TRB','42.4703',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0703CAZ0153_CSL_02','-75.872',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0703CAZ0153_CSL_02','42.9482',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0906BLA0001_CSL02','-75.62636',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0906BLA0001_CSL02','44.473209',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='0906BON0024_CSL_02','-75.3822',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='0906BON0024_CSL_02','44.1475',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1003USA0114_SCSL','-74.3004',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1003USA0114_SCSL','44.3017',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_CSL01','-73.4988',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_CSL01','43.6539',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_CSL07','-73.4988',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_CSL07','43.6539',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_CSL22','-73.4988',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_CSL22','43.6539',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_NWB_11','-73.4988',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1006GEO0367_NWB_11','43.6539',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1201MAR0570_CSL_02','-74.1119',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1201MAR0570_CSL_02','42.8337',Y_Coordinate),
         X_Coordinate=ifelse(LOCATION_ID=='1302LIN0057A_CSL_02','-73.7254',X_Coordinate),Y_Coordinate=ifelse(LOCATION_ID=='1302LIN0057A_CSL_02','41.342',Y_Coordinate),
         LocationName=ifelse(LOCATION_ID=="0705CAY0296_CSL4","CAYUGA LAKE CSLAP (2017-)",LocationName),
         LocationName=ifelse(LOCATION_ID=="0801NIC0804_DECBEACH","NICK'S LAKE CAMPGROUND",LocationName),
         LOCATION_ID=ifelse(LOCATION_ID=="1301OLAXXX2_C1301OLAXXX2_C","1301OLAXXX2_C",LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=="1301OLAXXX3_C1301OLAXXX2_C","1301OLAXXX3_C",LOCATION_ID),
         Type=ifelse(LOCATION_ID=="0901STLXXX1_CC","RIVER/STREAM",Type),
         LAKE_ID=toupper(LAKE_ID),
         LOCATION_ID=toupper(LOCATION_ID),
         Type=toupper(Type),
         Horz_Method=toupper(Horz_Method),
         Horz_Datum=toupper(Horz_Datum)) %>% 
  filter(!(LOCATION_ID %in% c('0302UWB5368_C',
                              '1308LON0887_C',
                              '1201SHAXXX1_PWS',
                              'NA_C',
                              '1006GEO0367_CSL10',
                              '1006GEO0367_CSL08',
                              '1006GEO0367_CSL05'))) %>% #doesn't impact downstream
  arrange(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct(LAKE_ID,LOCATION_ID,Type,.keep_all = TRUE)

  


#write location table 
#write.csv(location, file="2019/ITS.fixed.tables/location.csv", na = "", quote = TRUE, row.names = FALSE)

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#updates from rebecca
#location<-read.csv("2019/ITS.fixed.tables/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
updates<-read.csv("2019/ITS.fixed.tables/updates/locations.add.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
updates<-updates %>% 
  rename(LAKE_ID=LAKE_HISTORY_ID,
         LOCATION_ID=LOCATION_HISTORY_ID,
         X_Coordinate=LOCATION_X_COORDINATE,
         Y_Coordinate=LOCATION_Y_COORDINATE,
         Type=LOCATION_TYPE,
         Horz_Method=LOCATION_HORIZONTAL_METHOD,
         Horz_Datum=LOCATION_HORIZONTAL_DATUM)%>% 
  mutate(LAKE_ID=toupper(LAKE_ID),
         LOCATION_ID=toupper(LOCATION_ID),
         Type=toupper(Type),
         Horz_Method=toupper(Horz_Method),
         Horz_Datum=toupper(Horz_Datum)) %>% 
  select(LAKE_ID,LOCATION_ID,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct()

updates2<-read.csv("2019/ITS.fixed.tables/updates/rebeccas.updatestolocation.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
updates2<-updates2 %>% 
  mutate(LAKE_ID=toupper(LAKE_ID),
         LOCATION_ID=toupper(LOCATION_ID),
         Type=toupper(Type),
         Horz_Method=toupper(Horz_Method),
         Horz_Datum=toupper(Horz_Datum)) %>% 
  select(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct()

#merge together
updates<-merge(updates,updates2,all=TRUE)
rm(updates2)
updates<-updates %>% 
  arrange(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct(LAKE_ID,LOCATION_ID,.keep_all = TRUE)


updatecentroids<-read.csv("2019/ITS.fixed.tables/updates/rebeccas.Centroids_new.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
updatecentroids<-updatecentroids %>% 
  mutate(LAKE_ID=toupper(LAKE_ID),
         LOCATION_ID=paste(LAKE_ID,"_C",sep=""),
         LOCATION_ID=toupper(LOCATION_ID),
         Type="CENTROID",
         LocationName=paste(WATER," Centroid",sep=""),
         Horz_Method="INTERPOLATION",
         Horz_Datum="NAD83") %>% 
  select(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  filter(!is.na(LOCATION_ID),
         LAKE_ID!="0901STLXXX1") %>% 
  distinct()

#first remove those from updates that are in updatecentroids since updatecentroids is more accurate
updates<-anti_join(updates,updatecentroids,by=c('LOCATION_ID'))
#then merge the files
updates<-merge(updates,updatecentroids,all=TRUE)
#removet those locations already in the location table
updates<-anti_join(updates,location,by=c('LOCATION_ID'))
updates<-updates %>% 
  mutate(LAKE_ID=ifelse(LAKE_ID=="1702WOOXX1","1702WOOXXX1",LAKE_ID),
         LOCATION_ID=ifelse(LOCATION_ID=="1702WOOXX1_C","1702WOOXXX1_C",LOCATION_ID)) %>% 
  arrange(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct(LAKE_ID,LOCATION_ID,.keep_all = TRUE)


#merge
location<-merge(location,updates,all=TRUE)

#remove duplicates
location<-location %>% 
  arrange(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct(LAKE_ID,LOCATION_ID,.keep_all = TRUE)


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#Corrections 

#####################################################################################################################
#corrections 10/23/2020
location<-location%>%   
  mutate(X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0704CAN0286_CSL2","-77.344"),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0704CAN0286_CSL2","42.7206"),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0202CHA0122_DH","-79.436259"),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0201ALL5357_C","-78.946871"),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0201ALL5357_C","42.030537"),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1101BAL1090_MAIN","-73.869303"),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1101BAL1090_MAIN","42.913503"),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0202CHA0122_OPRBEACH","-79.412811"),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0202CHA0122_OPRBEACH","42.17215"),
         LocationName=replace(LocationName,LOCATION_ID=="0202CHA0122_OPRBEACH","LONG POINT SP BEACH"),
         LOCATION_ID=ifelse(LOCATION_ID=="1701SAGP0786_C","1701SAG0786_C",LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=="1701SAGP0786_SAGG","1701SAG0786_SAGG",LOCATION_ID),
         LOCATION_ID=ifelse(LOCATION_ID=="1701SAGP0786_SP","1701SAG0786_SP",LOCATION_ID)) %>% 
  filter(!LOCATION_ID %in% c("1307ASH0848_DECBEACH",'0402UWBXXX1_C','1702WOOXX1_C','1702WOOXXX1_C','1201MORXXX1_C')) #removes records that need to be deleted by LOCATION_ID 


#new locations
###NOTE### these will require GIS team to generate the fields
new<-read.csv("2020/newlocations_102320.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
new<-new %>% select(LAKE_ID,LOCATION_ID,LocationName,Y_Coordinate,X_Coordinate,Horz_Method,Horz_Datum,Type) %>% distinct()
new2<-read.csv("2020/new_locations.2021.02.10.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
new2<-new2 %>% rename(LAKE_ID=LAKE_HISTORY_ID,LOCATION_ID=LOCATION_HISTORY_ID,LocationName=LOCATION_NAME,Type=LOCATION_TYPE,Horz_Method=LOCATION_HORIZONTAL_METHOD,Horz_Datum=LOCATION_HORIZONTAL_DATUM,Y_Coordinate=LOCATION_Y_COORDINATE,X_Coordinate=LOCATION_X_COORDINATE) %>% 
  select(LAKE_ID,LOCATION_ID,LocationName,Y_Coordinate,X_Coordinate,Horz_Method,Horz_Datum,Type) %>% distinct()
new<-merge(new,new2,all=TRUE)
rm(new2)
new<-new %>% 
  select(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% 
  distinct() %>% 
  mutate(X_Coordinate=as.character(X_Coordinate),
         Y_Coordinate=as.character(Y_Coordinate))
sapply(new,class)
sapply(location,class)
location<-merge(location,new,by=c('LAKE_ID','LOCATION_ID','LocationName','Type','X_Coordinate','Y_Coordinate','Horz_Method','Horz_Datum'),all=TRUE)

#Stephanie's fixes
location<-location%>%   
  mutate(X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0906BLA0001_CSL02","-75.62636"),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0906BLA0001_CSL02","44.473209"))

#additions 2021.02.11
new<- read.csv("2020/newlakes_02112021.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
new<-new %>% 
  rename(LAKE_ID=LAKE.ID) %>% 
  mutate(Type=ifelse(Type=="Centroid","CENTROID",Type),
         X_C=Y_Coordinate,
         Y_Coordinate=X_Coordinate,
         X_Coordinate=X_C) %>% 
  select(LAKE_ID,LOCATION_ID,LocationName,Type,X_Coordinate,Y_Coordinate,Horz_Method,Horz_Datum) %>% distinct()
location<-merge(location,new,by=c('LAKE_ID','LOCATION_ID','LocationName','Type','X_Coordinate','Y_Coordinate','Horz_Method','Horz_Datum'),all=TRUE)

#fixing duplicate deep holes
location<-location %>% 
  mutate(Type=ifelse(LOCATION_ID=="0202CHA0122_NCSL","OTHER",Type),
         Type=ifelse(LOCATION_ID=="0202CHA0122_SCSL","OTHER",Type),
         Type=ifelse(LOCATION_ID=="0403AND0164_SE","OTHER",Type),
         Type=ifelse(LOCATION_ID=="1309NOR6398_DHS","OTHER",Type),
         Type=ifelse(LOCATION_ID=="1303ORA0239D_SW","OTHER",Type),
         LocationName=ifelse(LocationName=="Unnamed Pond Catskill","UNNAMED WATERBODY (RENSSELAERVILLE) CENTROID",LocationName)) %>% 
  distinct(LAKE_ID,LOCATION_ID,LocationName,Type,.keep_all = TRUE)
#March 2021 fixes
location<-location %>% 
  add_row(LAKE_ID="1309UWBXXX2",LOCATION_ID="1309UWBXXX2_C",LocationName="UNNAMED WATERBODY (RENSSELAERVILLE) CENTROID",Type="CENTROID",X_Coordinate='-74.1422',Y_Coordinate='42.51927',Horz_Method='INTERPOLATION-OTHER',Horz_Datum='NAD83') %>% 
  mutate(Horz_Datum=ifelse(Horz_Datum=="WGS 84","WGS84",Horz_Datum),#fixing mismatches with coded tables
         X_Coordinate=as.numeric(X_Coordinate),
         Y_Coordinate=as.numeric(Y_Coordinate)) 

#Jan 2022 fixes
location<-location %>% 
  mutate(X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0100ERI5000_C",-79.3081),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0100ERI5000_C",42.607368),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0102HAR5128_DH",-78.430702),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0102HAR5128_DH",42.905201),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0201ALL5357_C",-78.946871),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0201ALL5357_C",42.030537),
         Type=replace(Type,LOCATION_ID=="0201RED0095B_BC","INLET"),
         Type=replace(Type,LOCATION_ID=="0201RED0095B_MCC","OUTLET"),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0202CHA0122_DH",-79.436259),
         LocationName=replace(LocationName,LOCATION_ID=="0202CHA0122_OPRBEACH","LONG POINT SP BEACH"),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0202CHA0122_OPRBEACH",-79.412811),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0202CHA0122_OPRBEACH",42.17215),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0301BUC0153_C",-77.668496),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0301BUC0153_C",43.281359),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="0704CAN0286_CSL2",-77.344),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="0704CAN0286_CSL2",42.7206),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1101BAL1090_MAIN",-73.869303),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1101BAL1090_MAIN",42.913503),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1301ALC0185_C",-73.939249),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1301ALC0185_C",42.477828),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1301ALC0185_DH",-73.939249),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1301ALC0185_DH",42.477828),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1305COL0404_DH",-73.7108),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1305COL0404_DH",41.915901),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1501TAP5032_C",-73.995293),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1501TAP5032_C",41.040336),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1601GRA1135A_C",-73.491192),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1601GRA1135A_C",42.017212),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1601IND1131_C",-73.498078),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1601IND1131_C",41.916232),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1601RIG1135B_C",-73.489029),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1601RIG1135B_C",42.022009),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702CON1106V_C",-73.653355),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702CON1106V_C",41.135918),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702JOH1110G_C",-73.496033),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702JOH1110G_C",41.209129),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702MIA1106D_C",-73.614578),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702MIA1106D_C",41.154626),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702SHE1087_DH",40.95204),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702SUS5327_C",-73.585884),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702SUS5327_C",41.165765),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702UWB1109B_C",-73.523516),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702UWB1109B_C",41.262662),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702UWB5160_C",-73.67014),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702UWB5160_C",41.127759),
         X_Coordinate=replace(X_Coordinate,LOCATION_ID=="1702UWB5539_C",-73.703504),
         Y_Coordinate=replace(Y_Coordinate,LOCATION_ID=="1702UWB5539_C",41.111973))


####################################################
#adding 2021 locations
new<- read.csv("2021/newlakes_10_2021.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
new<-new %>% select(-WATER,-County)
colnames(new)[(colnames(new) %in% colnames(location))]
location<-merge(location,new,by=c('LAKE_ID','LOCATION_ID','LocationName','Type','X_Coordinate','Y_Coordinate','Horz_Method','Horz_Datum'),all=TRUE)

####################################################
#add PWLID

library(ggmap)
library(ggrepel)
library(rgdal)
library(sp)
library(spatialEco)
library(tidyverse)


sites<-location %>% select(LAKE_ID,LOCATION_ID,X_Coordinate,Y_Coordinate) %>% distinct()
wipwl<-readOGR("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/data.requests/gis.data.selector","dec_wipwl_lakes")

#change coords to web mercator for the map
#all of our layers are NAD83 so have to convert to web mercator
wipwl<-sp::spTransform(wipwl, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

  #convert sites to a points layer and then do left join with wipwls layer
coordinates(sites)=~X_Coordinate+Y_Coordinate
proj4string(sites)<-CRS("+proj=longlat")
wipwl_merge<-point.in.poly(sites,wipwl)

#convert attribute table to a data frame
wipwl_data<-fortify(wipwl_merge@data)
#generate sites list again
sites2<-location %>% select(LAKE_ID,LOCATION_ID,X_Coordinate,Y_Coordinate) %>% distinct()
sites2<-merge(sites2,wipwl_data,by=c('LOCATION_ID','LAKE_ID'),all=TRUE)
sites2<-sites2 %>% filter(!is.na(PWL_ID)) %>% select(LAKE_ID,LOCATION_ID,PWL_ID) %>% rename(PWLID=PWL_ID) %>% distinct()
rm(list=c("wipwl","wipwl_merge",'sites'))

sites<-sites2

location<-merge(sites,location,by=c('LAKE_ID','LOCATION_ID'),all=TRUE)


#fix those locations where the centroid joined to a PWLID but the shoreline locations didn't
#first identify those that only have one PWLID per lake anyway

#add a buffer only to those lakes and then redo the merge
#first select lakes where at least one location does have a join with the PWLID and at least 1 other does not
sites<-location %>% select(LAKE_ID,PWLID) %>% arrange(PWLID) %>% distinct(LAKE_ID,PWLID,.keep_all=TRUE)
junk<-sites %>% group_by(LAKE_ID) %>% summarize(n=n()) %>% ungroup() %>% filter(n==2) %>% distinct() %>% select(LAKE_ID) %>% distinct()
junk<-as.list(junk$LAKE_ID)
sites<-sites %>% filter(LAKE_ID %in% junk)
#now remove those that genuinely have two PWLIDs and restrict to only the entries that did in fact join
junk<-sites %>% filter(is.na(PWLID)) #if a lake genuinely had two pwlids they wouldn't have an NA row
junk<-as.list(junk$LAKE_ID)
sites<-sites %>% filter(LAKE_ID %in% junk,!is.na(PWLID)) %>% rename(newPWLID=PWLID)
#now join back in
location<-merge(location,sites,by=c('LAKE_ID'),all=TRUE)
location<-location %>% mutate(PWLID=ifelse(!is.na(newPWLID),newPWLID,PWLID)) %>% select(-newPWLID)

#now find the remainder of lakes which have at least one location joined and one not joined
sites<-location %>% select(LAKE_ID,PWLID) %>% arrange(PWLID) %>% distinct(LAKE_ID,PWLID,.keep_all=TRUE)
junk<-sites %>% filter(is.na(PWLID))
junk<-as.list(junk$LAKE_ID)
sites<-sites %>% filter(LAKE_ID %in% junk)
junk<-sites %>% filter(!is.na(PWLID))
junk<-as.list(junk$LAKE_ID)
sites<-sites %>% filter(LAKE_ID %in% junk)
sitesfilter<-sites %>% filter(!is.na(PWLID)) %>% select(PWLID) %>% distinct()
sitesfilter<-as.list(sitesfilter$PWLID)

#Fixing errors in the PWL layer
location<-location %>% 
  mutate(PWLID=ifelse(PWLID=="1104-0013","1104-0113",PWLID)) %>% 
  filter(is.na(PWLID)|PWLID!="0902-0082") %>% 
  distinct()
#still fixing errors - these locations don't overlap the polygons but should
missingpwl<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/missing_PWLs.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
missingpwl<-missingpwl %>% rename(newPWL=PWLID)
location<-merge(location,missingpwl,by=c('LAKE_ID','LOCATION_ID'),all.x=TRUE)
location<-location %>% 
  mutate(PWLID=ifelse(!is.na(newPWL),newPWL,PWLID)) %>% 
  select(-newPWL)


#############################
#now add in waterbody classification
#this is the list from Sarah of classifications for each PWLID
classification<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/data.requests/gis.data.selector/WIPWL.to.Classification.table.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
classification<-classification %>% rename(PWLID=SEG_ID,Waterbody_Classification=CLASS) %>% select(-TYPE) %>% 
  #remove FP or Forest Preserve classification since there are no standards applied to FP
  filter(Waterbody_Classification!="FP")
#add missing classification for lake ontario
classification<-classification %>% mutate(Waterbody_Classification=ifelse(PWLID=="0303-0011","A",Waterbody_Classification))
location<-merge(location,classification,by=c('PWLID'),all.x=TRUE)
location<-location %>% mutate(Waterbody_Classification=ifelse(PWLID=="0303-0011","A",Waterbody_Classification))

#############################
#Now add counties
sites<-location %>% select(LAKE_ID,LOCATION_ID,X_Coordinate,Y_Coordinate) %>% distinct()
wipwl<-readOGR("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/data.requests/gis.data.selector","Counties")

#change coords to web mercator for the map
#all of our layers are NAD83 so have to convert to web mercator
wipwl<-sp::spTransform(wipwl, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))

#convert sites to a points layer and then do left join with wipwls layer
coordinates(sites)=~X_Coordinate+Y_Coordinate
proj4string(sites)<-CRS("+proj=longlat")
wipwl_merge<-point.in.poly(sites,wipwl)

#convert attribute table to a data frame
wipwl_data<-fortify(wipwl_merge@data)
wipwl_data<-wipwl_data %>% select(LAKE_ID,LOCATION_ID,NAME) %>% rename(County=NAME) %>% distinct()
#generate sites list again
sites<-location %>% select(LAKE_ID,LOCATION_ID,X_Coordinate,Y_Coordinate) %>% distinct()
sites<-merge(sites,wipwl_data,by=c('LOCATION_ID','LAKE_ID'),all=TRUE)
sites<-sites %>% filter(!is.na(County)) %>% select(LAKE_ID,LOCATION_ID,County) %>% distinct()
#rm(list=c("wipwl","wipwl_merge",'sites'))


location<-merge(sites,location,by=c('LAKE_ID','LOCATION_ID'),all=TRUE)

#2022.01.06 fixes to location county



write.csv(location,file="C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current/old_database/from.other.files/location.csv",row.names=FALSE)

#############################################################################################
#convert fields to new database
L_LOCATION<-location

fields_changeto<-"L_LOCATION"
fields_now<-"old_database"
input_table<-L_LOCATION
source("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/convert.file.names.between.data.tables.R")
L_LOCATION<-input_table

#####################################################################################################################
#corrections 12/2/2021

new<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2021/newlocations.csv")
new<-new %>% 
  #rename(LOCATION_COUNTY=COUNTY) %>% 
  mutate(LOCATION_Y_COORDINATE=as.numeric(LOCATION_Y_COORDINATE),
         LOCATION_PWL_ID=ifelse(LOCATION_PWL_ID=="",NA,LOCATION_PWL_ID)) %>% 
  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_COUNTY,LOCATION_PWL_ID,LOCATION_NAME,
         LOCATION_TYPE,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,LOCATION_HORIZONTAL_METHOD,
         LOCATION_HORIZONTAL_DATUM) %>% distinct() 
classification<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/data.requests/gis.data.selector/WIPWL.to.Classification.table.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
classification<-classification %>% rename(LOCATION_PWL_ID=SEG_ID,LOCATION_WATERBODY_CLASSIFICATION=CLASS) %>% select(LOCATION_PWL_ID,LOCATION_WATERBODY_CLASSIFICATION) %>% 
  #remove FP or Forest Preserve classification since there are no standards applied to FP
  filter(LOCATION_WATERBODY_CLASSIFICATION!="FP") %>% distinct()
#add missing classification for lake ontario
classification<-classification %>% mutate(LOCATION_WATERBODY_CLASSIFICATION=ifelse(LOCATION_PWL_ID=="0303-0011","A",LOCATION_WATERBODY_CLASSIFICATION))
new<-merge(new,classification,by=c('LOCATION_PWL_ID'),all.x=TRUE)

#add new lakes
L_LOCATION<-merge(L_LOCATION,new,by=c('LAKE_HISTORY_ID','LOCATION_HISTORY_ID','LOCATION_COUNTY','LOCATION_PWL_ID','LOCATION_NAME',
                                      'LOCATION_TYPE','LOCATION_X_COORDINATE','LOCATION_Y_COORDINATE','LOCATION_HORIZONTAL_METHOD',
                                      'LOCATION_HORIZONTAL_DATUM','LOCATION_WATERBODY_CLASSIFICATION'),all=TRUE)

####################################################
#add dec region

sites<-L_LOCATION %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
wipwl<-readOGR("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/data.requests/gis.data.selector","NYS_DEC_Regions")

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
sites2<-L_LOCATION %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
sites2<-merge(sites2,wipwl_data,by=c('LOCATION_HISTORY_ID','LAKE_HISTORY_ID'),all=TRUE)
sites2<-sites2 %>% filter(!is.na(REGION)) %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,REGION) %>% rename(DEC_REGION=REGION) %>% distinct()
rm(list=c("wipwl","wipwl_merge",'sites'))

sites<-sites2

L_LOCATION<-merge(sites,L_LOCATION,by=c('LAKE_HISTORY_ID','LOCATION_HISTORY_ID'),all=TRUE)

####################################################
#add dec region

sites<-L_LOCATION %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
wipwl<-readOGR("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/data.requests/gis.data.selector","Cities_Towns")

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
sites2<-L_LOCATION %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE) %>% distinct()
sites2<-merge(sites2,wipwl_data,by=c('LOCATION_HISTORY_ID','LAKE_HISTORY_ID'),all=TRUE)
sites2<-sites2 %>% filter(!is.na(NAME)) %>% select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,NAME) %>% rename(MUNICIPALITY=NAME) %>% distinct(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,.keep_all = TRUE)
rm(list=c("wipwl","wipwl_merge",'sites'))

sites<-sites2

L_LOCATION<-merge(sites,L_LOCATION,by=c('LAKE_HISTORY_ID','LOCATION_HISTORY_ID'),all=TRUE)

####################################################
#add major basin

L_LOCATION<-L_LOCATION %>% 
  mutate(MAJOR_BASIN_ID=substr(LAKE_HISTORY_ID,1,2))

#####################################################################################################################
#write files

write.csv(L_LOCATION,file="C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/ITS.fixed.tables.comprehensive/L_LOCATION.csv",row.names=FALSE, na = "", quote = TRUE)
write.csv(L_LOCATION,file="C:/Users/amonion/New York State Office of Information Technology Services/BWAM - Lakes Database/Current/new_database/L_LOCATION.csv",row.names=FALSE, na = "", quote = TRUE)


#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#Check that table matches upstream tables
#############################################################################################

rm(list=setdiff(ls(), c("lab","data")))
location<-read.csv("2020/ITS.fixed.tables.comprehensive/L_LOCATION.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
lake<-read.csv("2020/ITS.fixed.tables.comprehensive/L_LAKE.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)

#Testing
#find duplicate location ids
junk<-location %>% 
  group_by(LOCATION_HISTORY_ID) %>% 
  mutate(n=n()) %>% 
  ungroup()
junk<-junk[junk$n>1,]
junk<-junk %>% 
  arrange(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_TYPE,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE)

#find duplicate centroids or deepholes
junk2<-location %>% 
  filter(LOCATION_TYPE %in% c('CENTROID','DEEP HOLE')) %>% 
  group_by(LAKE_HISTORY_ID,LOCATION_TYPE) %>% 
  mutate(n=n()) %>% 
  filter(n>1) %>%
  arrange(LAKE_HISTORY_ID,LOCATION_TYPE,LAKE_HISTORY_ID) %>% 
  ungroup()

#check again
missing1<-anti_join(lake,location,by=c('LAKE_HISTORY_ID'))
missing2<-anti_join(location,lake,by=c('LAKE_HISTORY_ID'))

#Count # characters for the following fields: LOCATION_ID, LocationName, LAKE_ID
lengths<-location %>% 
  select(LOCATION_HISTORY_ID,LOCATION_NAME,LAKE_HISTORY_ID) %>% distinct() %>% 
  mutate(locationlenissue=nchar(LOCATION_HISTORY_ID),
         namelenissue=nchar(LOCATION_NAME),
         idlenissue=nchar(LAKE_HISTORY_ID)) %>% 
  mutate(locationlenissue=as.numeric(locationlenissue),
         namelenissue=as.numeric(namelenissue),
         idlenissue=as.numeric(idlenissue)) %>% 
  mutate(locationlenissue=ifelse(locationlenissue>30,1,0),
         namelenissue=ifelse(namelenissue>100,1,0),
         idlenissue=ifelse(idlenissue>20,1,0)) %>%
  mutate(locationlenissue=as.numeric(locationlenissue),
         namelenissue=as.numeric(namelenissue),
         idlenissue=as.numeric(idlenissue)) %>% 
  mutate(lenissues=rowSums(.[4:6],na.rm=TRUE)) %>% 
  filter(lenissues>0) %>% 
  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_NAME,locationlenissue,namelenissue,idlenissue)
lengths<-colSums(lengths[,4:6],na.rm = TRUE)

#now check that there aren't any NA PWLIDs for locations in lakes that should have them
sites<-location %>% filter(!is.na(LOCATION_PWL_ID))
sites<-as.list(sites$LAKE_HISTORY_ID)
sites<-location %>% filter(LAKE_HISTORY_ID %in% sites,is.na(LOCATION_PWL_ID))
missingpwl<-read.csv("C:/Users/amonion/OneDrive - New York State Office of Information Technology Services/Lakes.Database/data/2020/missing_PWLs.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
missingpwl<-missingpwl %>% rename(LAKE_HISTORY_ID=LAKE_ID,LOCATION_HISTORY_ID=LOCATION_ID) %>% filter(is.na(PWLID)) %>% select(-PWLID) %>% distinct()
missingpwl<-as.list(missingpwl$LOCATION_HISTORY_ID)
sites<-sites %>% filter(!(LOCATION_HISTORY_ID %in% missingpwl))

#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
#Check that file returned from ITS matches
#check that lakes table matches its
#rm(list=setdiff(ls(), "data"))
#location<-read.csv("2020/ITS.fixed.tables.comprehensive/location.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#ITS<-read.csv("2019/ITS.fixed.tables/ITS/L_LOCATION.csv",na.strings=c("","NA"), stringsAsFactors=FALSE)
#library(dplyr)
#library(tidyr)

#truncate to necessary fields
#ITS<-ITS %>% 
#  select(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_NAME,LOCATION_TYPE,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,
#         LOCATION_HORIZONTAL_METHOD,LOCATION_HORIZONTAL_DATUM) 
#location<-location %>% 
#  rename(LAKE_HISTORY_ID=LAKE_ID,
#         LOCATION_HISTORY_ID=LOCATION_ID,
#         LOCATION_NAME=LocationName,
#         LOCATION_TYPE=Type,
#         LOCATION_X_COORDINATE=X_Coordinate,
#         LOCATION_Y_COORDINATE=Y_Coordinate,
#         LOCATION_HORIZONTAL_METHOD=Horz_Method,
#         LOCATION_HORIZONTAL_DATUM=Horz_Datum)

#change to character so can check for diff
#location<-location %>% 
#  mutate(LAKE_HISTORY_ID=as.character(LAKE_HISTORY_ID),
#         LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
#         LOCATION_NAME=as.character(LOCATION_NAME),
#         LOCATION_TYPE=as.character(LOCATION_TYPE),
#         LOCATION_X_COORDINATE=as.character(LOCATION_X_COORDINATE),
#         LOCATION_Y_COORDINATE=as.character(LOCATION_Y_COORDINATE),
#         LOCATION_HORIZONTAL_METHOD=as.character(LOCATION_HORIZONTAL_METHOD),
#         LOCATION_HORIZONTAL_DATUM=as.character(LOCATION_HORIZONTAL_DATUM))
#ITS<-ITS %>% 
#  mutate(LAKE_HISTORY_ID=as.character(LAKE_HISTORY_ID),
#         LOCATION_HISTORY_ID=as.character(LOCATION_HISTORY_ID),
#         LOCATION_NAME=as.character(LOCATION_NAME),
#         LOCATION_TYPE=as.character(LOCATION_TYPE),
#         LOCATION_X_COORDINATE=as.character(LOCATION_X_COORDINATE),
#         LOCATION_Y_COORDINATE=as.character(LOCATION_Y_COORDINATE),
#         LOCATION_HORIZONTAL_METHOD=as.character(LOCATION_HORIZONTAL_METHOD),
#         LOCATION_HORIZONTAL_DATUM=as.character(LOCATION_HORIZONTAL_DATUM))


#check class
#sapply(location,class)
#sapply(ITS,class)

#make sure samples sorted the same
#ITS<-ITS %>% 
#  arrange(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_NAME,LOCATION_TYPE,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,
#          LOCATION_HORIZONTAL_METHOD,LOCATION_HORIZONTAL_DATUM)
#rownames(ITS)<-NULL
#location<-location %>% 
#  arrange(LAKE_HISTORY_ID,LOCATION_HISTORY_ID,LOCATION_NAME,LOCATION_TYPE,LOCATION_X_COORDINATE,LOCATION_Y_COORDINATE,
#          LOCATION_HORIZONTAL_METHOD,LOCATION_HORIZONTAL_DATUM)
#rownames(location)<-NULL


#check if identical
#identical(location,ITS)

#check for differences between specific columns
#test <- lapply(names(location), function(name.i){
#  anti_join(location, ITS, by = name.i)
#})
#names(test) <- names(location)
#test


#test2 <- lapply(names(ITS), function(name.i){
#  anti_join(ITS, location, by = name.i)
#})
#names(test2) <- names(ITS)
#test2
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#List of functions above which might affect downstream data:
#LOCATION_ID = gsub("\\n*","",LOCATION_ID),
#LOCATION_ID = trimws(LOCATION_ID),
#LOCATION_ID = toupper(LOCATION_ID))
#LOCATION_ID=ifelse(LOCATION_ID=='0201HAR0110A_DH','0201HAR0110A_C',LOCATION_ID),
#LOCATION_ID=ifelse((LOCATION_ID=='1302CRO0059_C' & Type=='OTHER'),'1302CRO0059_NBC',LOCATION_ID),
#LOCATION_ID=ifelse(LOCATION_ID=='0201HAR0110A_C'&Type=="DEEP HOLE",'0201HAR0110A_DH',LOCATION_ID),
#LOCATION_ID=ifelse(LOCATION_ID=='1702BEA0150A_C'&Type=="OUTLET",'1702BEA0150A_O',LOCATION_ID),
#LAKE_ID=ifelse(LOCATION_ID=='1309SOU0921_DH','1309NOR6398',LAKE_ID),
#LOCATION_ID=ifelse(LOCATION_ID=='1309SOU0921_DH','1309NOR6398_DHS',LOCATION_ID),
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
