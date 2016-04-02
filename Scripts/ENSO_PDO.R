####################################################################
################ data re: large-scale processes ####################
####################################################################

#### Pacific Decadal Oscillation ####

#(need to add where this data came from)

#read in PDO data
setwd(data_dir)
data.agg.orig <- read.csv("./Data/wisconsin_under_ice_aggregate_lakes.csv", stringsAsFactors = FALSE)

PDO.orig <- read.csv("PDO.csv", stringsAsFactors = FALSE)
PDO<-PDO.orig
PDO<-PDO %>% gather(month,value,-YEAR)
PDO_meanprev<-PDO %>%
  group_by(YEAR=YEAR+1) %>%
  dplyr::summarize(PDO_mean_prevyear=mean(value)) %>% as.data.frame()
PDO_meanprev2<-PDO %>%
  group_by(YEAR=YEAR+2) %>%
  dplyr::summarize(PDO_mean_prevyear2=mean(value)) %>% as.data.frame()
PDO_meanprev3<-PDO %>%
  group_by(YEAR=YEAR+3) %>%
  dplyr::summarize(PDO_mean_prevyear3=mean(value)) %>% as.data.frame()
PDO_meanprev4<-PDO %>%
  group_by(YEAR=YEAR+4) %>%
  dplyr::summarize(PDO_mean_prevyear4=mean(value)) %>% as.data.frame()
PDO_JanFeb<-subset(PDO, PDO$month %in% c("JAN","FEB"))# %>% spread(month,value)
PDO_JanFeb<-PDO_JanFeb %>%
  group_by(YEAR) %>%
  dplyr::summarize(PDO_JanFeb=mean(value)) %>% as.data.frame()
PDO_JunJulAug<-subset(PDO, PDO$month %in% c("JUN","JUL","AUG"))#  %>% gather(month,value,-YEAR)
  PDO_JunJulAugprev<-PDO_JunJulAug %>%
  group_by(YEAR=YEAR+1) %>%
  dplyr::summarize(PDO_JunJulAug_prevyear=mean(value)) %>% as.data.frame()
PDO1<-merge(PDO_meanprev,PDO_JanFeb,by="YEAR")
PDO2<-merge(PDO1,PDO_JunJulAugprev,by="YEAR")
PDO3<-merge(PDO2,PDO_meanprev2,by="YEAR")
PDO4<-merge(PDO3,PDO_meanprev3,by="YEAR")
PDO5<-merge(PDO4,PDO_meanprev4,by="YEAR")
PDO<-PDO5

#### Oceanic Nino Index ####

ONI.orig <- read.csv("ONI.csv", stringsAsFactors = FALSE)
ONI<-ONI.orig
ONI<-ONI %>% gather(month,value,-YEAR)
ONI_meanprev<-ONI %>%
  group_by(YEAR=YEAR+1) %>%
  dplyr::summarize(ONI_mean_prevyear=mean(value)) %>% as.data.frame()
ONI_meanprev2<-ONI %>%
  group_by(YEAR=YEAR+2) %>%
  dplyr::summarize(ONI_mean_prevyear2=mean(value)) %>% as.data.frame()
ONI_meanprev3<-ONI %>%
  group_by(YEAR=YEAR+3) %>%
  dplyr::summarize(ONI_mean_prevyear3=mean(value)) %>% as.data.frame()
ONI_meanprev4<-ONI %>%
  group_by(YEAR=YEAR+4) %>%
  dplyr::summarize(ONI_mean_prevyear4=mean(value)) %>% as.data.frame()
ONI_JanFeb<-subset(ONI, ONI$month %in% c("JAN","FEB"))# %>% spread(month,value)
ONI_JanFeb<-ONI_JanFeb %>%
  group_by(YEAR) %>%
  dplyr::summarize(ONI_JanFeb=mean(value)) %>% as.data.frame()
ONI_JunJulAug<-subset(ONI, ONI$month %in% c("JUN","JUL","AUG"))#  %>% gather(month,value,-YEAR)
ONI_JunJulAugprev<-ONI_JunJulAug %>%
  group_by(YEAR=YEAR+1) %>%
  dplyr::summarize(ONI_JunJulAug_prevyear=mean(value)) %>% as.data.frame()
ONI1<-merge(ONI_meanprev,ONI_JanFeb,by="YEAR")
ONI2<-merge(ONI1,ONI_JunJulAugprev,by="YEAR")
ONI3<-merge(ONI2,ONI_meanprev2,by="YEAR")
ONI4<-merge(ONI3,ONI_meanprev3,by="YEAR")
ONI5<-merge(ONI4,ONI_meanprev4,by="YEAR")
ONI<-ONI5
