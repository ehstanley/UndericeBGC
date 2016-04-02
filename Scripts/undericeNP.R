#######################################################################################
######## This codes uses the NTL-LTER data to investigate nutrient ####################
######## trends under lake ice. For a given lake, TDN/TDP/N:P is ######################
######## plotted against days since iceon. Depth is considered in #####################
######## different ways, with portions of code dedicated to determining ###############
######## hypsometrically weighted nutrient concentrations. Additionally, ##############
######## oxygen is investigated in relation to both nutrient concentrations ###########
######## and whole lake trends during iceon.Oxygen too! ##########################################
#######################################################################################

#################### Information about NTL-LTER data used here ########################

#Data was downloaded from the NTL-LTER public download page.

#To begin, we are only interested in chemical attribute.
#https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-chemical-limnology-primary-study-lakes-nutrients-ph-and-carbon-19
#://lter.limnology.wisc.edu/datafile/chemical-limnology-north-temperate-lakes-lter-primary-study-lakes-nutrients-ph-and-carbon

#in SL folder, this is CSV called "chemical_limnology_of_north_temperate_lakes_lter_primary_study_lakes_nutrients_ph_and_carbon_ALL.csv"

#nutrient samples: measured at one station in the deepest part of each lake at the top and bottom of the epilimnion, mid-thermocline, and top, middle, and bottom of the hypolimnion

#Oxygen data: https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-physical-limnology-primary-study-lakes-1981-current
#measured at one station in the deepest part of each lake at 0.25-m to 1-m depth intervals depending on the lake. 

#in SL folder, this is CSV called "physical_limnology_of_the_north_temperate_lakes_primary_study_lakes_ALL.csv"

#looks to generally be same sample dates as nutrients (but different depths - not always the same)

#snow and ice data: https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-snow-and-ice-depth-1982-current

#####################################################
################ set up session #####################
#####################################################

rm(list = ls())

#base_dir<-"/Users/steve.powers/Desktop/Sandboxes/2016Mar8pm_IcePowers"

start_dir<-as.character(getwd())
base_dir<-start_dir
base_dir<-substr(start_dir,1,nchar(start_dir)-nchar("/Scripts"))
data_dir<-paste(base_dir,"/Data",sep="")
figs_dir<-paste(base_dir,"/Figures",sep="")

#base_dir<-"/Users/spowers/Desktop/Sandboxes/2016Mar16"
#data_dir<-paste(c(base_dir,"/DataAsText"),collapse="")

setwd(base_dir)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(tidyr)
#library(lme4)


#######################################################################
############### LTER data - nutrients & physical ######################
#######################################################################

########################## physical data ##############################

data.lter.phys.orig <- read.csv("physical_limnology_of_the_north_temperate_lakes_primary_study_lakes_ALL.csv", 
                                stringsAsFactors = FALSE)
data.lter.phys <- data.lter.phys.orig[which(data.lter.phys.orig$rep==1),]
data.lter.phys$sampledate <- as.Date(data.lter.phys$sampledate, format = "%m/%d/%Y")

#summarize monthly water temp and o2

wtempO2.monthly <- subset(data.lter.phys,data.lter.phys$depth>-1) %>%
  group_by(lakeid,sta,year4,month=as.character(format(sampledate,"%b"))) %>%
  dplyr::summarize(temp_mean=mean(wtemp, na.rm=TRUE),o2_mean=mean(o2sat, na.rm=TRUE))

#sumO2.monthly <- subset(data.lter.phys,data.lter.phys$depth>-1) %>%
#  group_by(lakeid,sta,year4,sampledate) %>%
#  dplyr::summarize(O2_sum=sum(o2)) %>% as.data.frame()

#summarize o2 incorporating depth (??)

sumO2.df <- subset(data.lter.phys,data.lter.phys$depth>-1 & data.lter.phys$o2>0)[order(data.lter.phys$lakeid,data.lter.phys$sampledate,data.lter.phys$sta,data.lter.phys$depth),]
sumO2.df <- sumO2.df %>%
  group_by(lakeid,sta,year4,sampledate) %>%
  dplyr::mutate(int0=depth-lag(depth),int=ifelse(is.na(int0)==TRUE,1,int0)) %>% as.data.frame()
sumO2.df <- sumO2.df %>%
  group_by(lakeid,sta,year4,sampledate) %>%
  dplyr::summarize(O2_sum=sum(o2*int)/max(depth)) %>% as.data.frame()
sumO2.monthly <- sumO2.df %>%
  group_by(lakeid,sta,year4,month=as.character(format(sampledate,"%b"))) %>%
  dplyr::summarize(O2_sum=mean(O2_sum))

#wtempO2.monthly$month<-paste("phys_",wtempO2.monthly$month,sep="")
wtemp.monthly<-wtempO2.monthly 
o2.monthly<-wtempO2.monthly 
wtemp.monthly$month<-paste("wtemp_",wtemp.monthly$month,sep="")
o2.monthly$month<-paste("o2_",o2.monthly$month,sep="")
sumO2.monthly$month<-paste("sumO2_",sumO2.monthly$month,sep="")

wtemp.monthly <- spread(wtemp.monthly %>% select(-o2_mean),key=month,value=temp_mean) %>% as.data.frame()
o2.monthly <- spread(o2.monthly %>% select(-temp_mean),key=month,value=o2_mean) %>% as.data.frame()
sumO2.monthly <- spread(sumO2.monthly,key=month,value=O2_sum) %>% as.data.frame()

#wtemp.monthly<-wtemp.monthly %>% select(-o2_mean)
#o2.monthly<-o2.monthly %>% select(-temp_mean)
wtemp.o2monthly<-merge(wtemp.monthly,o2.monthly,by=c("lakeid","year4","sta"))
wtemp.o2monthly<-merge(wtemp.o2monthly,sumO2.monthly,by=c("lakeid","year4","sta"))


data.lter.phys$lakestaday<-paste(data.lter.phys$lakeid,data.lter.phys$sta,data.lter.phys$sampledate)
lakestadays<-unique(data.lter.phys$lakestaday)

#test<-data.lter.nutrients %>%
#  group_by(lakeid,sta,sampledate,depth) %>% as.data.frame()
#  dplyr::summarize(ldepth)

##############################################################################

do<-0
if(do==1){
lakestaday.df<-c()
layerbound.df<-c()
grad_range.df<-c()
UMLbottom<-c()
deltad<-c()
#o2_sum<-c()
#metadepth<-c()
for(i in 1:length(lakestadays)){
  print(i)
  datai<-subset(data.lter.phys,data.lter.phys$lakestaday==lakestadays[i])
#  datai<-subset(datai,datai$depth>0)
  datai<-datai[order(datai$depth),]
#  wtempi.df<-na.omit(data.frame(datai$depth,datai$wtemp))
  gradi.df<-na.omit(data.frame(datai$depth,datai$o2))
  gradi<-gradi.df[,2]#$wtemp
  grad.rangei<-max(na.omit(gradi))-min(na.omit(gradi))
  depthi<-gradi.df[,1]#datai$depth
  deltadi<-(gradi[-1]-gradi[-length(gradi)])/
  (depthi[-1]-depthi[-length(depthi)])
#  o2_sumi<-NA
#  if(length(na.omit(datai$o2)>0)){
#  interpi<-approx(depthi,gradi,n=100)
#  o2_sumi<-sum((interpi$x[-1]-interpi$x[-length(interpi$x)])*(interpi$y[-1]))
#  }
  mini<-sort(na.omit(deltadi))
  if(length(mini)==1){mini<-c(mini,mini,mini)}
  if(length(mini)==2){mini<-c(mini,mini[1])}
  layerboundi<-mean(depthi[na.omit(which(deltadi %in% mini[1:3]))])
  UMLbottomi<-layerboundi
  if(is.na(layerboundi)==TRUE){layerboundi <- max(datai$depth)}
#  hypodepthi<-max(datai$depth)#-epidepthi
  lakestaday.df<-c(lakestaday.df,lakestadays[i])
  layerbound.df<-c(layerbound.df,layerboundi)
  UMLbottom<-c(UMLbottom,UMLbottomi)
#  hypodepth<-c(hypodepth,hypodepthi)
  grad_range.df<-c(grad_range.df,grad.rangei)
  deltad<-c(deltad,mini[1])
#  o2_sum<-c(o2_sum,o2_sumi)
#  if(length(deltadi)>0){
#  png(file=paste(lakestadays[i],".png"),width=4,height=4,units="in",res=200)
#    plot(datai$o2,datai$depth)
#  dev.off()}
}
mix.df<-data.frame(lakestaday=lakestaday.df,layerbound=layerbound.df,grad_range=grad_range.df,UMLbottom,deltad=deltad)#,o2_sum)
write.csv(mix.df,file="mix.csv")
}

mix<-read.csv("mix.csv")
mix<-mix[,-1]

######################################################################################

############################# nutrient data ##########################################

data.lter.nutrients.orig <- read.csv("chemical_limnology_of_north_temperate_lakes_lter_primary_study_lakes_nutrients_ph_and_carbon_ALL.csv", 
                                     stringsAsFactors = FALSE)

data.lter.nutrients<-data.lter.nutrients.orig[which(data.lter.nutrients.orig$rep==1),]
data.lter.nutrients$sampledate <- as.Date(data.lter.nutrients$sampledate, format = "%m/%d/%Y")


#for now, not worried about whether values are flagged
#also ignoring "sloh" columns - these are basically double checks by another lab
data.lter.nutrients$no3no2[which(nchar(data.lter.nutrients$flagno3no2)>1)]<-NA
data.lter.nutrients$nh4[which(nchar(data.lter.nutrients$flagnh4)>1)]<-NA
data.lter.nutrients$no3no2[which(data.lter.nutrients$flagno3no2=="I")]<-NA
data.lter.nutrients$nh4[which(data.lter.nutrients$flagnh4=="I")]<-NA
data.lter.nutrients <- data.lter.nutrients[-which(data.lter.nutrients$no3no2==data.lter.nutrients$nh4 & data.lter.nutrients$no3no2>300),]

#also note that biocarbonite-reactive silica is only available until 2003
#sampling should be every month during ice-free and every 6 weeks during ice-on


#data.lter.nutrients <- select(data.lter.nutrients, lakeid, year4, sampledate, daynum, depth, sta, ph, phair, alk, dic, tic,
#                    doc, toc, no3no2, no2, nh4, totnf, totnuf, totpf, totpuf, drsif, brsif, brsiuf, tpm)

#data.lter.nutrients <- filter(data.lter.nutrients, lakeid != "FI" & lakeid != "ME" & lakeid != "MO" & lakeid != "WI")

###############################################################################
############ master dataset for LTER - merged nutrient and phys data ##########
############### (including all previously calculated pieces) ##################
###############################################################################

#### merge data sets ####

data.lter.nutrients <- merge(data.lter.nutrients,data.lter.phys,by=c("lakeid","sta","year4","daynum","sampledate","depth","rep"),All=TRUE)
data.lter.nutrients <- merge(data.lter.nutrients,sumO2.df,by=c("lakeid","year4","sta","sampledate"),all=TRUE)
data.lter.nutrients <- merge(data.lter.nutrients,wtemp.o2monthly,by=c("lakeid","year4","sta"),all=TRUE)
data.lter.nutrients <- merge(data.lter.nutrients,mix,by=c("lakestaday"),all=TRUE)

#data.lter.nutrients <- merge(data.lter.nutrients,o2.monthly,all=TRUE)

#### rename lakes from lake id to full name ####

data.lter.nutrients$lakeid <- gsub("AL", "Allequash Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("BM", "Big Muskellunge Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("CB", "Crystal Bog", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("CR", "Crystal Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("SP", "Sparkling Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("TB", "Trout Bog", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("TR", "Trout Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("FI", "Fish Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("ME", "Lake Mendota", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("MO", "Lake Monona", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("WI", "Lake Wingra", data.lter.nutrients$lakeid)

data.lter.nutrients<-subset(data.lter.nutrients,!data.lter.nutrients$lakeid %in% c("Crystal Bog","Trout Bog"))

data.lter.nutrients <- dplyr::rename(data.lter.nutrients, lakename = lakeid, year = year4)
data.lter.nutrients$sampledate <- as.Date(data.lter.nutrients$sampledate, format = "%m/%d/%Y")


#for now, selecting variables there were pre-aggregated from aggregate data set
#could easily go back and re-create seasonal averages for other variables if needed

#data.lter.nutrients <- select(data.lter.nutrients, lakename, year, sampledate, daynum, depth, sta, doc, totnf, totnuf, totpf, totpuf)
#pre-filt for instances where all are !NA (cuts down on memory/time needed for merge later)

#data.lter.nutrients <- data.lter.nutrients %>% filter(!is.na(doc) | !is.na(totnf) | !is.na(totnuf) | !is.na(totpf) | !is.na(totpuf))

data.lter.nutrients$daynum_wateryr<-data.lter.nutrients$daynum+round(365*0.25,digits=0)
data.lter.nutrients$daynum_wateryr[which(data.lter.nutrients$daynum_wateryr>365)]<-365-data.lter.nutrients$daynum_wateryr[which(data.lter.nutrients$daynum_wateryr>365)]

#######################################################################
######## under-ice ecology aggregate data for same lake subset ########
#######################################################################

data.agg<-data.agg.orig

#for now, selecting variables there were pre-aggregated from aggregate data set
#could easily go back and re-create seasonal averages for other variables if needed
#station depth is depth of lake at sample station (depth of sample station)

data.agg.filt <- data.agg %>% select(year, season, lakename, stationlat, stationlong,
                                                                   startday, startmonth, startyear, endday, endmonth, endyear,
                                                                   periodn, sampledepth, photicdepth, avetotdoc, 
                                                                   avetotphos, avetotdissphos, avetotnitro, avetotdissnitro)

#create unique Y-m-d date for start and end to match/merge/filter with NTL-LTER dataset

data.agg.filt$start.month.num <- match(data.agg.filt$startmonth, month.abb)
data.agg.filt$end.month.num <- match(data.agg.filt$endmonth, month.abb)

data.agg.filt <- data.agg.filt %>% 
  mutate(startdate = as.Date(paste(startyear, start.month.num, startday, sep="-"), format = "%Y-%m-%d"),
         enddate = as.Date(paste(endyear, end.month.num, endday, sep="-"), format = "%Y-%m-%d")) %>% 
  select(-year, -startday, - startmonth, - startyear, - endday, - endmonth, -endyear, 
          -periodn, -stationlat, -stationlong)

#remove instances where all vars of interest are NA

data.agg.filt.nutrients <- data.agg.filt %>% filter(!is.na(avetotphos) | !is.na(avetotdissphos) | !is.na(avetotnitro) | !is.na(avetotdissnitro))

#this is the df of interest in terms of seasonal averages
#use a subset - just dates - to limit NTL-LTER dataset to only samples of interest in terms of seasons

data.agg.cut <- data.agg.filt.nutrients %>% select(season, lakename, sampledepth, photicdepth, startdate, start.month.num, enddate, end.month.num)

################################################################################
########### merge and subset LTER data with aggregate same-lake data ###########
############### (same data, just aggregated to seasonal level) #################
################################################################################

#this will be huge and unwieldy to begin with 

data.merge <- merge(data.agg.cut, data.lter.nutrients, by = "lakename")

data.subset <- subset(data.merge, sampledate <= enddate & sampledate >= startdate)

#sample month and year

data.subset <- data.subset %>% mutate(sample.month = as.integer(format(sampledate, "%m")),
                                      sampleyear = as.integer(format(sampledate, "%Y")),
                                      to97 = ifelse(sampleyear <= 1997, "yes", "no"))

#sample date - time since start of iceon (e.g. month diff between start of iceon and sample month)

data.subset.ice <- data.subset %>% 
                    filter(season == "iceon")

#start month num is either 11, 12, or 1
#end month num is 2-5
#try adding 3...adjusting for 12 months total (so +3-12 = -9 for 11 or 12)

data.subset.ice <- data.subset.ice %>% 
                #adjust months
                mutate(days.since.iceon.start = sampledate - startdate,
                              days.since.iceon.start = as.integer(days.since.iceon.start),
                  start.month.num.adj = ifelse(start.month.num == 1, start.month.num + 3, start.month.num - 9),
                       end.month.num.adj = end.month.num +3,
                       sample.month.adj = ifelse(sample.month == 11, sample.month - 9, sample.month + 3),
                #find difference
                       months.since.iceon.start = sample.month.adj - start.month.num.adj)

data.subset.ice$days.since.iceon.start<-as.numeric(data.subset.ice$days.since.iceon.start)

#yep, looks like it works...

data.subset.ice <- data.subset.ice %>% 
                    select(-start.month.num.adj, -end.month.num.adj, - sample.month.adj)

data.subset.ice <- dplyr::rename(data.subset.ice, NO3N=no3no2, NH4N=nh4, TDN=totnf, TN=totnuf, TDP=totpf, TP=totpuf)
data.subset.ice <- dplyr::mutate(data.subset.ice, DIN=NO3N+NH4N, DON=TDN-NO3N-NH4N)

data.N.iceon<-data.subset.ice

########################################################################################
#### Version 1: aggregating over ENTIRE *SAMPLED* WATER COLUMN ####
#average entire water column for sample date
#data.ice.sub.agg <- data.subset.ice %>% 
 #                 #already all iceon season
  #                group_by(lakename, sampledate) %>% 
  #                mutate(doc.depth.avg = mean(doc, na.rm = T),
  #                       totnf.depth.avg = mean(TDN, na.rm = T),
   #                      totnuf.depth.avg = mean(TN, na.rm = T),
    #                     totpf.depth.avg = mean(TDP, na.rm = T),
     #                    totpuf.depth.avg = mean(TP, na.rm = T)) %>% 
      #            #remove extraneous columns so can get unique rows
       #           select(-photicdepth, -depth, -sta, -doc, -TDN, -TN, -TDP, -TP) %>% 
        #          unique() %>% 
         #         as.data.frame()

#this has values averaged over entire *SAMPLED* water column
#########################################################################################

#####################
#check low o2 dates for Trout and Sparkling
O2check<-data.N.iceon %>% 
  group_by(lakename,year) %>%
  dplyr::summarize(minO2=min(O2_sum,na.rm=TRUE)) %>% as.data.frame()

#Trout Lake 1995  5.321429
#Sparkling Lake 2011  3.168750

O2check2<- subset(data.N.iceon, data.N.iceon$O2_sum==5.321429 | data.N.iceon$O2_sum==3.168750)
unique(data.frame(O2check2$lakename,O2check2$sampledate))

O2check3<- subset(data.N.iceon, data.N.iceon$O2_sum<5.5)
unique(data.frame(O2check3$lakename,O2check3$sampledate,O2check3$O2_sum))
#91     Sparkling Lake          2011-01-18        3.168750
#108        Trout Lake          1995-03-21        5.321429

lowO2SP<-subset(data.lter.phys,data.lter.phys$lakeid=="SP" & data.lter.phys$sampledate=="2011-01-18")
lowO2TR<-subset(data.lter.phys,data.lter.phys$lakeid=="TR" & data.lter.phys$sampledate=="1995-03-21")

lowO2<-rbind(lowO2SP,lowO2TR)
setwd(base_dir)
write.csv(lowO2,"lowO2.csv")
setwd()
#############################################################################
############### N type breakdown; P - by depth sections #####################
#############################################################################

#data.N.iceon[which(data.N.iceon$DON>300 & data.N.iceon$DON/data.N.iceon$TDN>0.8),]
#data.N.iceon <- 
data.N.iceon$DON[which(data.N.iceon$DON>400)]<-NA
data.N.iceon$TDN[which(data.N.iceon$TDN>1000)]<-NA

data.N.iceon$upperlayerTF <- as.numeric(data.N.iceon$depth<data.N.iceon$UMLbottom)
data.N.iceon[which(data.N.iceon$lakename=="Allequash Lake"),]$maxdepth<-7
data.N.iceon[which(data.N.iceon$lakename=="Big Muskellunge Lake"),]$maxdepth<-19
data.N.iceon[which(data.N.iceon$lakename=="Crystal Lake"),]$maxdepth<-19
data.N.iceon[which(data.N.iceon$lakename=="Sparkling Lake"),]$maxdepth<-18
data.N.iceon[which(data.N.iceon$lakename=="Trout Lake"),]$maxdepth<-32
data.N.iceon$bottomlayerTF <- as.numeric(data.N.iceon$depth>data.N.iceon$UMLbottom)
data.N.iceon$middlelayerTF <- as.numeric(data.N.iceon$depth==data.N.iceon$UMLbottom)

getmaxdepth<-data.N.iceon %>% 
  group_by(lakename,year,sampledate) %>%
  dplyr::summarize(maxdepth=max(depth))
data.N.iceon<-merge(data.N.iceon,getmaxdepth,by=c("lakename","year","sampledate"),all=TRUE)

#find N type breakdown and P for "shallow" depths (<UMLbottom)
data.NPtype.shallow <- filter(data.N.iceon, depth<UMLbottom)  %>% #for photic depths (no avering or anything)
                        group_by(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
                                 maxdepth, upperlayerTF,bottomlayerTF, 
                                 O2_sum,sumO2_Nov,sumO2_Dec,sumO2_Jan,sumO2_Oct,sumO2_Sep,o2,
                                 wtemp_Oct,wtemp_Aug,wtemp_Jul,wtemp_Jun,o2_Oct,o2_Aug,o2_Jul,
                                 o2_Jun,UMLbottom,grad_range) %>%
                        #find average N-type and P
                        dplyr::summarize(NO3N=mean(NO3N,na.rm=TRUE), 
                                         NH4N=mean(NH4N,na.rm=TRUE),
                                         DIN=mean(DIN,na.rm=TRUE),
                                         DON=mean(DON,na.rm=TRUE),
                                         TDN=mean(TDN,na.rm=TRUE),
                                         TDP=mean(TDP,na.rm=TRUE),
                                         TN=mean(TN,na.rm=TRUE),
                                         TP=mean(TP,na.rm=TRUE)) %>%
                        #select columns of interest
                        select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
                               maxdepth, upperlayerTF,bottomlayerTF,NO3N, NH4N, DIN, DON, TDN, 
                               TDP, TN, TP, o2,wtemp_Oct,wtemp_Aug,wtemp_Jul,wtemp_Jun,o2_Oct,
                               o2_Aug,o2_Jul,o2_Jun,UMLbottom,grad_range)

#find N type breakdown and P for "deep" depths (>UMLbottom)
data.NPtype.deep <- filter(data.N.iceon, depth>UMLbottom)  %>% 
                    group_by(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
                             maxdepth, upperlayerTF,bottomlayerTF, O2_sum,sumO2_Nov,sumO2_Dec,
                             sumO2_Jan,sumO2_Oct,sumO2_Sep,o2,wtemp_Oct,wtemp_Aug,wtemp_Jul,
                             wtemp_Jun,o2_Oct,o2_Aug,o2_Jul,o2_Jun,UMLbottom,grad_range) %>%
                    #find average N-type and P
                    dplyr::summarize(NO3N=mean(NO3N,na.rm=TRUE), 
                                     NH4N=mean(NH4N,na.rm=TRUE),
                                     DIN=mean(DIN,na.rm=TRUE),
                                     DON=mean(DON,na.rm=TRUE),
                                     TDN=mean(TDN,na.rm=TRUE),
                                     TDP=mean(TDP,na.rm=TRUE),
                                     TN=mean(TN,na.rm=TRUE),
                                     TP=mean(TP,na.rm=TRUE)) %>%
                    #select columns of interest
                    select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
                           maxdepth, upperlayerTF,bottomlayerTF,NO3N, NH4N, DIN, DON, TDN, TDP, 
                           TN, TP, o2,wtemp_Oct,wtemp_Aug,wtemp_Jul,wtemp_Jun,o2_Oct,o2_Aug,
                           o2_Jul,o2_Jun,UMLbottom,grad_range)

#calculate dissolved N:P (DIN:TDP) stoichiometry for shallow and deep
data.NPtype.shallow$NP_diss<-(data.NPtype.shallow$DIN/14)/(data.NPtype.shallow$TDP/31)
data.NPtype.shallow$NP_diss[which(data.NPtype.shallow$TDP==0)]<-(data.NPtype.shallow$DIN[which(data.NPtype.shallow$TDP==0)]/14)/(0.5/31)
data.NPtype.deep$NP_diss<-(data.NPtype.deep$DIN/14)/(data.NPtype.deep$TDP/31)
data.NPtype.deep$NP_diss[which(data.NPtype.deep$TDP==0)]<-(data.NPtype.deep$DIN[which(data.NPtype.deep$TDP==0)]/14)/(0.5/31)

#find N type breakdown and P - hypsometrically-weighted
data.NPtype.hyps <- data.N.iceon  %>% #for photic depths (no avering or anything)
                    select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
                           depth, maxdepth, upperlayerTF,bottomlayerTF, middlelayerTF, NO3N, 
                           NH4N, DIN, DON, TDN, TDP, TN, TP, O2_sum,sumO2_Nov,sumO2_Dec,
                           sumO2_Jan,sumO2_Oct,sumO2_Sep,o2,wtemp_Oct,wtemp_Aug,wtemp_Jul,
                           wtemp_Jun,o2_Oct,o2_Aug,o2_Jul,o2_Jun,UMLbottom,grad_range)

data.NPtype.hyps<- data.NPtype.hyps %>% 
                   select(-depth) %>%
                   group_by(lakename, year,sampledate,days.since.iceon.start,maxdepth,UMLbottom,
                            grad_range,O2_sum, sumO2_Nov, sumO2_Dec, sumO2_Jan, sumO2_Oct, sumO2_Sep) %>% 
  
                   #summarize - mean N type weighted by depth
                   dplyr::summarize(
                    ######## NO3N ########
                    NO3N.wt.middle = mean(NO3N*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    NO3N.wt.shallow = ifelse(NO3N.wt.middle>0,mean(NO3N*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(NO3N*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    NO3N.wt.deep = ifelse(NO3N.wt.middle>0,mean(NO3N*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(NO3N*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    NO3N = NO3N.wt.shallow+NO3N.wt.deep+NO3N.wt.middle,
                   
                    ######## NH4H ########
                    NH4N.wt.middle = mean(NH4N*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    NH4N.wt.shallow = ifelse(NH4N.wt.middle>0,mean(NH4N*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(NH4N*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    NH4N.wt.deep = ifelse(NH4N.wt.middle>0,mean(NH4N*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(NH4N*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    NH4N = NH4N.wt.shallow+NH4N.wt.deep+NH4N.wt.middle,
                   
                    ######## DIN ########
                    DIN.wt.middle = mean(DIN*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    DIN.wt.shallow = ifelse(DIN.wt.middle>0,mean(DIN*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(DIN*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    DIN.wt.deep = ifelse(DIN.wt.middle>0,mean(DIN*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(DIN*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    DIN = DIN.wt.shallow+DIN.wt.deep+DIN.wt.middle,
                   
                    ######## TDN ########
                    TDN.wt.middle = mean(TDN*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    TDN.wt.shallow = ifelse(TDN.wt.middle>0,mean(TDN*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(TDN*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    TDN.wt.deep = ifelse(TDN.wt.middle>0,mean(TDN*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(TDN*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    TDN = TDN.wt.shallow+TDN.wt.deep+TDN.wt.middle,
                   
                    ######## TN ########
                    TN.wt.middle = mean(TN*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    TN.wt.shallow = ifelse(TN.wt.middle>0,mean(TN*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(TN*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    TN.wt.deep = ifelse(TN.wt.middle>0,mean(TN*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(TN*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    TN = TN.wt.shallow+TN.wt.deep+TN.wt.middle,
                   
                    ######## DON ########
                    DON.wt.middle = mean(DON*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    DON.wt.shallow = ifelse(DON.wt.middle>0,mean(DON*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(DON*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    DON.wt.deep = ifelse(DON.wt.middle>0,mean(DON*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(DON*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    DON = DON.wt.shallow+DON.wt.deep+DON.wt.middle,
                   
                    ######## TDP ########
                    TDP.wt.middle = mean(TDP*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    TDP.wt.shallow = ifelse(TDP.wt.middle>0,mean(TDP*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(TDP*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    TDP.wt.deep = ifelse(TDP.wt.middle>0,mean(TDP*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(TDP*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    TDP = TDP.wt.shallow+TDP.wt.deep+TDP.wt.middle,
                   
                    ######## TP ########
                    TP.wt.middle = mean(TP*middlelayerTF/maxdepth,na.rm=TRUE),
                   
                    TP.wt.shallow = ifelse(TP.wt.middle>0,mean(TP*(UMLbottom-0.5)*upperlayerTF/maxdepth,na.rm=TRUE),
                                            mean(TP*UMLbottom*upperlayerTF/maxdepth,na.rm=TRUE)),
                   
                    TP.wt.deep = ifelse(TP.wt.middle>0,mean(TP*bottomlayerTF*(maxdepth-(UMLbottom-0.5))/maxdepth,na.rm=TRUE),
                                         mean(TP*bottomlayerTF*(maxdepth-UMLbottom)/maxdepth,na.rm=TRUE)),
                   
                    TP = TP.wt.shallow+TP.wt.deep+TP.wt.middle
            ) %>% 
            as.data.frame()

#(if # out divide by depth, remove entire section here...)
data.NPtype.hyps$NO3N<-data.NPtype.hyps$NO3N#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$NH4N<-data.NPtype.hyps$NH4N#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$DIN<-data.NPtype.hyps$DIN#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$TDN<-data.NPtype.hyps$TDN#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$DON<-data.NPtype.hyps$DON#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$TN<-data.NPtype.hyps$TN#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$TDP<-data.NPtype.hyps$TDP#/data.NPtype.hyps$maxdepth
data.NPtype.hyps$TP<-data.NPtype.hyps$TP#/data.NPtype.hyps$maxdepth

#stoichiometry calculations
data.NPtype.hyps$NP_diss<-(data.NPtype.hyps$DIN/14)/(data.NPtype.hyps$TDP/31)
data.NPtype.hyps$NP_diss[which(data.NPtype.hyps$TDP==0)]<-(data.NPtype.hyps$DIN[which(data.NPtype.hyps$TDP==0)]/14)/(0.5/31)

#name "type" (e.g. shallow, deeph, hypsometrically-weighted whole lake)
data.NPtype.shallow$method<-"shallow"
data.NPtype.deep$method<-"deep"
data.NPtype.hyps$method<-"whole (hyps-wt)"

############################################################
####################### plotting ###########################
############################################################

#subset to exclude certain lakes
dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.Nhyps <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps

dataplot<-rbind(data.NPtype.shallow,data.NPtype.deep)
#dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]
#00BA38 #00BFC4
plot.Nform.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value,group=method,colour=method)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38"))+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())
plot.Nform.depth

dataplot<-plyr::rbind.fill(data.NPtype.deep,data.NPtype.shallow,data.NPtype.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("DIN")))
dataplot$value[which(dataplot$value==0)]<-0.5
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.DINcombine.depths <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.DINcombine.depths

dataplot<-plyr::rbind.fill(data.NPtype.deep,data.NPtype.shallow,data.NPtype.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TDP")))
dataplot$value[which(dataplot$value==0)]<-0.5
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.TDPcombine.depths <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.TDPcombine.depths

dataplot<-plyr::rbind.fill(data.NPtype.hyps,data.NPtype.deep,data.NPtype.shallow)
#dataplot<-data.NPtype.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NP_diss")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.NPcombine <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value),color=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - NP")+
  geom_abline(intercept=log10(15),slope=0,linetype=2)+
  geom_abline(intercept=log10(7.5),slope=0,linetype=2)
plot.NPcombine

dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(depth,value,which(names(dataplot) %in% c("UMLbottom","maxdepth")))
dataplot$depth[which(dataplot$depth=="UMLbottom")]<-"upper layer"

plot.layers <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value,group=depth,color=depth)) +
  geom_point()+#shape=2,color="green")+#aes(color="green"))+#aes(color = wtemp_range)) +
  scale_colour_manual(values=c("darkgray","#00BA38"))+
  scale_y_reverse(lim=c(32,0))+ylab("Depth (m)")+xlab("Days since iceon")+xlim(0,150)+
  #  geom_point(aes(x=days.since.iceon.start, y=maxdepth),color="blue")+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "O2 mg/L")+
  #    geom_smooth(aes(color = variable)) +
  facet_grid(~lakename,scales="free_y") +
  theme(strip.text.x=element_text())
plot.layers

##################################################
############# print figs to png ##################
##################################################

setwd(figs_dir)

png(file="layers.png",width=12,height=4,units="in",res=300)
print(plot.layers)
dev.off()

png(file="Nform.depth.png",width=8,height=6,units="in",res=300)
print(plot.Nform.depth)
dev.off()

png(file="Nhyps.png",width=8,height=6,units="in",res=300)
print(plot.Nhyps)
dev.off()

png(file="DINcombine.depths.png",width=7,height=7,units="in",res=300)
print(plot.DINcombine.depths)
dev.off()

png(file="TDPcombine.depth.png",width=7,height=7,units="in",res=300)
print(plot.TDPcombine.depths)
dev.off()

png(file="NPcombine.png",width=7,height=7,units="in",res=300)
print(plot.NPcombine)
dev.off()

##################################################

dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN","TDP")))

plot.NPhypsO2 <-  ggplot(dataplot, aes(x=O2_sum, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPhypsO2

plot.NPhypsO2.lumplakes <-  ggplot(dataplot, aes(x=O2_sum, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_wrap(~form,ncol=3,scales="free") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPhypsO2.lumplakes

plot.hypsO2 <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=O2_sum)) +
  geom_point(color="#619CFF") + ylab("O2 Conc (mg/L))") + xlab("Days since iceon")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_wrap(~lakename,ncol=3) +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.hypsO2

dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NP_diss")))

plot.NPratio.hypsO2 <-  ggplot(dataplot, aes(x=O2_sum, y=log10(value))) +
  geom_point(color="#619CFF") + ylab("Log10 Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_wrap(~lakename,ncol=3)+#,scales="free") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPratio.hypsO2

dataplot<-rbind(data.NPtype.shallow,data.NPtype.deep)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN","TDP")))
#00BA38 #00BFC4
plot.NPO2.depth <-  ggplot(dataplot, aes(x=O2_sum, y=value,group=method,colour=method)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  scale_colour_manual(values=c("darkgray","#00BA38"))+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())
plot.NPO2.depth

dataplot<-data.NPtype.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN","TDP")))

plot.NPdeepO2 <-  ggplot(dataplot, aes(x=O2_sum, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPdeepO2

dataplot<-data.NPtype.shallow
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN","TDP")))

plot.NPshallowO2 <-  ggplot(dataplot, aes(x=O2_sum, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPshallowO2

############################################################
################# print figs to png ########################
############################################################

png(file="NPhypsO2.png",width=8,height=8,units="in",res=300)
print(plot.NPhypsO2)
dev.off()

png(file="NPhypsO2.lumplakes.png",width=8,height=5.5,units="in",res=300)
print(plot.NPhypsO2.lumplakes)
dev.off()

png(file="hypsO2.png",width=8,height=5.5,units="in",res=300)
print(plot.hypsO2)
dev.off()

png(file="NPO2.depth.png",width=8,height=6,units="in",res=300)
print(plot.NPO2.depth)
dev.off()

png(file="NPdeepO2.png",width=8,height=6,units="in",res=300)
print(plot.NPdeepO2)
dev.off()

png(file="NPshallowO2.png",width=8,height=8,units="in",res=300)
print(plot.NPshallowO2)
dev.off()

png(file="NPratio.hypsO2.png",width=8,height=8,units="in",res=300)
print(plot.NPratio.hypsO2)
dev.off()

#############################################################


dataplot<-data.NPtype.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.Nhyps <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value)) +
  geom_point(color="#619CFF") + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps



dataplot<-data.NPtype.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.Nhyps <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("Days since iceon")+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps



#dataplot<-rbind(data.NPtype.shallow,data.NPtype.deep)
dataplot<-data.NPtype.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Trout Lake","Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]
#00BA38 #00BFC4
plot.Nform.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value,colour=sumO2_Nov)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("Days since iceon")+
#  scale_colour_manual(values=c("darkgray","#00BA38"))+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())
plot.Nform.depth

plot.Nform.depth <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value,colour=sumO2_Nov)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("Days since iceon")+
  #  scale_colour_manual(values=c("darkgray","#00BA38"))+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())
plot.Nform.depth


dataplot<-plyr::rbind.fill(data.NPtype.deep,data.NPtype.shallow,data.NPtype.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("DIN")))
dataplot$value[which(dataplot$value==0)]<-0.5
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.DINcombine.depths <-  ggplot(dataplot, aes(x=sumO2_Nov, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.DINcombine.depths

dataplot<-plyr::rbind.fill(data.NPtype.deep,data.NPtype.shallow,data.NPtype.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TDP")))
dataplot$value[which(dataplot$value==0)]<-0.5
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.TDPcombine.depths <-  ggplot(dataplot, aes(x=sumO2_Nov, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.TDPcombine.depths

dataplot<-plyr::rbind.fill(data.NPtype.hyps,data.NPtype.deep,data.NPtype.shallow)
#dataplot<-data.NPtype.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NP_diss")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]

plot.NPcombine <-  ggplot(dataplot, aes(x=sumO2_Nov, y=log10(value),color=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=sumO2_Nov, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - NP")+
  geom_abline(intercept=log10(15),slope=0,linetype=2)+
  geom_abline(intercept=log10(7.5),slope=0,linetype=2)
plot.NPcombine

##########################
##########################

dataplot<-plyr::rbind.fill(data.NPtype.deep,data.NPtype.shallow,data.NPtype.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TDN")))
#dataplot$value[which(dataplot$value==0)]<-0.5
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]


dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.o2 <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value,group=method,color=method)) +
  geom_point(colour="darkgray") + ylab("Conc (ug/L)") + xlab("sum O2 Nov")+#xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_wrap(lakename~method,scales="free",ncol=4)+#,ncol=3) +
  theme(strip.text.x=element_text())
plot.o2

dataplot<-rbind(data.NPtype.shallow,data.NPtype.deep)
#dataplot<-data.NPtype.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
#dataplot<-dataplot[-which(is.na(dataplot$value)==TRUE),]
#00BA38 #00BFC4


plot.o2 <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value)) +
  geom_point(colour="darkgray") + ylab("Conc (ug/L)") + xlab("sum O2 Nov")+#xlim(0,150)+
#  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  #  geom_text(aes(color=o2,x=days.since.iceon.start, y=value,label=substr(year,3,4))) +
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_wrap(lakename~form,scales="free",ncol=4)+#,ncol=3) +
  theme(strip.text.x=element_text())
plot.o2


###########################
############################

