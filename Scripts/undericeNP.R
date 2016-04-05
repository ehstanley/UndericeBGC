#######################################################################################
######## This codes uses the NTL-LTER data to investigate nutrient ####################
######## trends under lake ice. For a given lake, TDN/TDP/N:P is ######################
######## plotted against days since iceon. Depth is considered in #####################
######## different ways, with portions of code dedicated to determining ###############
######## hypsometrically weighted nutrient concentrations. Additionally, ##############
######## oxygen is investigated in relation to both nutrient concentrations ###########
######## and whole lake trends during iceon. ##########################################
#######################################################################################
#################### Information about NTL-LTER data used here ########################
#Data was downloaded from the NTL-LTER public download page.
#chemical linology
#https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-chemical-limnology-primary-study-lakes-nutrients-ph-and-carbon-19
#://lter.limnology.wisc.edu/datafile/chemical-limnology-north-temperate-lakes-lter-primary-study-lakes-nutrients-ph-and-carbon
#this is CSV called "chemical_limnology_of_north_temperate_lakes_lter_primary_study_lakes_nutrients_ph_and_carbon_ALL.csv"
#nutrient samples: measured at one station in the deepest part of each lake at the top and bottom of the epilimnion, mid-thermocline, and top, middle, and bottom of the hypolimnion
#physical limnology, including oxygen
#https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-physical-limnology-primary-study-lakes-1981-current
#measured at one station in the deepest part of each lake at 0.25-m to 1-m depth intervals depending on the lake. 
#this is CSV called "physical_limnology_of_the_north_temperate_lakes_primary_study_lakes_ALL.csv"
#snow and ice data: https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-snow-and-ice-depth-1982-current

############################################################################
################ start session and set up dependencies #####################
############################################################################

rm(list = ls())

#base_dir<-"/Users/steve.powers/Desktop/Sandboxes/2016Mar8pm_IcePowers"
start_dir<-as.character(getwd())
base_dir<-start_dir
#base_dir<-substr(start_dir,1,nchar(start_dir)-nchar("/Scripts"))
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
library(MESS)
#library(lme4)

#######################################################################
####################### read in data - all ############################
#################### do some renameing, etc. ##########################
#######################################################################

# read in original synthesis data for Wisconsin lakes (from synthesis dataset) - used for iceon/iceoff bounding dates
data.agg.orig <- read.csv("./Data/wisconsin_under_ice_aggregate_lakes.csv", stringsAsFactors = FALSE)

# elevations (see script lake_depths.R for how lake depths were calculated based on lake elevation in m above sea level)
data.elevs.orig <- read.csv("./Data/final_elevs.csv", stringsAsFactors = FALSE)

data.elevs <- data.elevs.orig %>% rename(year4 = yr_winter)

# physical limnology data for LTER lakes
data.lter.phys.orig <- read.csv("./Data/physical_limnology_of_the_north_temperate_lakes_primary_study_lakes_ALL.csv", 
                                stringsAsFactors = FALSE)

data.lter.phys <- data.lter.phys.orig

data.lter.phys$lakeid <- gsub("AL", "Allequash Lake", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("BM", "Big Musky Lake", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("CB", "Crystal Bog", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("CR", "Crystal Lake", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("SP", "Sparkling Lake", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("TB", "Trout Bog", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("TR", "Trout Lake", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("FI", "Fish Lake", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("ME", "Lake Mendota", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("MO", "Lake Monona", data.lter.phys$lakeid)
data.lter.phys$lakeid <- gsub("WI", "Lake Wingra", data.lter.phys$lakeid)

data.lter.phys <- filter(data.lter.phys, lakeid != "Fish Lake" & lakeid != "Lake Mendota" & lakeid != "Lake Monona" & lakeid != "Lake Wingra")

# merge physical and lake_elev data - lake depth (year and lake specific) is named "maxdepth.t"

data.lter.phys <- merge(data.lter.phys, data.elevs, by = c("lakeid", "year4"))

# nutrient data for LTER lakes
data.lter.nutrients.orig <- read.csv("./Data/chemical_limnology_of_north_temperate_lakes_lter_primary_study_lakes_nutrients_ph_and_carbon_ALL.csv", 
                                     stringsAsFactors = FALSE)

data.lter.nutrients <- data.lter.nutrients.orig

data.lter.nutrients$lakeid <- gsub("AL", "Allequash Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("BM", "Big Musky Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("CB", "Crystal Bog", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("CR", "Crystal Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("SP", "Sparkling Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("TB", "Trout Bog", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("TR", "Trout Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("FI", "Fish Lake", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("ME", "Lake Mendota", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("MO", "Lake Monona", data.lter.nutrients$lakeid)
data.lter.nutrients$lakeid <- gsub("WI", "Lake Wingra", data.lter.nutrients$lakeid)

data.lter.nutrients <- filter(data.lter.nutrients, lakeid != "Fish Lake" & lakeid != "Lake Mendota" & lakeid != "Lake Monona" & lakeid != "Lake Wingra")

#read in here - calculated in script but currently not, because takes a while to calculate...
mix<-read.csv("./Data/mix.csv", stringsAsFactors = FALSE)

##########################################################################
############## LTER data - physical (oxygen) calculations ################
##########################################################################

#only first replicate
data.lter.phys <- data.lter.phys[which(data.lter.phys$rep==1),]

#made date column a date
data.lter.phys$sampledate <- as.Date(data.lter.phys$sampledate, format = "%m/%d/%Y")

#remove columns with flags - not doing any sort of filtering by flag or sloh at this time
#also not doing anything with light at this time
data.lter.phys <- select(data.lter.phys, lakeid, year4, daynum, sampledate, depth, maxdepth.t, rep, sta, wtemp, o2)

#correct incorrect (as per discussion with Noah Lottig?) O2 measurement for Sparkling Lake
#water temp and O2 had been switched in downloaded data
which_correct_Sparkling_18Jan2011<-which(data.lter.phys$lakeid=="Sparkling Lake" & data.lter.phys$sampledate=="2011-01-18")
wtemp_correct<-data.lter.phys$o2[which_correct_Sparkling_18Jan2011]
wo2_correct<-data.lter.phys$wtemp[which_correct_Sparkling_18Jan2011]
data.lter.phys$wtemp[which_correct_Sparkling_18Jan2011]<-wtemp_correct
data.lter.phys$o2[which_correct_Sparkling_18Jan2011]<-wo2_correct


###################################################################################################
############################## LEGACY CODE - IGNORE FOR NOW #######################################
###################################################################################################

#summarize monthly water temp and o2
#wtempO2.monthly <- subset(data.lter.phys,data.lter.phys$depth>-1) %>%
#  group_by(lakeid,sta,year4,month=as.character(format(sampledate,"%b"))) %>%
#  dplyr::summarize(temp_mean=mean(wtemp, na.rm=TRUE),o2_mean=mean(o2sat, na.rm=TRUE))

do<-0
if(do==1){
sumO2.df <- subset(data.lter.phys,data.lter.phys$depth>-1 & data.lter.phys$o2>0)[order(data.lter.phys$lakeid,
                                                                                       data.lter.phys$sampledate,data.lter.phys$sta,data.lter.phys$depth),]
sumO2.df <- sumO2.df %>%
  group_by(lakeid,sta,year4,sampledate) %>%
  dplyr::mutate(int0=depth-lag(depth),int=ifelse(is.na(int0)==TRUE,1,int0)) %>% as.data.frame()
sumO2.df <- sumO2.df %>%
  group_by(lakeid,sta,year4,sampledate) %>%
  dplyr::summarize(O2_sum=sum(o2*int)/max(depth)) %>% as.data.frame()
sumO2.monthly <- sumO2.df %>%
  group_by(lakeid,sta,year4,month=as.character(format(sampledate,"%b"))) %>%
  dplyr::summarize(O2_sum=mean(O2_sum))
}

######################################################################################################

#### CHANGED TO BE MAXDEPTH.T

#new O2 aggregation
sumO2.df <- data.lter.phys

O2ct <- data.lter.phys %>%
  group_by(lakeid,sta,sampledate) %>%
  dplyr::summarize(n=sum(o2>0,na.rm=TRUE)) %>% as.data.frame()

O2ct<-O2ct[which(O2ct$n>=2),]

sumO2.df <- merge(sumO2.df,O2ct,by=c("lakeid","sta","sampledate"))

#sumO2.df <- sumO2.df%>%
  #group_by(lakeid,sta,year4,sampledate) %>%
  #dplyr::mutate(int0=depth-lag(depth),int=ifelse(is.na(int0)==TRUE,1,int0)) %>% as.data.frame()

#merge lake elevations with sumO2.df

#sumO2.df <- rename(sumO2.df, year = year4)
#data.elevs.merge <- rename(data.elevs, year = yr_winter)

#sumO2.df.merge <- merge(sumO2.df, data.elevs.merge, by = c("lakeid", "year"))

sumO2.df.summary <- sumO2.df %>%
  group_by(lakeid,sta,year4,sampledate) %>% arrange(lakeid,sta,year4,depth) %>%
  #calculating auc (from MESS package) - which is area under curve for oxygen vs depth - mg*m/L
  #area under curve here is area below deepest sample, to bottom of lake 
  #(assuming o2 is same as deepest sample - best we can do)
  dplyr::summarize(O2_auc=auc(type="spline",x=c(depth, last(maxdepth.t)),y=c(o2,last(o2))),
                   #back to mg/L units - but for whole water column
                   O2_sum=mean(O2_auc/maxdepth.t, na.rm = TRUE)) %>% as.data.frame()

sumO2.monthly <- sumO2.df.summary %>%
  group_by(lakeid,sta,year4,month=as.character(format(sampledate,"%b"))) %>%
  dplyr::summarize(O2_sum=mean(O2_sum))


############################## LEGACY CODE - IGNORE FOR NOW #################################
#wtemp.monthly<-wtempO2.monthly 
#o2.monthly<-wtempO2.monthly 
#wtemp.monthly$month<-paste("wtemp_",wtemp.monthly$month,sep="")
#o2.monthly$month<-paste("o2_",o2.monthly$month,sep="")
#sumO2.monthly$month<-paste("sumO2_",sumO2.monthly$month,sep="")

#wtemp.monthly <- spread(wtemp.monthly %>% select(-o2_mean),key=month,value=temp_mean) %>% as.data.frame()
#o2.monthly <- spread(o2.monthly %>% select(-temp_mean),key=month,value=o2_mean) %>% as.data.frame()
#sumO2.monthly <- spread(sumO2.monthly,key=month,value=O2_sum) %>% as.data.frame()

#wtemp.o2monthly<-merge(wtemp.monthly,o2.monthly,by=c("lakeid","year4","sta"))
#wtemp.o2monthly<-merge(wtemp.o2monthly,sumO2.monthly,by=c("lakeid","year4","sta"))

#############################################################################################


#### still needed, but already output (and takes a while, so set as do <- 0 so don't need to rerun every time) ####

data.lter.phys$lakestaday<-paste(data.lter.phys$lakeid,data.lter.phys$sta,data.lter.phys$sampledate)
data.lter.nutrients$lakestaday<-paste(data.lter.nutrients$lakeid,data.lter.nutrients$sta,
                                      as.Date(data.lter.nutrients$sampledate, format = "%m/%d/%Y"))
lakestadays<-unique(data.lter.phys$lakestaday)

do<-0
if(do==1){
lakestaday.df<-c()
#layerbound.df<-c()
#grad_range.df<-c()
#UMLbottom<-c()
#deltad<-c()
upper.bound<-c()
lower.bound<-c()
for(i in 1:length(lakestadays)){
  print(i)
  datai<-subset(data.lter.phys,data.lter.phys$lakestaday==lakestadays[i])
  datai<-datai[order(datai$depth),]
  data2i<-subset(data.lter.nutrients,data.lter.nutrients$lakestaday==lakestadays[i] & data.lter.nutrients$rep==1)
  data2i<-data2i[order(data2i$depth),]
  depthCi<-na.omit(data.frame(data2i$depth,data2i$no3no2))
  depthsi<-depthCi[,1]
  if(length(depthsi)==2){depthsi<-c(depthsi[1],mean(c(depthsi[1],depthsi[2])),depthsi[2])}
  if(length(depthsi) %in% c(1,0)){next()}
    #  gradi.df<-na.omit(data.frame(datai$depth,datai$o2))
  gradi.df<-na.omit(data.frame(datai$depth,datai$wtemp))
#  gradi.df<-na.omit(data.frame(datai$depth,datai$o2))
  if(length(gradi.df[,1])==0){next()}
  deltadi<-abs(c(NA,(gradi.df[-1,2]-gradi.df[-length(gradi.df[,1]),2])/
    (gradi.df[-1,1]-gradi.df[-length(gradi.df[,1]),1])))
  gradi.df<-data.frame(gradi.df,deltadi)
  gradi.upper.df<-gradi.df[which(gradi.df[,1]<=depthsi[2]),] 
  gradi.lower.df<-gradi.df[which(gradi.df[,1]>depthsi[2]),] 
#  max.upper.i<-max(gradi.upper.df$deltadi,na.rm=TRUE)
#  max.lower.i<-max(gradi.lower.df$deltadi,na.rm=TRUE)
  max.upper.i<-sort(na.omit(gradi.upper.df$deltadi),decreasing=TRUE)[1:2]
  max.lower.i<-sort(na.omit(gradi.lower.df$deltadi),decreasing=TRUE)[1:2]
  which.upper.i<-which(gradi.upper.df[,3] %in% max.upper.i)
  which.lower.i<-which(gradi.lower.df[,3] %in% max.lower.i)
  upper.bound.i<-mean(gradi.upper.df[c(which.upper.i,which.upper.i-1),1])
  lower.bound.i<-mean(gradi.lower.df[c(which.lower.i,which.lower.i-1),1])
  lakestaday.df<-c(lakestaday.df,lakestadays[i])
#  layerbound.df<-c(layerbound.df,layerboundi)
  upper.bound<-c(upper.bound,upper.bound.i)
  lower.bound<-c(lower.bound,lower.bound.i)
#  grad_range.df<-c(grad_range.df,grad.rangei)
#  deltad<-c(deltad,mini[1])
}
#mix.df<-data.frame(lakestaday=lakestaday.df,layerbound=layerbound.df,grad_range=grad_range.df,UMLbottom,deltad=deltad)#,o2_sum)
mix.df<-data.frame(lakestaday=lakestaday.df,upper.bound,lower.bound)#,o2_sum)
write.csv(mix.df,file="mix.csv")
}

#mix<-read.csv("./Data/mix.csv")
mix<-mix[,-1]

######################################################################################
##################### LTER data - nutrient calculations ##############################
######################################################################################

data.lter.nutrients<-data.lter.nutrients[which(data.lter.nutrients$rep==1),]
data.lter.nutrients$sampledate <- as.Date(data.lter.nutrients$sampledate, format = "%m/%d/%Y")

#for now, not worried about whether values are flagged
#also ignoring "sloh" columns - these are basically double checks by another lab
data.lter.nutrients$no3no2[which(nchar(data.lter.nutrients$flagno3no2)>1)]<-NA
data.lter.nutrients$nh4[which(nchar(data.lter.nutrients$flagnh4)>1)]<-NA
data.lter.nutrients$no3no2[which(data.lter.nutrients$flagno3no2=="I")]<-NA
data.lter.nutrients$nh4[which(data.lter.nutrients$flagnh4=="I")]<-NA

#remove flag/sloh columns
data.lter.nutrients <- select(data.lter.nutrients, lakeid, year4, daynum, sampledate, depth, 
                              rep, sta, event, ph, phair, alk, dic, tic, doc, toc, no3no2, no2, 
                              nh4, totnf, totnuf, totpf, totpuf)


#remove select instances - these values look suspect (not true outliers, possible data entry mistake)
#emailing Noah 2016-04-04 re: these values
data.lter.nutrients <- data.lter.nutrients[-which(data.lter.nutrients$no3no2==data.lter.nutrients$nh4 & data.lter.nutrients$no3no2>300),]


###############################################################################
############ master dataset for LTER - merged nutrient and phys data ##########
############# (including all previously calculated dataframes) ################
###############################################################################

#### merge data sets ####

data.lter.nutrients <- merge(data.lter.nutrients,data.lter.phys,by=c("lakeid","sta","year4","daynum","sampledate","depth","rep"), all=TRUE)
data.lter.nutrients <- merge(data.lter.nutrients,sumO2.df.summary,by=c("lakeid","year4","sta","sampledate"),all=TRUE)
#data.lter.nutrients <- merge(data.lter.nutrients,wtemp.o2monthly,by=c("lakeid","year4","sta"),all=TRUE)
data.lter.nutrients <- merge(data.lter.nutrients,mix,by=c("lakestaday"),all=TRUE)

#data.lter.nutrients <- merge(data.lter.nutrients,o2.monthly,all=TRUE)

#not interested in bog lakes at the moment
data.lter.nutrients<-subset(data.lter.nutrients,!data.lter.nutrients$lakeid %in% c("Crystal Bog","Trout Bog"))

#rename columns, ensure dates are dates
data.lter.nutrients <- dplyr::rename(data.lter.nutrients, lakename = lakeid, year = year4)
data.lter.nutrients$sampledate <- as.Date(data.lter.nutrients$sampledate, format = "%m/%d/%Y")

#calculates days since water year (?) oct. 1
#(doesn't include leap years)
data.lter.nutrients$daynum_wateryr<-data.lter.nutrients$daynum+round(365*0.25,digits=0)
data.lter.nutrients$daynum_wateryr[which(data.lter.nutrients$daynum_wateryr>365)]<-365-data.lter.nutrients$daynum_wateryr[which(data.lter.nutrients$daynum_wateryr>365)]


#######################################################################
######## under-ice ecology aggregate data for same lake subset ########
############ (used for iceon/iceoff bounding dates) ###################
#######################################################################

data.agg<-data.agg.orig

data.agg$lakename[which(data.agg$lakename=="Big Muskellunge Lake")]<-"Big Musky Lake"

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
################# (finds "iceon" (winter) season for lakes) ####################
################################################################################

#this will be huge and unwieldy to begin with 

data.merge <- merge(data.agg.cut, data.lter.nutrients, by = "lakename")

data.subset <- subset(data.merge, sampledate <= enddate & sampledate >= startdate)

#sample month and year

data.subset <- data.subset %>% mutate(sample.month = as.integer(format(sampledate, "%m")),
                                      sampleyear = as.integer(format(sampledate, "%Y")))
                                      #to97 = ifelse(sampleyear <= 1997, "yes", "no"))

#sample date - time since start of iceon (e.g. month diff between start of iceon and sample month)

data.subset.ice <- data.subset %>% 
                    filter(season == "iceon")

#start month num is either 11, 12, or 1
#end month num is 2-5
#try adding 3...adjusting for 12 months total (so +3-12 = -9 for 11 or 12)

#this was for monthly for plotting...may be legacy (don't think need anymore?)

data.subset.ice <- data.subset.ice %>% 
                #adjust months
                mutate(days.since.iceon.start = sampledate - startdate,
                       days.since.iceon.start = as.integer(days.since.iceon.start)) #,
                       #start.month.num.adj = ifelse(start.month.num == 1, start.month.num + 3, start.month.num - 9),
                       #end.month.num.adj = end.month.num +3,
                       #sample.month.adj = ifelse(sample.month == 11, sample.month - 9, sample.month + 3),
                #find difference
                       #months.since.iceon.start = sample.month.adj - start.month.num.adj)

data.subset.ice$days.since.iceon.start<-as.numeric(data.subset.ice$days.since.iceon.start)

#yep, looks like it works...

#data.subset.ice <- data.subset.ice %>% 
                    #select(-start.month.num.adj, -end.month.num.adj, - sample.month.adj)

data.subset.ice <- dplyr::rename(data.subset.ice, NO3N=no3no2, NH4N=nh4, TDN=totnf, TN=totnuf, TDP=totpf, TP=totpuf)
data.subset.ice <- dplyr::mutate(data.subset.ice, DIN=NO3N+NH4N, DON=TDN-NO3N-NH4N)

data.N.iceon<-data.subset.ice

#########################################################################################################################
########################### CHECKING SPECIFIC INSTANCES - MOVE ALONG ####################################################
#########################################################################################################################

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

#set suspect high values as NA
data.N.iceon$DON[which(data.N.iceon$DON>400)]<-NA
data.N.iceon$TDN[which(data.N.iceon$TDN>1000)]<-NA


#create layer columns (1 = TRUE, 0 = FALSE)
#data.N.iceon$upperlayerTF <- as.numeric(data.N.iceon$depth<data.N.iceon$UMLbottom)
#data.N.iceon$bottomlayerTF <- as.numeric(data.N.iceon$depth>data.N.iceon$UMLbottom)
#data.N.iceon$middlelayerTF <- as.numeric(data.N.iceon$depth==data.N.iceon$UMLbottom)

data.N.iceon$upperlayerTF <- as.numeric(data.N.iceon$depth<=data.N.iceon$upper.bound)
data.N.iceon$bottomlayerTF <- as.numeric(data.N.iceon$depth>data.N.iceon$lower.bound)
data.N.iceon$middlelayerTF <- as.numeric(data.N.iceon$depth>data.N.iceon$upper.bound & data.N.iceon$depth<=data.N.iceon$lower.bound)



#getting rid of station 3 at Trout Lake - why?
data.N.iceon <- data.N.iceon[-which(data.N.iceon$lakename=="Trout Lake" & data.N.iceon$sta==3),]

#getmaxdepth<-data.N.iceon %>% 
  #group_by(lakename,year,sampledate) %>%
  #dplyr::summarize(maxdepth.actual=max(depth))

#data.N.iceon<-merge(data.N.iceon,getmaxdepth,by=c("lakename","year","sampledate"),all=TRUE)

#for rows where NO3N is not missing, count length, by depth (e.g. number of samples per depth)
chems<-data.N.iceon[which(is.na(data.N.iceon$NO3N)==FALSE),]

#this isn't used down the road...but useful for now
chemdepths<-chems %>%
  group_by(lakename,sta,depth) %>%
  dplyr::summarize(ncount=length(NO3N)) %>% 
  arrange(lakename,ncount) %>% 
  as.data.frame()

########## re do shallow/deep for upper/middle/bottom? #############


#find average concentrations for "shallow" depths (e.g. surface depths)
data.NP.shallow<-subset(data.N.iceon,data.N.iceon$depth==0)

data.NP.shallow <- data.NP.shallow %>%
  #group by lake/sampledate/maxdepth
  group_by(lakename, year, sampledate, days.since.iceon.start, 
           daynum_wateryr, O2_sum) %>% #maxdepth.actual, O2_sum) %>% #,UMLbottom) %>%
  #find average N-type and P
  dplyr::summarize(NO3N=mean(NO3N,na.rm=TRUE), 
                   NH4N=mean(NH4N,na.rm=TRUE),
                   DIN=mean(DIN,na.rm=TRUE),
                   DON=mean(DON,na.rm=TRUE),
                   TDN=mean(TDN,na.rm=TRUE),
                   TDP=mean(TDP,na.rm=TRUE),
                   TN=mean(TN,na.rm=TRUE),
                   TP=mean(TP,na.rm=TRUE),
                   o2=mean(o2,na.rm=TRUE),
                   wtemp=mean(wtemp,na.rm=TRUE)) %>%
  #select columns of interest
  select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
#         maxdepth.actual, 
         NO3N, NH4N, DIN, DON, TDN, 
         TDP, TN, TP, o2,O2_sum,wtemp)#,UMLbottom)


data.NP.deep<-subset(data.N.iceon,
                     (data.N.iceon$lakename=="Allequash Lake" & data.N.iceon$depth>=6)|
                       (data.N.iceon$lakename=="Big Musky Lake" & data.N.iceon$depth>=16)|
                       (data.N.iceon$lakename=="Crystal Lake" & data.N.iceon$depth>=15)|
                       (data.N.iceon$lakename=="Sparkling Lake" & data.N.iceon$depth>=15)|
                       (data.N.iceon$lakename=="Trout Lake" & data.N.iceon$depth>=25))

data.NP.deep <- data.NP.deep %>%
  group_by(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
#           maxdepth, 
           O2_sum) %>% #,UMLbottom) %>%
  #find average N-type and P
  dplyr::summarize(NO3N=mean(NO3N,na.rm=TRUE), 
                   NH4N=mean(NH4N,na.rm=TRUE),
                   DIN=mean(DIN,na.rm=TRUE),
                   DON=mean(DON,na.rm=TRUE),
                   TDN=mean(TDN,na.rm=TRUE),
                   TDP=mean(TDP,na.rm=TRUE),
                   TN=mean(TN,na.rm=TRUE),
                   TP=mean(TP,na.rm=TRUE),
                   o2=mean(o2,na.rm=TRUE),
                   wtemp=mean(wtemp,na.rm=TRUE)) %>%
  #select columns of interest
  select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
#         maxdepth, 
         NO3N, NH4N, DIN, DON, TDN, 
         TDP, TN, TP, o2,O2_sum,wtemp)#,UMLbottom)

data.NP.middle<-subset(data.N.iceon,
                     (data.N.iceon$lakename=="Allequash Lake" & data.N.iceon$depth>0 & data.N.iceon$depth<6) |
                       (data.N.iceon$lakename=="Big Musky Lake" & data.N.iceon$depth>0 & data.N.iceon$depth<16)|
                       (data.N.iceon$lakename=="Crystal Lake" & data.N.iceon$depth>0 & data.N.iceon$depth<15)|
                       (data.N.iceon$lakename=="Sparkling Lake" & data.N.iceon$depth>0 & data.N.iceon$depth<15)|
                       (data.N.iceon$lakename=="Trout Lake" & data.N.iceon$depth>0 & data.N.iceon$depth<25))

data.NP.middle <- data.NP.middle %>%
  group_by(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
           #           maxdepth, 
           O2_sum) %>% #,UMLbottom) %>%
  #find average N-type and P
  dplyr::summarize(NO3N=mean(NO3N,na.rm=TRUE), 
                   NH4N=mean(NH4N,na.rm=TRUE),
                   DIN=mean(DIN,na.rm=TRUE),
                   DON=mean(DON,na.rm=TRUE),
                   TDN=mean(TDN,na.rm=TRUE),
                   TDP=mean(TDP,na.rm=TRUE),
                   TN=mean(TN,na.rm=TRUE),
                   TP=mean(TP,na.rm=TRUE),
                   o2=mean(o2,na.rm=TRUE),
                   wtemp=mean(wtemp,na.rm=TRUE)) %>%
  #select columns of interest
  select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
         #         maxdepth, 
         NO3N, NH4N, DIN, DON, TDN, 
         TDP, TN, TP, o2,O2_sum,wtemp)#,UMLbottom)

data.NP.shallow$NP_diss<-(data.NP.shallow$DIN/14)/(data.NP.shallow$TDP/31)
data.NP.shallow$NP_diss[which(data.NP.shallow$TDP==0)]<-(data.NP.shallow$DIN[which(data.NP.shallow$TDP==0)]/14)/(0.5/31)
data.NP.deep$NP_diss<-(data.NP.deep$DIN/14)/(data.NP.deep$TDP/31)
data.NP.deep$NP_diss[which(data.NP.deep$TDP==0)]<-(data.NP.deep$DIN[which(data.NP.deep$TDP==0)]/14)/(0.5/31)
data.NP.middle$NP_diss<-(data.NP.middle$DIN/14)/(data.NP.middle$TDP/31)
data.NP.middle$NP_diss[which(data.NP.middle$TDP==0)]<-(data.NP.middle$DIN[which(data.NP.middle$TDP==0)]/14)/(0.5/31)


#########################################################################

#find N type breakdown and P - hypsometrically-weighted
data.NP.hyps <- data.N.iceon  %>% #for photic depths (no avering or anything)
                    select(lakename, year, sampledate,days.since.iceon.start, daynum_wateryr, 
                           depth, maxdepth.t, 
                           upperlayerTF,bottomlayerTF, middlelayerTF, 
                           upper.bound,lower.bound,
                           NO3N, NH4N, DIN, DON, TDN, TDP, TN, TP, o2,O2_sum,wtemp)#,UMLbottom)

data.NP.hyps<- data.NP.hyps %>% 
                   select(-depth) %>%
                   group_by(lakename, year,sampledate,days.since.iceon.start,maxdepth.t,#UMLbottom,
                            O2_sum) %>% 
                   #summarize - mean N type weighted by depth
                   #
                   dplyr::summarize(
                    ######## NO3N ########
                    NO3N.wt.middle = mean(NO3N[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    NO3N.wt.deep = mean(NO3N[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    NO3N.wt.shallow = mean(NO3N[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    NO3N = NO3N.wt.shallow+NO3N.wt.deep+NO3N.wt.middle,
                    ######## NH4H ########
                    NH4N.wt.middle = mean(NH4N[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    NH4N.wt.deep = mean(NH4N[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    NH4N.wt.shallow = mean(NH4N[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    NH4N = NH4N.wt.shallow+NH4N.wt.deep+NH4N.wt.middle,
                    ######## DIN ########
                    DIN.wt.middle = mean(DIN[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    DIN.wt.deep = mean(DIN[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    DIN.wt.shallow = mean(DIN[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    DIN = DIN.wt.shallow+DIN.wt.deep+DIN.wt.middle,
                    ######## TDN ########
                    TDN.wt.middle = mean(TDN[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    TDN.wt.deep = mean(TDN[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    TDN.wt.shallow = mean(TDN[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    TDN = TDN.wt.shallow+TDN.wt.deep+TDN.wt.middle,
                    ######## TN ########
                    TN.wt.middle = mean(TN[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    TN.wt.deep = mean(TN[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    TN.wt.shallow = mean(TN[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    TN = TN.wt.shallow+TN.wt.deep+TN.wt.middle,
                    ######## DON ########
                    DON.wt.middle = mean(DON[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    DON.wt.deep = mean(DON[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    DON.wt.shallow = mean(DON[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    DON = DON.wt.shallow+DON.wt.deep+DON.wt.middle,
                    ######## TDP ########
                    TDP.wt.middle = mean(TDP[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    TDP.wt.deep = mean(TDP[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    TDP.wt.shallow = mean(TDP[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    TDP = TDP.wt.shallow+TDP.wt.deep+TDP.wt.middle,
                    ######## TP ########
                    TP.wt.middle = mean(TP[which(middlelayerTF==1)],na.rm=TRUE)*(lower.bound[1]-upper.bound[1])/maxdepth.t[1],
                    TP.wt.deep = mean(TP[which(bottomlayerTF==1)],na.rm=TRUE)*(maxdepth.t[1]-lower.bound[1])/maxdepth.t[1],
                    TP.wt.shallow = mean(TP[which(upperlayerTF==1)],na.rm=TRUE)*(upper.bound[1])/maxdepth.t[1],
                    TP = TP.wt.shallow+TP.wt.deep+TP.wt.middle
            ) %>% 
            as.data.frame()

#(if # out divide by depth, remove entire section here...)
#data.NP.hyps$NO3N<-data.NP.hyps$NO3N#/data.NP.hyps$maxdepth
#data.NP.hyps$NH4N<-data.NP.hyps$NH4N#/data.NP.hyps$maxdepth
#data.NP.hyps$DIN<-data.NP.hyps$DIN#/data.NP.hyps$maxdepth
#data.NP.hyps$TDN<-data.NP.hyps$TDN#/data.NP.hyps$maxdepth
#data.NP.hyps$DON<-data.NP.hyps$DON#/data.NP.hyps$maxdepth
#data.NP.hyps$TN<-data.NP.hyps$TN#/data.NP.hyps$maxdepth
#data.NP.hyps$TDP<-data.NP.hyps$TDP#/data.NP.hyps$maxdepth
#data.NP.hyps$TP<-data.NP.hyps$TP#/data.NP.hyps$maxdepth

#stoichiometry calculations
data.NP.hyps$NP_diss<-(data.NP.hyps$DIN/14)/(data.NP.hyps$TDP/31)
data.NP.hyps$NP_diss[which(data.NP.hyps$TDP==0)]<-(data.NP.hyps$DIN[which(data.NP.hyps$TDP==0)]/14)/(0.5/31)

#name "type" (e.g. shallow, deeph, hypsometrically-weighted whole lake)
data.NP.shallow$method<-"shallow"
data.NP.deep$method<-"deep"
data.NP.middle$method<-"middle"
data.NP.hyps$method<-"whole (hyps-wt)"

data.NP.shallow$ptsize<-0.5
data.NP.deep$ptsize<-0.5
data.NP.middle$ptsize<-0.5
data.NP.hyps$ptsize<-.55

############################################################
####################### plotting ###########################
############################################################
dataplot<-data.NP.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDP")))
dataplot$form <- factor(dataplot$form, levels=c("NO3N","NH4N","DIN","TDP"))

#color="#619CFF"
plot.NPtime.hyps <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=(value), group=method, colour=method)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())#+
#  ggtitle("Winter N and P, whole lake")
plot.NPtime.hyps

dataplot<-rbind.fill(data.NP.shallow,data.NP.deep,data.NP.middle)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDP")))
dataplot$form <- factor(dataplot$form, levels=c("NO3N","NH4N","DIN","TDP"))

#color="#619CFF"
plot.NPtime.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value), group=method, colour=method)) +
  geom_point(size=0.6) + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(aes(color=method),se=FALSE,lwd=0.5)+#col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())#+
#  ggtitle("Winter N and P, surface and deep")
plot.NPtime.depth

plot.NPO.depth <-  ggplot(dataplot, aes(x=o2, y=log10(value), colour=method)) +
  geom_point(size=0.5) + ylab("Log10 Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
#  geom_smooth(aes(color=method),se=FALSE)+#col="black")+#aes(color = form)) +
#  geom_smooth(col="dark gray",se=FALSE)+#col="black")+#aes(color = form)) +
  geom_smooth(col="black",lwd=0.6)+#,se=FALSE)+#col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())#+
#  ggtitle("Winter N and P, surface and deep")
plot.NPO.depth

dataplot<-rbind.fill(data.NP.hyps,data.NP.shallow,data.NP.deep)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDP")))
dataplot$o2[which(dataplot$method=="whole (hyps-wt)")]<-dataplot$O2_sum[which(dataplot$method=="whole (hyps-wt)")]
dataplot$form <- factor(dataplot$form, levels=c("NO3N","NH4N","DIN","TDP"))

plot.Otime.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=o2, group=method, colour=method)) +
  geom_point(size=0.75) + ylab("O2 Conc (mg/L)") + xlab("Days since iceon")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(linetype=1,se=FALSE)+#col="black")+#aes(color = form)) +
  facet_wrap(~lakename,ncol=3) +
  theme(strip.text.x=element_text())#+
#  ggtitle("Winter O2")
plot.Otime.depth

plot.DINTDP.time.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(NP_diss), group=method, colour=method)) +
  geom_point(size=0.75) + ylab("Log10 DIN:TDP") + xlab("Days since iceon")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(linetype=1,se=FALSE)+#col="black")+#aes(color = form)) +
  facet_wrap(~lakename,ncol=3) +
  geom_abline(intercept=log10(15),slope=0,linetype=2)+
  #  geom_abline(intercept=log10(7.5),slope=0,linetype=2)+
  theme(strip.text.x=element_text())#+
#  ggtitle("Winter N:P")
plot.DINTDP.time.depth

#dataplot<-rbind.fill(data.NP.hyps,data.NP.shallow,data.NP.deep)
dataplot<-data.NP.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDP")))
dataplot$form <- factor(dataplot$form, levels=c("NO3N","NH4N","DIN","TDP"))

plot.NPOhyps <-  ggplot(dataplot, aes(x=O2_sum, y=(value), group=lakename, colour=lakename)) +
  geom_point(size=0.75) + ylab("Conc (ug/L)") + xlab("O2")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(linetype=1,se=FALSE,col="dark gray")+#col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())#+
#  ggtitle("Winter N, P, O2")
plot.NPOhyps

#dataplot<-data.NP.hyps
#dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
#dataplot<-dataplot %>% gather(depth,value,which(names(dataplot) %in% c("UMLbottom","maxdepth")))
#dataplot$depth[which(dataplot$depth=="UMLbottom")]<-"upper layer"

#plot.layers <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value,group=depth,color=depth)) +
#  geom_point()+
#  scale_colour_manual(values=c("darkgray","#00BA38"))+
#  scale_y_reverse(lim=c(32,0))+ylab("Depth (m)")+xlab("Days since iceon")+xlim(0,150)+
#  theme_bw()+#scale_color_gradient(name = "O2 mg/L")+
#  facet_grid(~lakename,scales="free_y") +
#  theme(strip.text.x=element_text())
#plot.layers

##########################################
setwd(figs_dir)

png(file="NPtime.hyps.png",width=7,height=7,units="in",res=300)
print(plot.NPtime.hyps)
dev.off()

png(file="NPtime.depth.png",width=7,height=7,units="in",res=300)
print(plot.NPtime.depth)
dev.off()

png(file="NPO.depth.png",width=7,height=7,units="in",res=300)
print(plot.NPO.depth)
dev.off()

png(file="Otime.depth.png",width=9,height=5,units="in",res=300)
print(plot.Otime.depth)
dev.off()

png(file="NPOhyps.png",width=9,height=7,units="in",res=300)
print(plot.NPOhyps)
dev.off()

png(file="DINTDP.time.depth.png",width=9,height=5,units="in",res=300)
print(plot.DINTDP.time.depth)
dev.off()

#png(file="layers.png",width=12,height=4,units="in",res=300)
#print(plot.layers)
#dev.off()

setwd(base_dir)


##########################################



