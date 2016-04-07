####################################################################################
######## This script finds summary statistics for each lake/variable      ##########
######## combination for the iceon season. Summary statistics include:    ##########
######## number of observations minimum, maximum, and quartiles, as well  ##########
######## as number of NAs. The output data frame includes metadata for    ##########
######## each lake (e.g. station latitude and longitude, as well as lake  ##########
######## area), which was taken from the under-ice synthesis dataset,     ##########
######## available at KNB TBD ------                                      ##########
####################################################################################

#### set wd to main folder ####

setwd("D:/Labou/UnderIceBGC")

#### load libraries ####

library(reshape2)
library(tidyr)
library(dplyr)

#### find summary statistics per lake/variable (all depths) ####

#aggregate lake data from synthesis (for metadata)

data.agg <- read.csv("Data/Original/wisconsin_under_ice_aggregate_lakes.csv", stringsAsFactors = FALSE)

meta.agg <- select(data.agg, lakename, stationlat, stationlong, lakearea, watershedarea) %>% unique()

meta.agg <- filter(meta.agg, lakename %in% c("Allequash Lake", "Big Muskellunge Lake", "Crystal Lake", 
                                             "Sparkling Lake", "Trout Lake"))

meta.agg$lakename[which(meta.agg$lakename=="Big Muskellunge Lake")]<-"Big Musky Lake"


#starting from data.N.iceon for NTL-LTER data (see undericeNP.R for how calculated)

data.N.iceon <- readRDS("Data/Outputs/data_N_iceon.rds")

head(data.N.iceon)

data.N.iceon <- data.N.iceon %>% select(lakename, season, lakestaday, year, sampledate, daynum, daynum_wateryr, depth, maxdepth.t,
                                          #ph, phair, alk, dic, tic, doc, toc, 
                                          NO3N, no2, NH4N, DIN, DON, TDN, TN, TDP, TP,
                                          wtemp, o2)
                                          #O2_auc, O2_sum)

#solves later problem is try to split names by "."
data.N.iceon <- rename(data.N.iceon, maxdepth_t = maxdepth.t)

#find range of years for each lake
lake_yrs <- data.frame(lake = character(), min_yr = numeric(), max_yr = numeric())
for (i in unique(data.N.iceon$lakename)) {
  dat <- filter(data.N.iceon, lakename == i)
  lake <- i
  min_yr <- min(dat$year)
  max_yr <- max(dat$year)
  lake_yrs <- rbind(lake_yrs, data.frame(lake, min_yr, max_yr))
}

#each lake year ranges from 1987 to 2013

#find full unique years for each lake
out_yr <- data.frame(lakename = character(), year = numeric())
for (i in unique(data.N.iceon$lakename)) {
  df <- filter(data.N.iceon, lakename == i) %>% 
        select(lakename, year) %>% 
        unique() %>% 
        arrange(year)
  out_yr <- rbind(out_yr, df)
}

#1990 seems to be a missing year and 1996 - Allequash and Big Musky
#Crystal missing 1989 and 1990, 1996
#Sparkling missing 1997
#trout seems to have all years...

#double check this...

##### across all depths, per lake ####

data.N.melt <- melt(data.N.iceon, id.vars = c("lakename", "season", "year", "lakestaday", "sampledate"))

#find N for each variable (per lake, across all years)
data.N.count <- data.N.melt %>% 
  group_by(lakename, variable) %>% 
  filter(!is.na(value)) %>% 
  summarize(n.obvs = length(value))

#group by lake and use summary() function to get summary stats for each variable
# all.test <- filter(data.N.melt, lakename == "Allequash Lake" & variable == "NO3N" & !is.na(value))
# yep, 213 values (across years, because at multiple depths per sample date)

data.N.all <- select(data.N.melt, lakename, season, year, sampledate, lakestaday, variable, value)

#set category groups for variables

data.N.all$category <- ""

data.N.all$category[which(data.N.all$variable %in% c("daynum", "daynum_wateryr", "depth", "maxdepth_t"))] <- "1-meta"

data.N.all$category[which(data.N.all$variable %in% c("NO3N", "no2", "NH4N", "DIN", "DON", "TDN", "TN", "TDP", "TP"))] <- "2-chem"

data.N.all$category[which(data.N.all$variable %in% c("wtemp", "o2"))] <- "3-phys"

data.N.cat <- select(data.N.all, lakename, season, variable, category) %>% unique()


#use summary() function to get summary stats for each variable (for each lake)
out <- list()
for (i in unique(data.N.all$lakename)) {
    #for only that lake, find summary stats for each variable
    sum.stats <- tapply(filter(data.N.all, lakename == i)$value, filter(data.N.all, lakename == i)$variable, summary)
    #add lakename to item names in list output
    names(sum.stats) <- paste(i, names(sum.stats), sep = ".")
    #add to master list
    out <- c(out, sum.stats)
}

#################################################################################################

#technique from: http://stackoverflow.com/questions/27153979/converting-nested-list-unequal-length-to-data-frame
indx <- sapply(out, length)

out.df <- as.data.frame(do.call(rbind,lapply(out, `length<-`, max(indx))))

colnames(out.df) <- names(out[[which.max(indx)]])

data.N.summary <- out.df

#################################################################################################
#fix row names
sum.names <- row.names(data.N.summary)

data.N.summary <- cbind(sum.names, data.N.summary)

row.names(data.N.summary) <- NULL

#separate first column into lakename and variable
data.N.final <- data.N.summary %>% separate(sum.names, into = c("lakename", "variable"), sep = "\\.")

#merge with count and category labels and metadata
data.summary.stats <- merge(data.N.count, data.N.final, by = c("lakename", "variable"))

data.summary.stats <- merge(data.N.cat, data.summary.stats, by = c("lakename", "variable"))

data.summary.stats <- merge(meta.agg, data.summary.stats, by = "lakename")

data.summary.stats <- data.summary.stats[,c(1:5, 7:8, 6, 9:16)]

data.summary.stats <- arrange(data.summary.stats, lakename, category)

#replace NA's in "NA's" column with zero
data.summary.stats$`NA's` <- ifelse(is.na(data.summary.stats$`NA's`), 0, data.summary.stats$`NA's`)

#some weeeeeeird negative DON values for Sparkling Lake...that's...well, that's not right
#actually, there are quite a few negative DON values, which...can't be right

#but otherwise looks good!
#filter(data.N.melt, lakename == "Allequash Lake" & variable == "DIN")  %>% summary()
#matches output in data.N.final

#write to csv
write.csv(data.summary.stats, "Data/data_var_summary.csv", row.names = FALSE)

###############################################################################################

####### summary table, version 2.0 ######

###############################################################################################

setwd("D:/Labou/UnderIceBGC")

#### load libraries ####

library(reshape2)
library(tidyr)
library(dplyr)

#starting from data.N.iceon for NTL-LTER data (see undericeNP.R for how calculated)

data.N.iceon <- readRDS("Data/data_N_iceon.rds")

head(data.N.iceon)

data.N.iceon <- data.N.iceon %>% select(lakename, season, lakestaday, year, sampledate, daynum, daynum_wateryr, depth, maxdepth.t,
                                        upperlayerTF, middlelayerTF, bottomlayerTF, NO3N, NH4N, DIN, TDP, o2)

#solves later problem is try to split names by "."
data.N.iceon <- rename(data.N.iceon, maxdepth_t = maxdepth.t)

#find how many sample year for each lake
lake_yrs <- data.frame(lakename = character(), n.yrs = numeric())

for (i in unique(data.N.iceon$lakename)) {
  dat <- filter(data.N.iceon, lakename == i)
  lakename <- i
  n.yrs <- length(unique(dat$year))
  lake_yrs <- rbind(lake_yrs, data.frame(lakename, n.yrs))
}

#this has lake and per lake number of unique sample years


##### across all depths, per lake ####

data.N.layers <- select(data.N.iceon, lakename, season, sampledate, depth, maxdepth_t, 
                      upperlayerTF, middlelayerTF, bottomlayerTF, NO3N, NH4N, DIN, TDP, o2)

data.N.layers <- data.N.layers %>% 
  group_by(lakename) %>% 
  mutate(max_d_max = max(maxdepth_t)) %>% 
  select(-maxdepth_t) %>% 
  as.data.frame()


data.N.count <- data.N.layers %>% 
                #want number of observations per variable - e.g. on sample date, what vars were sampled
                group_by(lakename, sampledate) %>% 
                #1 if sampled on that day, 0 if not (e.g. all NAs)
                mutate(no3n_tf = ifelse(length(!is.na(NO3N)) > 0, 1, 0),
                       nh4n_tf = ifelse(length(!is.na(NH4N)) > 0, 1, 0),
                       din_tf = ifelse(length(!is.na(DIN)) > 0, 1, 0),
                       tdp_tf = ifelse(length(!is.na(TDP)) > 0, 1, 0),
                       o2_tf = ifelse(length(!is.na(o2)) > 0, 1, 0)) %>% 
                as.data.frame() %>% 
                ungroup() %>% 
                #want to remove multiple counts per day (e.g. because of depth)
                select(lakename, season, sampledate, max_d_max, no3n_tf, nh4n_tf, din_tf, tdp_tf, o2_tf) %>% 
                unique() %>% 
                #within lake, count "number" of observations per variable (e.g. days sampled...)
                group_by(lakename, max_d_max) %>% 
                summarize(NO3N = sum(no3n_tf),
                          NH4N = sum(nh4n_tf),
                          DIN = sum(din_tf),
                          TDP = sum(tdp_tf),
                          o2 = sum(o2_tf)) %>% 
                as.data.frame()
                
#yeah, I mean...that's just less than 3 samples per year (iceon) for Trout and it has the best temporal coverage, year-wise

data.N.count.all <- merge(lake_yrs, data.N.count, by = "lakename", all = TRUE)

data.N.count.all <- melt(data.N.count.all, id.vars = c("lakename", "n.yrs", "max_d_max")) %>% 
                      arrange(lakename, variable) %>% 
                      rename(n.obvs = value)

######### surface means of variables ##########

data.N.surface <- filter(data.N.layers, upperlayerTF == 1) %>% select(-middlelayerTF, -bottomlayerTF, -upperlayerTF, -depth)

data.N.surface <- melt(data.N.surface, id.vars = c("lakename", "season", "sampledate", "max_d_max")) %>% 
                arrange(lakename, sampledate, variable) 

#use summary() function to get summary stats for each variable (for each lake)
out.surface <- list()
for (i in unique(data.N.surface$lakename)) {
  #for only that lake, find summary stats for each variable
  sum.stats <- tapply(filter(data.N.surface, lakename == i)$value, filter(data.N.surface, lakename == i)$variable, summary)
  #add lakename to item names in list output
  names(sum.stats) <- paste(i, names(sum.stats), sep = ".")
  #add to master list
  out.surface <- c(out.surface, sum.stats)
}

#collapse to data frame

indx.surface <- sapply(out.surface, length)

out.surface.df <- as.data.frame(do.call(rbind,lapply(out.surface, `length<-`, max(indx.surface))))

colnames(out.surface.df) <- names(out.surface[[which.max(indx.surface)]])

surface.summary <- out.surface.df

#rename NA columns

surface.summary <- rename(surface.summary, Missing = `NA's`)

#add "surface" designation to columnname
colnames(surface.summary) <- paste("surface", colnames(surface.summary), sep = "_")

#replace NA's in "NA's" column with zero

surface.summary$surface_Missing <- ifelse(is.na(surface.summary$surface_Missing), 0, surface.summary$surface_Missing)

#fix row names
sum.names <- row.names(surface.summary)

surface.summary <- cbind(sum.names, surface.summary)

row.names(surface.summary) <- NULL

#separate first column into lakename and variable
surface.summary.full <- surface.summary %>% separate(sum.names, into = c("lakename", "variable"), sep = "\\.")

#merge with counts
surface.summary.counts <- merge(data.N.count.all, surface.summary.full, by = c("lakename", "variable"))

surface.summary.counts <- surface.summary.counts[, c(1, 3:4, 2, 5, 6:12)]

surface.summary.counts <- arrange(surface.summary.counts, lakename, variable)

#write to csv

write.csv(surface.summary.counts, "Data/Outputs/surface_summary_table.csv", row.names = FALSE)
