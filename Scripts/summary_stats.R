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

data.agg <- read.csv("./Data/wisconsin_under_ice_aggregate_lakes.csv", stringsAsFactors = FALSE)

meta.agg <- select(data.agg, lakename, stationlat, stationlong, lakearea, watershedarea) %>% unique()

meta.agg <- filter(meta.agg, lakename %in% c("Allequash Lake", "Big Muskellunge Lake", "Crystal Lake", 
                                             "Sparkling Lake", "Trout Lake"))

meta.agg$lakename[which(meta.agg$lakename=="Big Muskellunge Lake")]<-"Big Musky Lake"


#starting from data.N.iceon for NTL-LTER data (see undericeNP.R for how calculated)

data.N.iceon <- readRDS("./Data/data_N_iceon.rds")

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
write.csv(data.summary.stats, "./Data/data_var_summary.csv", row.names = FALSE)

