#pulling together summary stats for under ice nutrients project
#SL starteed 4/416

#max depth
#surface area
#data availability stats: n, by variable, by depth (surface, deep, whole lake)
# --> iceon only, for Nh4N, NO3N, TDP, 02

#use summary stats from synthesis scripts (e.g. n, median, quartiles)

setwd("D:/Labou/UnderIceBGC")

#starting from data.N.iceon...

head(data.N.iceon)

library(reshape2)
library(tidyr)

data.N.summary <- data.N.iceon %>% select(lakename, season, lakestaday, year, sampledate, daynum, daynum_wateryr, depth, maxdepth.t,
                                          #ph, phair, alk, dic, tic, doc, toc, 
                                          NO3N, no2, NH4N, DIN, DON, TDN, TN, TDP, TP,
                                          wtemp, o2)
                                          #O2_auc, O2_sum)

##### across all depths, per lake ####

data.N.melt <- melt(data.N.summary, id.vars = c("lakename", "season", "year", "lakestaday", "daynum", "daynum_wateryr", "sampledate", "depth", "maxdepth.t"))


#find N for each variable (per lake, across all years)
data.N.count <- data.N.melt %>% 
  group_by(lakename, variable) %>% 
  filter(!is.na(value)) %>% 
  summarize(n.obvs = length(value))

#group by lake and use summary() function to get summary stats for each variable
# all.test <- filter(data.N.melt, lakename == "Allequash Lake" & variable == "NO3N" & !is.na(value))
# yep, 213 values (across years, because at multiple depths per sample date)

#merge with main dataset

#data.N.all <- merge(data.N.melt, data.N.count, by = c("lakename", "variable"))

data.N.all <- select(data.N.all, lakename, season, year, sampledate, lakestaday, daynum, daynum_wateryr, maxdepth.t, variable, value)

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

data.N.summary <- do.call("rbind", out) %>% as.data.frame()

sum.names <- row.names(data.N.summary)

data.N.summary <- cbind(sum.names, data.N.summary)

row.names(data.N.summary) <- NULL

data.N.final <- data.N.summary %>% separate(sum.names, into = c("lakename", "variable"), sep = "\\.")

data.N.final <- merge(data.N.count, data.N.final, by = c("lakename", "variable"))

#some weeeeeeird negative DON values for Sparkling Lake...that's...well, that's not right
#actually, there are quite a few negative DON values, which...can't be right

#but otherwise looks good!
#filter(data.N.melt, lakename == "Allequash Lake" & variable == "DIN")  %>% summary()
#matches output in data.N.final

write.csv(data.N.final, "./Data/data_var_summary.csv", row.names = FALSE)




