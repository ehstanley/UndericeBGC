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

data.N.summary <- data.N.iceon %>% select(lakename, season, lakestaday, year, sampledate, daynum, daynum_wateryr, depth, maxdepth.t,
                                          ph, phair, alk, dic, tic, doc, toc, NO3N, no2, NH4N, DIN, DON, TDN, TN, TDP, TP,
                                          wtemp, o2, O2_auc, O2_sum)

data.N.melt <- melt(data.N.summary, id.vars = c("lakename", "season", "lakestaday", "year", "sampledate", "daynum", "daynum_wateryr", "maxdepth.t"))

data.N.summary <- tapply(data.N.melt$value, data.N.melt$variable, summary)

data.N.summary <- do.call("rbind", data.N.summary) %>% as.data.frame()

sum.names <- row.names(data.N.summary)

data.N.summary <- cbind(sum.names, data.N.summary)

row.names(data.N.summary) <- NULL

data.N.summary <- rename(data.N.summary, variable = sum.names)

write.csv(data.N.summary, "./Data/data.N.summary.csv", row.names = FALSE)
