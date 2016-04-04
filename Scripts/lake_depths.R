#This script calculates specific lake depths based on NTL-LTER lake elevation (m above sea level) data.
# For each year, the last measured lake elevation of the previous year is used.
# For example, all winter samples for Allequash 1987 would come from the last sampled lake elevation of 1986 (e.g. Nov. 1986)
# this takes into consider "max lake depth" in years where lake depth changes due to drought, etc.
# this accounts for some years in ex: Sparkling where the lake elevation shows a change of something like 2 meters over the long term trend

data.agg.orig <- read.csv("./Data/wisconsin_under_ice_aggregate_lakes.csv", stringsAsFactors = FALSE)

data.agg<-data.agg.orig %>% rename(lakeid = lakename)

data.agg$lakeid <- gsub("Big Muskellunge Lake", "Big Musky Lake", data.agg$lakeid)

from_agg <- data.agg %>% group_by(lakeid) %>% summarize(max_depth = max(lakemaxdepth)) %>% as.data.frame()


lake <- read.csv("./Data/north_temperate_lakes_lter__lake_levels_ALL.csv", stringsAsFactors = FALSE)

#assuming maximum ever lake elevation is corresponding to what we get as lake max depth in synthesis agg data for these lakes
maximums <- lake  %>% group_by(lakeid)  %>% summarize(max_level_elev = max(llevel_elevation)) %>% as.data.frame()

maximums$lakeid <- gsub("AL", "Allequash Lake", maximums$lakeid)
maximums$lakeid <- gsub("BM", "Big Musky Lake", maximums$lakeid)
maximums$lakeid <- gsub("CB", "Crystal Bog", maximums$lakeid)
maximums$lakeid <- gsub("CR", "Crystal Lake", maximums$lakeid)
maximums$lakeid <- gsub("SP", "Sparkling Lake", maximums$lakeid)
maximums$lakeid <- gsub("TB", "Trout Bog", maximums$lakeid)
maximums$lakeid <- gsub("TR", "Trout Lake", maximums$lakeid)


all <- merge(maximums, from_agg, by = "lakeid")

all <- mutate(all, offset = max_level_elev - max_depth)

all <- select(all, lakeid, offset)


#lake elevation for year - offset - this is how we use lake elevation to get to actual lake max depth

#use lake elevation from last previous sampled....

#filter by daynumber - grab the latest elevation sample per year

latest <- lake %>% group_by(lakeid, year4) %>% filter(daynum == max(daynum)) %>% select(-sta) %>% as.data.frame()

latest$lakeid <- gsub("AL", "Allequash Lake", latest$lakeid)
latest$lakeid <- gsub("BM", "Big Musky Lake", latest$lakeid)
latest$lakeid <- gsub("CB", "Crystal Bog", latest$lakeid)
latest$lakeid <- gsub("CR", "Crystal Lake", latest$lakeid)
latest$lakeid <- gsub("SP", "Sparkling Lake", latest$lakeid)
latest$lakeid <- gsub("TB", "Trout Bog", latest$lakeid)
latest$lakeid <- gsub("TR", "Trout Lake", latest$lakeid)

latest <- mutate(latest, yr_winter = year4 + 1)

#merge offset data with max fall elevation (e.g. last sampled lake elevation before iceon season)

full <- merge(latest, all, by = "lakeid")

full <- select(full, -year4, -daynum, -sampledate)

full <- rename(full, fall_elev = llevel_elevation)

#max depth (winter t) = lake elevation (fall t-1) - offset

full <- full %>% select(lakeid, yr_winter, fall_elev, offset)

full <- full %>% mutate(maxdepth.wintert = fall_elev - offset)

final_elevs <- full %>% select(lakeid, yr_winter, maxdepth.wintert) %>% rename(maxdepth.t = maxdepth.wintert)

write.csv(final_elevs, "./Data/final_elevs.csv", row.names = FALSE)

