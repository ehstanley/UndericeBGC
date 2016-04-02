# code moved out of undericNP.R on 2 Apr 2016

icesynth<-data.agg

#try adding 3...adjusting for 12 months total (so +3-12 = -9 for 11 or 12)

non.na<-subset(icesynth,is.na(icesynth$avetotnitro)==FALSE | is.na(icesynth$avetotphos)==FALSE)
non.na<-subset(non.na,non.na$season=="iceon")

yrcounts<- non.na %>%
  group_by(lakename, stationname,season) %>%
  dplyr::summarize(count=length(year))

yrcounts.use<-yrcounts[yrcounts$count>=20,] %>% as.data.frame()
lakesta.use<-unique(data.frame(yrcounts.use$lakename,yrcounts.use$stationname))

icesynth.use<-non.na[which(paste(non.na$lakename,non.na$stationname) %in% paste(lakesta.use[,1],lakesta.use[,2])),]
#  subset(non.na,non.na$lakename==lakesta.use[,1])
icesynth.use<-icesynth.use %>% select(lakename,stationname,lakeregloc,year,iceduration,season,
                                      watertemp,TN=maxtotnitro, TP=maxtotphos)
icesynth.use$TP[which(icesynth.use$TP==0)]<-0.5
icesynth.use$NP_tot<-(icesynth.use$TN/14)/(icesynth.use$TP/31)
icesynth.use$lakesta<-paste(icesynth.use$lakename,substr(icesynth.use$stationname,1,4))
dataplot<-icesynth.use
#dataplot<-subset(icesynth.use,icesynth.use$lakeregloc!="Wisconsin")
#dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Trout Bog", "Crystal Bog","Buffalo Pound Lake"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TN")))#("NP_tot")))#TN","TP")))#,"TP","NP_tot")))
#c("TN","TP")))#,"NP_tot")))
#dataplot <- subset(dataplot,dataplot$lakeregloc=="Wisconsin")
dataplot<-dataplot[order(dataplot$lakesta,dataplot$form,dataplot$year),]
dataplot$lakestaform<-paste(dataplot$lakesta,dataplot$form)
dataplot<-dataplot[which(is.na(dataplot$value)==FALSE),]

do<-1
if(do==1){
dataploti<-dataplot
lakestaforms<-sort(unique(dataploti$lakestaform))
plotdata1<-lapply(lakestaforms,function(lakestaform){
  datai<-dataploti[dataploti$lakestaform==lakestaform,]
  preds<-predict(fit1<-loess(value~year, data=datai,na.action=na.exclude))
  #preds<-predict(fit1<-lme(value~year, data=datai,na.action=na.exclude, correlation=corAR1()))
  df<-data.frame(lakestaform,year=datai$year,preds)
}
)

df<-do.call('rbind',plotdata1)
df<-unique(df)
dataplot<-merge(dataplot,df,by=c("lakestaform","year"))
}

NPdiss.shallow.time <-  ggplot(dataplot, aes(x=year, y=value,group=season,color=season))+#-preds)) +
  geom_point() +
  theme_bw()+#scale_color_gradient(name = "degrees")+
  #    geom_smooth(aes(color = variable)) +
  facet_wrap(~lakesta,scales="free")+#,ncol=6,scales="free") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - shallow")

NPdiss.shallow.time



############################
############################

icesynth<-data.agg

#try adding 3...adjusting for 12 months total (so +3-12 = -9 for 11 or 12)

non.na<-subset(icesynth,is.na(icesynth$avetotnitro)==FALSE | is.na(icesynth$avetotphos)==FALSE)
non.na<-subset(non.na,non.na$season=="iceon")

yrcounts<- non.na %>%
  group_by(lakename, stationname,season) %>%
  dplyr::summarize(count=length(year))

yrcounts.use<-yrcounts[yrcounts$count>=20,] %>% as.data.frame()
lakesta.use<-unique(data.frame(yrcounts.use$lakename,yrcounts.use$stationname))

icesynth.use<-non.na[which(paste(non.na$lakename,non.na$stationname) %in% paste(lakesta.use[,1],lakesta.use[,2])),]
#  subset(non.na,non.na$lakename==lakesta.use[,1])
icesynth.use<-icesynth.use %>% select(lakename,stationname,lakeregloc,year,iceduration,
                                      watertemp,TN=avetotnitro, TP=avetotphos)
icesynth.use$NP_tot<-(icesynth.use$TN/14)/(icesynth.use$TP/31)
icesynth.use$lakesta<-paste(icesynth.use$lakename,substr(icesynth.use$stationname,1,4))
dataplot<-icesynth.use
#dataplot<-subset(icesynth.use,icesynth.use$lakeregloc!="Wisconsin")
#dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Trout Bog", "Crystal Bog","Buffalo Pound Lake"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TN","TP")))#,"NP_tot")))
dataplot <- subset(dataplot,dataplot$lakeregloc!="Wisconsin")
dataplot<-dataplot[order(dataplot$lakesta,dataplot$form,dataplot$year),]
dataplot$lakestaform<-paste(dataplot$lakesta,dataplot$form)

#lakestas<-sort(unique(dataplot$lakesta))
#forms<-sort(unique(dataplot$forms))
#lakestaforms<-sort(unique(dataplot$lakestaform))
#dataploti<-subset(dataplot,dataplot$form=="TN")
#lakestas<-sort(unique(dataploti$lakesta))
dataploti<-dataplot
lakestaforms<-sort(unique(dataploti$lakestaform))
plotdata1<-lapply(lakestaforms,function(lakestaform){
  datai<-dataploti[dataploti$lakestaform==lakestaform,]
  preds<-predict(fit1<-loess(value~year, data=datai,na.action=na.exclude))
  #preds<-predict(fit1<-lme(value~year, data=datai,na.action=na.exclude, correlation=corAR1()))
  df<-data.frame(lakestaform,year=datai$year,preds)
}
)

df<-do.call('rbind',plotdata1)
df<-unique(df)
dataplot<-merge(dataplot,df,by=c("lakestaform","year"))

NPdiss.shallow.time <-  ggplot(dataplot, aes(x=year, y=value))+#-preds)) +
  geom_point(aes(color = watertemp)) +
  theme_bw()+scale_color_gradient(name = "degrees")+
  #    geom_smooth(aes(color = variable)) +
  facet_grid(form~lakesta,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - shallow")

NPdiss.shallow.time

NPdiss.shallow.icedur <-  ggplot(dataplot, aes(x=iceduration, y=value-preds)) +
  geom_point() +
  theme_bw()+scale_color_gradient(name = "degrees")+
  #    geom_smooth(aes(color = variable)) +
  facet_grid(form~lakename,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - shallow")

NPdiss.shallow.icedur


