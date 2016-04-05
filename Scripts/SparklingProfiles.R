test<-subset(data.lter.nutrients,data.lter.nutrients$lakename=="Sparkling Lake")
  


test


test2<-data.frame(sampledate=test$sampledate,sta=test$sta,depth=test$depth,wtemp=test$wtemp,o2=test$o2)

sampledate.unique<-unique(test2$sampledate)
for(i in 1:length(sampledate.unique)){
  datai<-subset(test2,test2$sampledate==sampledate[i])
  datai<-datai[order(datai$depth),]
  filei<-paste(sampledate.unique[i],".png",sep="")
  png(file=filei,width=9,height=5,units="in",res=300)
  par(mfrow=c(1,2))
  plot(datai$wtemp,-1*datai$depth,xlab="wtemp",ylab="depth")
  plot(datai$o2,-1*datai$depth,xlab="wtemp",ylab="depth")
  dev.off()
  
}