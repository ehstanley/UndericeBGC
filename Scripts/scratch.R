plot.Nhyps <-  ggplot(dataplot, aes(x=O2_sum, y=log10(value), group=interaction(lakename,method), colour=lakename)) +
  geom_point(aes(shape=method)) + ylab("Log10 Conc (ug/L)") + xlab("O2")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth()+#col="black")+#aes(color = form)) +
  facet_wrap(~form,ncol=3) +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps



##########################################

dataplot<-rbind.fill(data.NP.hyps,data.NP.shallow,data.NP.deep)
#dataplot<-data.NP.shallow
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDP")))

plot.NPOdepths <-  ggplot(dataplot, aes(x=O2_sum, y=log10(value), group=lakename, colour=lakename)) +
  geom_point(size=0.5) + ylab("Log10 Conc (ug/L)") + xlab("O2")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(se=FALSE)+#col="black")+#aes(color = form)) +
  facet_grid(form~method) +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPOdepths






############################################################
dataplot<-data.NP.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.Nhyps <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps

dataplot<-rbind(data.NP.shallow,data.NP.deep)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.Nform.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value,group=method,colour=method)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38"))+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())
plot.Nform.depth

dataplot<-plyr::rbind.fill(data.NP.deep,data.NP.shallow,data.NP.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("DIN")))
dataplot$value[which(dataplot$value==0)]<-0.5

plot.DINcombine.depths <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.DINcombine.depths

dataplot<-plyr::rbind.fill(data.NP.deep,data.NP.shallow,data.NP.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TDP")))
dataplot$value[which(dataplot$value==0)]<-0.5

plot.TDPcombine.depths <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.TDPcombine.depths

dataplot<-plyr::rbind.fill(data.NP.hyps,data.NP.deep,data.NP.shallow)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NP_diss")))

plot.NPcombine <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=log10(value),color=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+xlim(0,150)+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~method,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - NP")+
  geom_abline(intercept=log10(15),slope=0,linetype=2)+
  geom_abline(intercept=log10(7.5),slope=0,linetype=2)
plot.NPcombine

dataplot<-data.NP.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(depth,value,which(names(dataplot) %in% c("UMLbottom","maxdepth")))
dataplot$depth[which(dataplot$depth=="UMLbottom")]<-"upper layer"


dataplot<-rbind.fill(data.NP.hyps,data.NP.shallow,data.NP.deep)
#dataplot<-data.NP.shallow
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDP")))

plot.NPOdepths <-  ggplot(dataplot, aes(x=O2_sum, y=log10(value), group=lakename, colour=lakename)) +
  geom_point(size=0.5) + ylab("Log10 Conc (ug/L)") + xlab("O2")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(se=FALSE)+#col="black")+#aes(color = form)) +
  facet_grid(form~method) +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.NPOdepths


dataplot<-data.NP.hyps
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

dataplot<-data.NP.hyps
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

dataplot<-rbind(data.NP.shallow,data.NP.deep)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN","TDP")))
plot.NPO2.depth <-  ggplot(dataplot, aes(x=O2_sum, y=value,group=method,colour=method)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("O2 Conc (mg/L)")+
  scale_colour_manual(values=c("darkgray","#00BA38"))+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth(col="black")+#aes(color = form)) +
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())
plot.NPO2.depth

dataplot<-data.NP.deep
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

dataplot<-data.NP.shallow
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
setwd(figs_dir)

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

setwd(base_dir)
#############################################################


dataplot<-data.NP.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.Nhyps <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value)) +
  geom_point(color="#619CFF") + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps

dataplot<-data.NP.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.Nhyps <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value)) +
  geom_point(color="#619CFF") + ylab("Conc (ug/L)") + xlab("Days since iceon")+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~form,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps

dataplot<-data.NP.deep
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Trout Lake","Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))
plot.Nform.depth <-  ggplot(dataplot, aes(x=days.since.iceon.start, y=value,colour=sumO2_Nov)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("Days since iceon")+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())
plot.Nform.depth

plot.Nform.depth <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value,colour=sumO2_Nov)) +
  geom_point() + ylab("Conc (ug/L)") + xlab("Days since iceon")+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~form,scales="free") +
  theme(strip.text.x=element_text())
plot.Nform.depth

dataplot<-plyr::rbind.fill(data.NP.deep,data.NP.shallow,data.NP.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("DIN")))
dataplot$value[which(dataplot$value==0)]<-0.5

plot.DINcombine.depths <-  ggplot(dataplot, aes(x=sumO2_Nov, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.DINcombine.depths

dataplot<-plyr::rbind.fill(data.NP.deep,data.NP.shallow,data.NP.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TDP")))
dataplot$value[which(dataplot$value==0)]<-0.5

plot.TDPcombine.depths <-  ggplot(dataplot, aes(x=sumO2_Nov, y=log10(value),colour=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~method) +
  theme(strip.text.x=element_text())
plot.TDPcombine.depths

dataplot<-plyr::rbind.fill(data.NP.hyps,data.NP.deep,data.NP.shallow)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NP_diss")))

plot.NPcombine <-  ggplot(dataplot, aes(x=sumO2_Nov, y=log10(value),color=method)) +
  geom_point() + ylab("Log10 Conc (ug/L)") + xlab("Days since iceon")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+
  geom_smooth(col="black")+
  facet_grid(lakename~method,scales="free_y") +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - NP")+
  geom_abline(intercept=log10(15),slope=0,linetype=2)+
  geom_abline(intercept=log10(7.5),slope=0,linetype=2)
plot.NPcombine

##########################
##########################

dataplot<-plyr::rbind.fill(data.NP.deep,data.NP.shallow,data.NP.hyps)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("TDN")))

dataplot<-data.NP.hyps
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.o2 <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value,group=method,color=method)) +
  geom_point(colour="darkgray") + ylab("Conc (ug/L)") + xlab("sum O2 Nov")+
  scale_colour_manual(values=c("darkgray","#00BA38","#619CFF"))+
  theme_bw()+
  geom_smooth(col="black")+
  facet_wrap(lakename~method,scales="free",ncol=4)+
  theme(strip.text.x=element_text())
plot.o2

dataplot<-rbind(data.NP.shallow,data.NP.deep)
dataplot<-subset(dataplot,!dataplot$lakename %in% c("Lake Monona", "Fish Lake","Lake Mendota","Lake Wingra"))
dataplot<-dataplot %>% gather(form,value,which(names(dataplot) %in% c("NO3N","NH4N","DIN","TDN")))

plot.o2 <-  ggplot(dataplot, aes(x=sumO2_Nov, y=value)) +
  geom_point(colour="darkgray") + ylab("Conc (ug/L)") + xlab("sum O2 Nov")+#xlim(0,150)+
  theme_bw()+
  geom_smooth(col="black")+
  facet_wrap(lakename~form,scales="free",ncol=4)+
  theme(strip.text.x=element_text())
plot.o2


###########################
############################

