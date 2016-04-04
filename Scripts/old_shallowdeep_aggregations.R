
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
