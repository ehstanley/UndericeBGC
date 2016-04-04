plot.Nhyps <-  ggplot(dataplot, aes(x=O2_sum, y=log10(value), group=interaction(lakename,method), colour=lakename)) +
  geom_point(aes(shape=method)) + ylab("Log10 Conc (ug/L)") + xlab("O2")+
  theme_bw()+#scale_color_gradient(name = "UML bottom")+
  geom_smooth()+#col="black")+#aes(color = form)) +
  facet_wrap(~form,ncol=3) +
  theme(strip.text.x=element_text())+
  ggtitle("under ice - hyps weighted")
plot.Nhyps