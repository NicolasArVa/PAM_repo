n <- 1000

error_table <- tidy_DE3_Hlim %>%
  mutate(iptg=factor(iptg))%>%
  filter(iptg %in% c(1,n), time <=5)%>%
  group_by(strain, iptg)%>%
  summarize(time = time[which.max(growth_rate)]
            ,growth_rate=max(growth_rate),
            se=growth_rate_se[which.max(growth_rate)])

labels<-data.frame(x = 0,y = 1.5, label = c("D", "E", "B", "C","A"),
                   strain=factor(c("MG1655","MG1655~lacI^OV","BLR","BLR~lacI^OV","BL21~lacI^OV")))

growth_burden_GFP <- tidy_DE3_Hlim %>%
  mutate(iptg=str_replace(iptg,"1$", "-"), 
         iptg=str_replace(iptg,"1000", "+"),
         strain=factor(str_replace_all(strain, " lacI", "~lacI^OV"), c("MG1655","MG1655~lacI^OV","BLR","BLR~lacI^OV","BL21~lacI^OV")))%>%
  filter(iptg %in% c("-","+"), time <= 5)%>%
  rename(IPTG=iptg)%>%
  ggplot(aes(time,growth_rate))+
  theme_classic()+
  geom_point(aes(color=IPTG))+
  geom_line(aes(color=IPTG))+
  geom_text(data=labels,aes(x = x,y = y, label = label), color = "black")+
  #geom_errorbar(data = error_table, 
                #aes(ymin = growth_rate - qnorm(0.975)*se,
                    #ymax = growth_rate + qnorm(0.975)*se))+
  facet_wrap(~strain, labeller = label_parsed, ncol = 2, as.table = F)+
  ylab(expression(Growth~rate~(h^-1)))+
  xlab("Time (h)")+
  theme(strip.background = element_rect(color = 'white'))

ggsave("Figure2S.tiff", growth_burden_GFP, width = 174, height = 180, units = "mm", dpi = 600)



