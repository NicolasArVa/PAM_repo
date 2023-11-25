
s <- c("MG1655", "MG1655 lacI", "BLR", "BLR lacI", "BL21 lacI")
test <-lapply(1:length(s), function(i){
  bl <- tidy_DE3_Hlim %>% 
    filter(strain == s[i], between(time,0.5,10)) %>%
    filter(iptg == min(iptg)) %>% 
    select(phi)
  
  tidy_DE3_Hlim %>% 
    filter(strain == s[i], between(time,0.5,10)) %>% 
    group_by(iptg) %>% 
    mutate(phi = phi - bl) %>%
    reframe(phi = max(phi), strain=s[i])
})

measured_phi<-bind_rows(test)%>%
  filter(iptg %in%c(1,62,100,250,1000)) %>%
  mutate(strain=str_replace(strain,"lacI", "+lacI^OV"),
         iptg=factor(str_replace(iptg, "$", "~mu*M"),
                     c("1~mu*M","62~mu*M","100~mu*M","250~mu*M","1000~mu*M")))

slope_grid2 <- tidy_DE3_Hlim %>%
  filter(iptg %in% c(1,62,100,250,1000), time > 2, time <6) %>%
  mutate(strain=str_replace(strain,"lacI", "+lacI^OV"),
         iptg=factor(str_replace(iptg, "$", "~mu*M"),
                     c("1~mu*M","62~mu*M","100~mu*M","250~mu*M","1000~mu*M")))%>%
  ggplot(aes(growth_rate, production_rate/1000))+
  theme_classic()+
  geom_smooth(method = "lm", linewidth=0.5,linetype="dotted", color = "black")+
  geom_point(shape=1,show.legend = F)+
  geom_abline(data = measured_phi, aes(slope= phi/1000, intercept=0))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))+
  xlab(expression(Growth~rate~(h^-1)))+
  facet_grid(iptg~strain, labeller=label_parsed)+
  theme(plot.margin = margin(2,2,2,2, "mm"),
        strip.text = element_text(size = unit(9, "mm")),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(aspect.ratio=1/1)
slope_grid2
ggsave("Figure4_experimentalPhi.tiff", slope_grid2, width = 174, height = 174, units = "mm", dpi = 600)
