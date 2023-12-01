library(tidyverse)
labels = tibble(label=LETTERS[1:5], x=0, y=1.5, cumate = factor(c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M"),levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))

blc <- tidy_j23_Hlim %>% 
  filter(time > 0.5, cumate == min(cumate)) %>% 
  select(time, phi)%>%
  rename(bl = phi)

CymR_tab <- tidy_j23_Hlim%>% 
  filter(time > 0.5) %>% 
  left_join(blc, by="time")%>%
  mutate(phi = phi - bl) %>%
  group_by(cumate) %>%
  summarize(phi = max(phi))%>%
  mutate(cumate=factor(str_replace_all(cumate, "$","~mu*M"), 
                       levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))

fig5s<-tidy_j23_Hlim %>% 
  filter(time < 6) %>%
  mutate(cumate=factor(str_replace_all(cumate, "$","~mu*M"), 
                       levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))%>%
  ggplot()+
  geom_smooth(method = "lm",
              aes(growth_rate, production_rate/1000), color="black", linetype="dotted")+
  geom_point(aes(growth_rate, production_rate/1000), pch=1)+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_abline(data=CymR_tab, aes(slope=phi/1000, intercept=0))+
  facet_wrap(~cumate, ncol = 2, labeller=label_parsed)+
  geom_text(data=labels, aes(x,y,label=label))+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))+
  theme(plot.margin = margin(0,0,0,1, "mm"),
        axis.text.x = element_text(size = unit(9, "mm")),
        axis.text.y = element_text(size = unit(9, "mm")),
        strip.text = element_text(size = unit(9, "mm")),
        strip.background = element_rect(color = 'white'),
        aspect.ratio=1/1)
fig5s
ggsave("Figure5S_experimentalPhi.tiff", fig5s, width = 84, height = 140, units = "mm", dpi = 600)
