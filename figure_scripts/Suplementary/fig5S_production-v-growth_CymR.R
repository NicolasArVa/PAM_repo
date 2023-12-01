library(tidyverse)
labels = tibble(label=LETTERS[1:5], x=0, y=1.5, cumate = factor(c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M"),levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))

predicted_fi <- tab_CymR %>% filter(cumate %in% c(0,125,250,500,1000)) %>%
  select(cumate, fi, error)%>%
  mutate(cumate=factor(str_replace_all(cumate, "$","~mu*M"), 
                       levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))

fig5s<-tidy_j23_Hlim %>% 
  filter(time < 3) %>%
  mutate(cumate=factor(str_replace_all(cumate, "$","~mu*M"), 
                       levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))%>%
ggplot()+
  geom_smooth(method = "lm",
              aes(growth_rate, production_rate/1000), color="black", linetype="dotted")+
  geom_point(aes(growth_rate, production_rate/1000), pch=1)+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_abline(data=predicted_fi, aes(slope=fi/1000, intercept=0))+
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
ggsave("Figure5S.tiff", fig5s, width = 84, height = 140, units = "mm", dpi = 600)
