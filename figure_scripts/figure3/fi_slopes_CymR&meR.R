library(tidyverse)
labels = tibble(label=LETTERS[1:5], x=0, y=1.5, cumate = factor(c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M"),levels=c("0~mu*M","125~mu*M","250~mu*M","500~mu*M","1000~mu*M")))

predicted_fi <- tab_CymR %>% filter(cumate %in% c(0,125,250,500,1000)) %>%
  select(cumate, fi, error)
GP <- expand.grid(fi = predicted_fi$fi, gr = seq(0,0.5, 0.1)) %>%
  mutate(pr = fi*gr)
GP_lines <- predicted_fi %>% left_join(GP, by = "fi") %>%
  mutate(lower = pr - qnorm(0.975)*error*gr,
         upper = pr + qnorm(0.975)*error*gr) %>% select(-error)%>%
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
  geom_line(data=GP_lines, aes(gr, pr/1000), show.legend=F)+
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

################################# merR ########################################################

labels = tibble(label=LETTERS[1:6], x=0, y=20, Hg = factor(c("0~n*M","50~n*M","125~n*M","250~n*M","500~n*M","1000~n*M"),
                                                               levels=c("0~n*M","50~n*M","125~n*M","250~n*M","500~n*M","1000~n*M")))


predicted_fi <- tab_merR %>% 
  filter(Hg %in% c(0, 50, 125, 250, 500, 1000)) %>% 
  select(Hg, fi, error)
GP <- expand.grid(fi = predicted_fi$fi, gr = seq(0,3, 0.1)) %>%
  mutate(pr = fi*gr)
GP_lines <- predicted_fi %>% left_join(GP, by = "fi") %>%
  mutate(lower = pr - qnorm(0.975)*error*gr,
         upper = pr + qnorm(0.975)*error*gr) %>% select(-error)

fig6s<-tidy_Hg_Hlim %>% 
  filter(between(time, 2.5, 8), !Hg %in% c(1,2,5,10,25,2000)) %>%
  mutate(Hg = factor(str_replace_all(Hg, "$", "~n*M"),levels=c("0~n*M","50~n*M","125~n*M","250~n*M","500~n*M","1000~n*M")))%>%
 ggplot()+
  geom_point(aes(growth_rate, production_rate/1000), pch=1)+
  geom_smooth(method = "lm",aes(growth_rate, production_rate/1000), color="black", linetype="dotted")+
  theme_classic()+
  geom_hline(yintercept = 0)+
  facet_wrap(~Hg, ncol=2, labeller=label_parsed, scales = "free")+
  geom_text(data=labels, aes(x,y,label=label))+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))+  
  theme(plot.margin = margin(0,0,0,1, "mm"),
        axis.text.x = element_text(size = unit(9, "mm")),
        axis.text.y = element_text(size = unit(9, "mm")),
        strip.text = element_text(size = unit(9, "mm")),
        strip.background = element_rect(color = 'white'),
        aspect.ratio=1/1)
fig6s
ggsave("Figure6S.tiff", fig6s, width = 84, height = 140, units = "mm", dpi = 600)

Hg_slopes <- tidy_Hg_Hlim %>% 
  filter(between(time, 2, 8)) %>%
  group_by(Hg)%>%
  summarize(slope=lm(production_rate~growth_rate)$coefficients["growth_rate"])
Hg_slopes

Hg_slopes %>%
  ggplot()+
  geom_point(aes(Hg,slope/1000))+
  geom_smooth(aes(Hg,slope/1000))

