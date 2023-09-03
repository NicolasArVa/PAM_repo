data <- tidy_DE3_Hlim %>% 
  filter(iptg %in% c(1,62,100,250,1000)) %>% 
  mutate(iptg = as.factor(iptg))

ggplot(data = data, aes(growth_rate, production_rate/1000, color = iptg))+
  theme_classic()+
  geom_path()+
  geom_point(aes(shape = iptg))+
  geom_line(data = fi_predicted, aes(x,y/1000, color = iptg))+
  geom_smooth(data  = data %>% filter(time > 2), 
              aes(growth_rate, production_rate/1000, 
                  color = iptg), method = "lm", se = F)+
  xlim(c(0, NA))+
  ylim(c(0, NA))+
  facet_wrap(~strain, scales = "free_y")
