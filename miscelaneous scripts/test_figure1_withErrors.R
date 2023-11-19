library(tidyverse)
test <- tidy_DE3_Hlim %>% select (-`...1`)%>%
  group_by(strain,iptg) %>%
  summarise(gr=max(growth_rate),gr_se = growth_rate_se[which.max(growth_rate)],
            phi = phi[which.max(growth_rate)],phi_se = phi_se[which.max(growth_rate)])

test %>% 
  mutate(phi = phi/1000, phi_se=phi_se/1000)%>%
  filter(strain %in% c("MG1655", "MG1655 lacI")) %>%
  ggplot(aes(gr,phi,color=strain))+
  theme_classic()+
  geom_point()+
  ylim(c(0,NA))+
  xlim(c(0,NA))+
  geom_errorbar(aes(ymin=phi-phi_se,
                    ymax=phi+phi_se))+
  geom_errorbar(aes(xmin=gr-gr_se,
                    xmax=gr+gr_se))+
  geom_smooth(method = "lm", se=F)


test %>% 
  mutate(phi = phi/1000, phi_se=phi_se/1000)%>%
  filter(!strain %in% c("MG1655", "MG1655 lacI")) %>%
  ggplot(aes(gr,phi,color=strain))+
  theme_classic()+
  geom_point()+
  ylim(c(0,NA))+
  xlim(c(0,NA))+
  geom_errorbar(aes(ymin=phi-phi_se,
                    ymax=phi+phi_se))+
  geom_errorbar(aes(xmin=gr-gr_se,
                    xmax=gr+gr_se))+
  geom_smooth(method = "lm", se=F)