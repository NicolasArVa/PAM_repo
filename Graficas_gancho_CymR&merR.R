#install.packages("tidyverse")
library(tidyverse)

plot1 <- tidy_j23_Hlim %>% filter(time<4)%>%
  mutate(cumate=factor(cumate))%>%
  ggplot(aes(growth_rate, production_rate/1000, 
             size = cumate, color = cumate))+
  theme_classic()+
  geom_path()+
  scale_color_manual(values = c("1000" = "black", "500"="grey30",
                                "250"="grey50","125"= "grey70", 
                                "0"= "grey80"),
                     expression(Cumate~(mu*M)))+
  scale_size_manual(values = c("1000" = unit(1.3, "mm"), "500"=unit(1, "mm"),
                               "250"=unit(0.7, "mm"),"125"= unit(0.4, "mm"), 
                               "0"= unit(0.1, "mm")),
                    expression(Cumate~(mu*M)))+
  geom_hline(aes(yintercept=0))+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1 * h^-1)))+
  theme(aspect.ratio = 1/1)

ggsave("production_v_growth_CymR.tiff", plot1, width = 84, height = 80, units = "mm", dpi = 600)

plot2 <- tidy_Hg_Hlim %>% filter(between(time, 0.75,8), Hg>=5)%>%
  mutate(Hg=factor(Hg))%>%
  ggplot(aes(growth_rate, production_rate, 
             shape = Hg, color = Hg, size = Hg))+
  theme_classic()+
  geom_path()+
  geom_hline(aes(yintercept=0))+
  scale_color_manual(values = c("250" = "black", "125"="grey30",
                                "50"="grey50","25"= "grey70", 
                                "10"= "grey80"),
                     expression(Hg~(n*M)))+
  scale_size_manual(values = c("250" = unit(1.3, "mm"), "125"=unit(1, "mm"),
                               "50"=unit(0.7, "mm"),"25"= unit(0.4, "mm"), 
                               "10"= unit(0.1, "mm")),
                    expression(Hg~(n*M)))+
  geom_hline(aes(yintercept=0))+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1 * h^-1)))+
  theme(aspect.ratio = 1/1)

ggsave("production_v_growth_merR.tiff", plot2, width = 84, height = 80, units = "mm", dpi = 600)
