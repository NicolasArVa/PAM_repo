install.packages("tidyverse")
library(tidyverse)

tidy_j23_Hlim %>% filter(time<8)%>%
  mutate(cumate=factor(cumate))%>%
  ggplot(aes(growth_rate, production_rate, 
             shape = cumate, color = cumate))+
  geom_path(linewidth = 2)+
  geom_point(size = 5)+
  geom_hline(aes(yintercept=0))+
  ggtitle("Production vs Growth: CymR")


tidy_Hg_Hlim %>% filter(time<8, Hg>=5)%>%
  mutate(Hg=factor(Hg))%>%
  ggplot(aes(growth_rate, production_rate, 
             shape = Hg, color = Hg))+
  geom_path(linewidth = 2)+
  geom_point(size = 5)+
  geom_hline(aes(yintercept=0))+
  ggtitle("Production vs Growth: merR")
