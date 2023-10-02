library(readxl)
library(tidyverse)
tidy_Hg_Hlim %>% mutate(Hg=as.factor(Hg)) %>%
  ggplot(aes(time, production_rate, group=Hg, color = Hg))+
  geom_line()

test <- read_xlsx("BRADFORD_GELES_ETC.xlsx")
names(test) <- c("strain", "iptg", "time", "od", "flu", "prot_conc", "G")

test %>% mutate(prot_od=prot_conc/od) %>%
  ggplot(aes(strain,prot_od))+
  geom_boxplot()+
  geom_point()+
  ylim(c(0,NA))

test %>% mutate(prot_od=prot_conc/od) %>%
  ggplot(aes(iptg,prot_od))+
  geom_point()+
  facet_grid(time~strain)+
  ylim(c(0,NA))

test %>% mutate(prot_od=prot_conc/od) %>%
  ggplot(aes(iptg,prot_od))+
  geom_point()+
  ylim(c(0,NA))
