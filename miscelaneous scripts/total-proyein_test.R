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

Fig8S <- test %>% mutate(prot_od=prot_conc/od) %>%
  ggplot(aes(iptg,prot_od))+
  geom_boxplot()+
  geom_point()+
  ylim(c(0,NA))+
  xlab("IPTG")+
  ylab(expression(Protein~per~optical~density~(mu*g~mu*l^-1~OD[600]^-1)))+
  theme(plot.margin = margin(2,2,2,2, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(12, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))+
  theme(aspect.ratio=1/1)
ggsave("Figure8S.tiff", Fig8S, width = 84, height = 84, units = "mm", dpi = 600)
