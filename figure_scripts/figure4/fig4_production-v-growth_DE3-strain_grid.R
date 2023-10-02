library(tidyverse)


tags <- tibble(x_coord = 0.1,y_coord = c(3, 1.25, 6.5, 6.5),
               label = c('D', 'E', 'B', 'C'),
               strain = tidy_DE3_Hlim %>% 
                 filter(strain != 'BL21 lacI') %>% 
                 .$strain %>% unique())
PvG_grid <- tidy_DE3_Hlim %>% 
  filter(strain != "BL21 lacI" & time <= 5)%>% 
  mutate(iptg = as.factor(iptg))%>%
  filter(iptg %in% c(1,62,100,250,1000)
  ) %>%
  mutate(iptg = str_replace_all(iptg, "1$", "0")
  )%>%
  mutate(iptg = as.numeric(iptg))%>%arrange(iptg)%>%mutate(iptg = as.factor(iptg)) %>%  
  #-------------------------------------------------------------------
ggplot(aes(growth_rate, production_rate/1000, size = iptg, color = iptg))+
  
  facet_wrap(~strain, scales = "free_y")+
  theme_classic()+
  geom_path()+
  geom_text(data = tags, aes(x = x_coord, y = y_coord, label = label),
            size = unit(3, "mm"), color = 'black')+
  scale_color_manual(values = c("1000" = "black", "250"="grey30",
                                "100"="grey50","62"= "grey70", 
                                "0"= "grey80"),
                     name = "IPTG concentration"
  )+
  scale_size_manual(values = c("1000" = unit(1.3, "mm"), "250"=unit(1, "mm"),
                               "100"=unit(0.7, "mm"),"62"= unit(0.4, "mm"), 
                               "0"= unit(0.1, "mm")
  ),
  name = "IPTG concentration"
  )+
  xlab("")+
  ylab("")+
  theme(plot.margin = margin(0,0,0,1, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(3, "mm"),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        strip.text = element_text(size = unit(8, "mm")),
        strip.background = element_rect(color = 'white'),
        aspect.ratio=1/1)

