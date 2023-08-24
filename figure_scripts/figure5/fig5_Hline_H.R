library(deSolve)
library(tidyverse)
library(ggpubr)


{
  Ind <- c(0.001, 0.01, 0.05, 0.08, 0.1, 0.15, 0.25, 0.5, 0.8, 1)
  Hlim_list <- list()
  cap <- c(1, 10)*0.2
  for(i in 1:length(cap)){
    n <- cap[i]
    m <- sapply(Ind, func_PAM, h = n) %>% 
      t() %>% 
      as_tibble() %>% 
      unnest(cols = c(growth_rate, H))%>%
      mutate(iptg_mM = Ind, n = n,
             .before = growth_rate)
    Hlim_list[[i]] <- m
  }
  Hlim_all <- bind_rows(Hlim_list) %>% 
    mutate(n = as.factor(n))
  
  tab_H<- Hlim_all %>% 
    ggplot(aes(growth_rate, H))+
    geom_point(aes(color = n) , size = 1, show.legend = F)+
    geom_segment(x = 0.55, y = 0.1, xend = 0.41, yend = 0.25,
                 color = "red3", linewidth = unit(0.3, "mm"),
                 arrow = arrow(length = unit(2, "mm"), type = "closed"))+
    xlab("")+
    ylab("")+
    ylim(c(0,0.5))+
    theme_classic()+
    scale_color_manual(values = c("1" = "grey50", 
                                  "10"="black"))+
    theme(plot.margin = margin(0,0,0,0, "mm"),
          axis.text.x = element_text(size = unit(8, "mm")),
          axis.text.y = element_text(size = unit(8, "mm")))+
    theme(aspect.ratio=1/1)
}
tab_H
