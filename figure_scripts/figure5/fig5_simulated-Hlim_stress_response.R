library(deSolve)
library(tidyverse)

Ind <- c(0.001, 0.01, 0.05, 0.08, 0.1, 0.15, 0.25, 0.5, 0.8, 1)
{
  cap <- c(1, 10)*0.2
  for(i in 1:length(cap)){
    n <- cap[i]
    m <- sapply(Ind, func_stress_PAM, # <--- Funcion: corre la simulacion del modelo 
                m = n) %>% 
      t() %>% 
      as_tibble() %>% 
      unnest(cols = c(growth_rate, H))%>%
      mutate(iptg_mM = Ind, n = n, .before = growth_rate)
    name <- paste0("Hlim_c", i)
    assign(name, m)
  }
  
  Hlim_all <- bind_rows(Hlim_c1, Hlim_c2) %>% 
    mutate(n = as.factor(n))
#-------------------- ploteo de lo simulado -----------------------------------

plot_stress <-  Hlim_all %>% 
  mutate(sign = ifelse(n == 0.2, "-", "+"))%>%
  ggplot(aes(growth_rate, H, color = sign))+
    geom_point(size = 3, show.legend = T)+
    geom_curve(x = 0.58, y = 0.072, xend = 0.43, yend = 0.045,
                 color = "red3", linewidth = unit(0.3, "mm"),
               arrow = arrow(length = unit(4, "mm"), type = "closed"),
               curvature = 0.1)+
    xlim(c(0.2,0.7))+
    xlab("")+
    ylim(c(0.0,0.1))+
    ylab("Heterologous fraction")+
    theme_classic()+
  scale_color_manual(values = c("-" = "grey", 
                                "+"="black"),
                     name="")+
  theme(plot.margin = margin(0,0,0,0, "mm"),
        legend.title = element_text(size = unit(8, "mm"), face = "bold"),
        legend.text = element_text(size = unit(8, "mm")),
        legend.box.margin = margin(0,0, 0, 0),
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_text(size = unit(8, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 15),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))+
  theme(aspect.ratio=3/2)
}
plot_stress
