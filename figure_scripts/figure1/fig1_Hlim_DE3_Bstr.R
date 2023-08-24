library(tidyverse)

Hline_B <- ggplot()+
  geom_point(data = BL21_lacI_Hlim_fit, aes(GR, phi/1000, shape = "BL21 lacI"),
             size = 2)+
  geom_line(data = BL21_lacI_Hline, aes(x,y/1000),
            size = unit(0.3, "mm"), linetype = 1, show.legend = F)+
  geom_point(data = BLR_lacI_Hlim_fit, aes(GR, phi/1000, shape = "BLR lacI"), 
             size = 2, show.legend = F)+
  geom_line(data = BLR_lacI_Hline, aes(x,y/1000),
            size = unit(0.3, "mm"), linetype = 2, show.legend = F)+
  geom_point(data = BLR_Hlim_fit, aes(GR, phi/1000, shape = "BLR"),
             size = 2, show.legend = F)+
  geom_line(data = BLR_Hline, aes(x,y/1000), 
            size = unit(0.3, "mm"), linetype = 2, show.legend = F)+
  scale_shape_manual(values = c("BLR" = 0, 
                                "BLR lacI" = 7, 
                                "BL21 lacI" = 2), 
                     name = "Strain")+
  ylim(c(0,2.8))+
  xlim(c(0.5,1.25))+
  xlab("")+
  ylab(expression(10^3~FU~OD^-1))+
  theme_classic()+
  theme(plot.margin = margin(0,0,0,0, "mm"),
        legend.title = element_text(size = unit(8, "mm"), face = "bold"),
        legend.text = element_text(size = unit(8, "mm")),
        legend.box.margin = margin(0,0,0,0),
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_text(size = unit(8, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 2.2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        aspect.ratio=1/1)
Hline_B
