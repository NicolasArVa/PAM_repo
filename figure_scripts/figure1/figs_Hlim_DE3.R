#---------------------- Plot H-line -----------------------------------------------------------------------------------------------------
library(ggdark)

#cols <- rainbow(10)
Hline <- ggplot()+
  geom_point(data = BL21_lacI_Hlim_fit, aes(GR, phi/1000, shape = "BL21 lacI"),
             size = 2)+
  geom_line(data = BL21_lacI_Hline, aes(x,y/1000),
            size = unit(0.3, "mm"), linetype = 1)+
  geom_point(data = BLR_lacI_Hlim_fit, aes(GR, phi/1000, shape = "BLR lacI"), 
            size = 2)+
  geom_line(data = BLR_lacI_Hline, aes(x,y/1000),
            size = unit(0.3, "mm"), linetype = 2)+
  geom_point(data = BLR_Hlim_fit, aes(GR, phi/1000, shape = "BLR"),
             size = 2)+
  geom_line(data = BLR_Hline, aes(x,y/1000), 
             size = unit(0.3, "mm"), linetype = 2)+
  geom_point(data = MG1655_lacI_Hlim_fit, aes(GR, phi/1000, shape = "MG1655 lacI"),
             size = 2)+
  geom_line(data = MG1655_lacI_Hline, aes(x,y/1000), 
            size = unit(0.3, "mm"), linetype = 3)+
  geom_point(data = MG1655_Hlim_fit, aes(GR, phi/1000, shape = "MG1655"),
             size = 2)+
  geom_line(data = MG1655_Hline, aes(x,y/1000), 
            size = unit(0.3, "mm"), linetype = 3)+
  scale_shape_manual(values = c("MG1655" = 1, "BLR" = 0, 
                                "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                "BL21 lacI" = 2), 
                     name = "")+
  ylim(c(0,3))+
  xlim(c(0.5,1.25))+
  xlab(expression("Maximum growth rate ("~h^-1~")"))+
  ylab(expression(10^3~FU~OD^-1))+
  theme_classic()+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        legend.title = element_text(size = unit(8, "mm"), face = "bold"),
        legend.text = element_text(size = unit(8, "mm")),
        legend.box.margin = margin(0,0, 0, 0),
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_text(size = unit(8, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 2.2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))+
  theme(aspect.ratio=1/1)

Hline
library(cowplot)
library(ggpubr)
library(gridExtra)
library(tidyverse)

p_leg <- cowplot::get_legend(Hline)
mytheme <- ttheme_default(base_size = unit(8, "mm"))
names(tab)
tab2 <- tab %>% select(strain, `slope (10^3)`)
p2 <- tableGrob(tab2, rows = NULL, theme = mytheme)
p2$widths <- unit(c(20,30,20), "mm")

## we don't want the legend twice
p <- Hline + theme(legend.position = "none")