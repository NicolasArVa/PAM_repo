install.packages("ggpubr")
library(ggpubr)
library("cowplot")

DE3_plot
CymR_plot
merR_plot

library(ggthemes)
library(gridExtra)
{
  a <- ggarrange(CymR_plot, merR_plot, labels = c("B", "C"),
                 ncol = 1, nrow = 2, align = "v")+
    theme(plot.margin = margin(0,0,0,0, "mm"))
  
  p1 <- ggarrange(DE3_plot, a, legend,
                  labels = c("A","", ""),
                  ncol = 3, nrow = 1,
                  widths = c(3,1.8, 0.9))+
    theme(plot.margin = margin(2,2,2,2, "mm"))

  
  ggsave("Figure3.tiff", p1, width = 174, height = 61.05, units = "mm", dpi = 600)
}
p1
p2 <- annotate_figure(p1,
                      #left = text_grob(expression(10^3~FU~OD^-1),
                      face = "bold",
                      rot = 90,
                      size = 12,
                      vjust = 1) 
  
  
  