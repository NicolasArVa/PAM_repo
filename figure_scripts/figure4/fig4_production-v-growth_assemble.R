library(tidyverse)
library(ggpubr)
library(ggrepel)

pre <- ggarrange(PvG_BL21, PvG_grid,
                 widths = c(1,1.355))



fig4 <- annotate_figure(pre, 
                        bottom = text_grob(expression(Growth~rate~(h^-1)),
                                           size = unit(8, "mm"),
                                           vjust = -0.8,
                                           hjust = 2.7)
)
fig4
ggsave("Figure4.tiff", fig4, width = 174, height = 85, units = "mm", dpi = 600)

#test <- ggarrange(PvG_BL21,slope_grid,nrow = 2, heights = c(1,2.4))
#ggsave("Figure4_test.tiff", test, width = 174, height = 200, units = "mm", dpi = 600)


test <- ggarrange(DE3_plot,PvG_BL21,widths = c(1.42,1), labels = c("A", "B"))
ggsave("Figure3_test.tiff", test, width = 174, height = 90, units = "mm", dpi = 600)
