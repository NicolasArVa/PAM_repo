library(tidyverse)
library(ggpubr)


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
