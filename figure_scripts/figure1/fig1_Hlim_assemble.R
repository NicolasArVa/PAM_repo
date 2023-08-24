library(tidyverse)
library(ggpubr)
library(cowplot)

mytheme <- ttheme_default(base_size = unit(8, "mm"))
plot_slope_table <- tableGrob(slope_table, rows = NULL, theme = mytheme)

fig1 <- ggarrange(plot_slope_table,
                  Hline_B,
                  Hline_K,
                  nrow = 3,
                  heights = c(0.8,1,1),
                  labels = c("A", "B", "C"))
fig1


ggsave("Figure1.tiff", fig1, width = 84, height = 160, units = "mm", dpi = 600)
