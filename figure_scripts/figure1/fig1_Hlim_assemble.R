library(tidyverse)
library(ggpubr)
library(cowplot)
library(gridExtra)

mytheme <- ttheme_default(base_size = unit(8, "mm"))
plot_slope_table <- tableGrob(slope_table, rows = NULL, theme = mytheme)

fig1 <- ggarrange(Hline_B,
                  Hline_K,
                  nrow = 2,
                  labels = c("A", "B"))
fig1


ggsave("Figure1.tiff", fig1, width = 84, height = 120, units = "mm", dpi = 600)
