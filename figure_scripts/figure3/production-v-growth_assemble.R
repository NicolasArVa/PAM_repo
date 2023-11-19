library(tidyverse)
library(ggpubr)
library(ggrepel)

test <- ggarrange(DE3_plot,PvG_BL21,widths = c(1.125,1), labels = c("A", "B"))
ggsave("Figure3.tiff", test, width = 174, height = 75, units = "mm", dpi = 600)
