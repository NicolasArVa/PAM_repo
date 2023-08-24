fig5_part1 <- ggarrange(tab_H, tab_R0,
                        nrow =  2,
                        labels = c("A", "B"),
                        widths = c(1.5,1))

fig5 <- ggarrange(fig5_part1, plot_stress, 
                  ncol = 2,
                  labels = c("", "C"), widths = c(1,1.5))
fig5<-annotate_figure(fig5, 
                      bottom = text_grob(expression("Maximum growth rate ("~h^-1~")"),
                                         size = unit(8, "mm")))+
  theme(plot.margin = margin(1,1,1,1, "mm"))
fig5
ggsave("Figure5.tiff",
       fig5, width = 174, height = 100, units = "mm", dpi = 600)