library(ggpubr)
GR_max_table <- tidy_DE3_Hlim %>% 
  filter(iptg == 250, time <= 3) %>% 
  group_by(strain) %>%
  summarize(t = time[which.max(growth_rate)], max = max(growth_rate), 
            phi = phi[which.max(growth_rate)])
  


part1 <- tidy_DE3_Hlim %>% filter(iptg == 250, time <= 3) %>% 
  ggplot(aes(time, growth_rate, shape = strain))+
  theme_classic()+
  geom_point()+
  geom_line()+
  geom_point(data = GR_max_table, 
             aes(t, max, color = strain), shape = 21, size = 5)+
  ylab(expression(Growth~rate~(h^-1)))+
  xlab('Time (h)')+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        legend.position = 'none',
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))


part2 <- tidy_DE3_Hlim %>% filter(iptg == 250, time <= 3) %>% 
  ggplot(aes(time, phi/1000, shape = strain))+
  theme_classic()+
  geom_point()+
  geom_line()+
  geom_point(data = GR_max_table, 
             aes(t, phi/1000, color = strain), shape = 21, size = 5)+
  ylab(expression(Specific~fluorescence~(10^3~Flu~OD[600]^-1)))+
  xlab('Time (h)')+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))

legend <- get_legend(part2)


max_gr_phi <- ggarrange(part1, part2 + theme(legend.position = 'none'), legend,
          ncol = 3, widths = c(3,3,1),
          labels = LETTERS[c(1,2)])
ggsave("Figure2S.tiff", max_gr_phi, width = 174, height = 90, units = c('mm'), dpi = 600)
