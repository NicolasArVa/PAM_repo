library(tidyverse)
library(rlang)
library(ggpubr)
library(cowplot)
strain_cols <- c("BL21 lacI"="#e60049", "BLR"="#0bb4ff", 
                 "BLR lacI"="#50e991","MG1655"="#ffa300", 
                 "MG1655 lacI"="#9b19f5")


{
fig1_list <- tidy_DE3_exp %>%
    filter(inductor == 1000, time <= 3.5) %>%
    split(.$strain)
  
data_hat <- lapply(fig1_list, function(x){
  gr_model <- loess(gr~time, data = x)
  gr_hat <- predict(gr_model, tibble(time=seq(0,3,0.01)))
  
  phi_model <- loess(phi~time, data = x)
  phi_hat <- predict(phi_model, tibble(time=seq(0,3,0.01)))
  
  tibble(time = seq(0,3,0.01), inductor = x$inductor[1], strain=x$strain[1],
         gr = gr_hat, phi = phi_hat)
}) %>% bind_rows()

GR_max_table <- data_hat %>% 
  reframe(t = time[which.max(gr)], max = max(gr), 
          phi_selected = phi[which.max(gr)], .by = strain)


part1 <- data_hat %>% filter(inductor == 1000, time <= 3) %>% 
  ggplot(aes(time, gr, shape = strain, colour = strain))+
  theme_classic()+
  geom_path()+
  geom_point(data = GR_max_table, 
             aes(t, max, shape = strain), size = 3)+
  geom_segment(data = GR_max_table, 
               aes(x = t, xend = t, y = max, yend = 0), 
               linetype = "dotted") + 
  scale_shape_manual(name = 'Strain',
                     values = c("MG1655" = 1, "BLR" = 0, 
                                "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                "BL21 lacI" = 2), 
                     labels= eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  scale_color_manual(name = 'Strain',
                     values = strain_cols,
                     labels = 
                       eval(
                         parse_exprs(
                           c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  ylim(c(0,0.8))+
  xlim(c(0,2.5))+
  ylab(expression(Growth~rate~(h^-1)))+
  xlab('Time (h)')+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm")),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        legend.position = "none",
        aspect.ratio=1.5/2)
part1

part2 <- data_hat %>% filter(inductor == 1000, time <= 3) %>% 
  ggplot(aes(time, phi/1000, colour = strain))+
  theme_classic()+
  geom_path()+
  geom_point(data = GR_max_table, size = 3,
             aes(t, phi_selected/1000, shape = strain))+
  geom_segment(data = GR_max_table, 
               aes(x = t, xend = t, y = phi_selected/1000, yend = 0), 
               linetype = "dotted") + 
  geom_segment(data = GR_max_table, 
               aes(x = t, xend = 0, y = phi_selected/1000, yend = phi_selected/1000), 
               linetype = "dotted") + 
  scale_shape_manual(name = 'Strain',
                     values = c("MG1655" = 1, "BLR" = 0, 
                                "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                "BL21 lacI" = 2), 
                     labels= eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  scale_color_manual(name = 'Strain',
    values = strain_cols,
    labels = 
      eval(
        parse_exprs(
          c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  scale_y_continuous(limits = c(0, 5), breaks = c(0,1.5,3.0,4.5))+
  xlim(c(0,2.5))+
  ylab(expression((10^3~FU~OD[600]^-1)))+
  xlab('Time (h)')+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 0.5, hjust = 0.7),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        aspect.ratio=1.5/2)
part2
}
{
  strain_list <- tidy_DE3_Hlim %>% 
    reframe(growth = mean(gr), phi = mean(phi),
              .by = c(time, inductor,strain)) %>% split(.$strain)
  
  maxPhiValues <- lapply(strain_list, function(x, a = 1000){
    
    bl <- x %>% 
      filter(between(time,0,10)) %>%
      filter(inductor == 0) %>% 
      .$phi
    
    x %>% 
      filter(between(time,0,10), inductor == a) %>% 
      mutate(phib = phi - bl, .by = inductor) %>%
      reframe(phi_max = max(phib), time = time[which.max(phib)], 
              .by = strain)
  }) %>%
    bind_rows()
  PhiCurves <- lapply(strain_list, function(x, a = 1000){
    
    bl <- x %>% 
      filter(inductor == 0) %>% 
      .$phi
    
    x %>% 
      filter(inductor == a) %>% 
      mutate(phi = phi - bl)
  }) %>% 
    bind_rows()
  
  graph <- PhiCurves %>%
    filter(time <= 12)%>%
    ggplot(aes(time, phi/1000, shape=strain, colour = strain))+
    theme_classic()+
    geom_line()+
    geom_point(data = maxPhiValues, 
               aes(time, phi_max/1000, shape=strain), 
               size = 3, show.legend = F)+
    geom_segment(data = maxPhiValues, 
                 aes(x = time, xend = time, y = phi_max/1000, yend = 0), 
                 linetype = "dotted") +
    geom_segment(data = maxPhiValues, 
                 aes(x = time, xend = 0, y = phi_max/1000, yend = phi_max/1000), 
                 linetype = "dotted") +
    scale_shape_manual(name = 'Strain',
                       values = c("MG1655" = 1, "BLR" = 0, 
                                  "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                  "BL21 lacI" = 2), 
                       labels= eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
    scale_color_manual(name = 'Strain',
                       values = strain_cols,
                       labels = eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
    scale_x_continuous(limits = c(0, 12), n.breaks = 7)+
    scale_y_continuous(limits = c(0, 13), n.breaks = 7)+
    xlab('Time (h)')+
    ylab(expression((10^3~FU~OD[600]^-1)))+
    theme(plot.margin = margin(1,1,1,1, "mm"),
          legend.title = element_text(size = unit(6, "mm"), face = "bold"),
          legend.text = element_text(size = unit(6, "mm")),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.key.size = unit(5, "mm"),
          axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
          axis.title.y = element_text(size = unit(9, "mm"), vjust = 0.5, hjust = 0.7),
          axis.text.x = element_text(size = unit(8, "mm")),
          axis.text.y = element_text(size = unit(8, "mm")),
          legend.position = "none",
          aspect.ratio=1.5/2)
}

#legend <- get_legend(part2)
fig1 <- ggarrange(graph, part1, part2,  
                  hjust = -4, ncol = 3,  
                  common.legend = T, legend = "top",
                  labels = c("A", "B", "C"))
fig1

ggsave("Figure1.tiff", fig1, width = 6.75, height = 2.5, units = "in", dpi = 1200)
ggsave("Figure1.png", fig1, width = 6.75, height = 2.5, units = "in", dpi = 1200)

