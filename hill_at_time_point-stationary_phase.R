{
phi_table_test <- tidy_DE3_Hlim_blanked %>%
  filter(time < 17) %>%
  summarise(max_phi = phi[49], 
            .by = c(strain, inductor, rep)) %>%
  group_by(strain, inductor) %>%
  summarise(phi = mean(max_phi), 
            phi_sd = sd(max_phi), 
            phi_se = sd(max_phi)/(n()^0.5))


results <- lapply(split(phi_table_test, phi_table_test$strain), function(x){
  model <- nls(phi~hill(inductor, H, k, n),
               data = x,
               start = list(H = 5000, k = 10, n = 1),
               algorithm = "port", lower = c(0,0,1),
               weights = 1/((x$phi_sd)^2 + 1e-4))
  cons <- summary(model) %>% coef()
  
  phi_hat <- predict(model, tibble(inductor = 0:1000))
  tab <- tibble(strain = x$strain[1], phi_hat = phi_hat, inductor = 0:1000)
  list(constants = cons, prediction = tab)
})

# Extract results from the regression
phi_constants <- lapply(results, function(x) x$constants)
phi_curves <- lapply(results, function(x) x$prediction) %>% bind_rows()

# Final plot
strain_cols <- c("BL21 lacI"="#e60049", "BLR"="#0bb4ff", 
                 "BLR lacI"="#50e991","MG1655"="#ffa300", 
                 "MG1655 lacI"="#9b19f5")

a <- phi_table_test %>% mutate(mode = "phi[H]~maxima", label = "A") %>% 
  rename(value = phi, se = phi_se) %>% 
  mutate(mode = factor(mode))

x <- phi_curves %>% mutate(mode = "phi[H]~maxima") %>% 
  rename(value = phi_hat) %>% 
  mutate(mode = factor(mode))

DE3_plot_phi <- a %>%
  filter(inductor > 0) %>%
  ggplot(aes(inductor/1000, value/1000, shape = strain))+
  ggtitle("phi selected after 12 hours")+
  theme_classic()+
  geom_errorbar(aes(ymin = value/1000 -se/1000,
                    ymax = value/1000 + se/1000),
                linewidth = 0.2)+
  geom_point(aes(shape = strain, color = strain), size = 2)+
  #geom_text(aes(0.1,13, label = label))+
  geom_line(data = x, aes(inductor/1000, value/1000, linetype = strain),
            show.legend = F)+
  facet_wrap(~strain)+
  xlab("IPTG concentration (mM)")+
  ylab(expression(10^3~FU~OD[600]^-1))+
  scale_y_continuous(limit = c(0,14), breaks = seq(0, 14, by = 4))+
  scale_shape_manual(values = c("MG1655" = 1, "BLR" = 0, 
                                "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                "BL21 lacI" = 2), 
                     name = "Strain",
                     labels= eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  scale_color_manual(name = 'Strain',
                     values = strain_cols,
                     labels = eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  scale_linetype_manual(values = c("MG1655" = 3, "BLR" = 2, 
                                   "MG1655 lacI" = 3, "BLR lacI" = 2, 
                                   "BL21 lacI" = 1), 
                        name = "Strain",
                        labels= eval(parse_exprs(c("BL21~lacI^OV", "BLR", "BLR~lacI^OV","MG1655", "MG1655~lacI^OV"))))+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        #legend.position = "top",
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 1),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        strip.text = element_text(size = unit(8, "mm")),
        strip.background = element_rect(color = 'white'),
        aspect.ratio=1/1.5)
DE3_plot_phi
}
