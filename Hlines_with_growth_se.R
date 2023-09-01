strs <- unique(tidy_DE3_Hlim$strain)
tables <- list()
m <- 2
{
  for(n in 1:5){
    
    table <- tidy_DE3_Hlim %>% 
      filter(strain == strs[n])
    
    inductor <- sort(unique(table$iptg))
    growth_rate <- c()
    phi <- c()
    gr_se <- c()
    phi_se <- c()
    for(i in seq_along(inductor)){
      growth_smooth <- table %>% filter(iptg == inductor[i]) %>%
        loess(growth_rate~time, data=., span=0.2)
      
      phi_smooth <- table %>% filter(iptg == inductor[i]) %>%
        loess(phi~time, data=., span=0.2)
      
      gr_err <- table %>% filter(iptg == inductor[i]) %>% .$growth_rate_se
      phi_err <- table %>% filter(iptg == inductor[i]) %>% .$phi_se
      
      gr_se[i] <- gr_err[m]
      phi_se[i] <- phi_err[m]
      growth_rate[i] <- growth_smooth$fitted[m]
      phi[i] <- phi_smooth$fitted[m]
    }
    test_fit <- tibble(strain=strs[n], iptg=inductor, 
                       growth_rate=growth_rate, phi=phi,
                       growth_error = gr_se, phi_error = phi_se)
    tables[[n]] <- test_fit
  }
  
  
  for(i in 1:5){
    model <- lm(phi~growth_rate,data=tables[[i]])
    x <- tables[[i]]$growth_rate
    gr = seq(min(x), max(x), length.out=20)
    phi_hat <- predict(model, newdata = data_frame(growth_rate=gr))
    
    reg <- tibble(strain=unique(tables[[i]]$strain),
                  phi=phi_hat,
                  growth_rate=gr)
    assign(paste0("regression",i), reg)
  }
  
  pic <- ggplot()+
    geom_point(data = tables[[1]], aes(growth_rate, phi/1000, shape = strain),
               size = 2)+
    geom_errorbar(data=tables[[1]], aes(x = growth_rate, y = phi/1000,
                                        xmin = growth_rate - qnorm(0.975)*growth_error,
                                        xmax = growth_rate + qnorm(0.975)*growth_error))+
    geom_line(data = regression1, aes(growth_rate, phi/1000), 
              size = unit(0.3, "mm"), linetype = 3)+
    geom_point(data = tables[[2]], aes(growth_rate, phi/1000, shape = strain),
               size = 2)+
    geom_errorbar(data=tables[[2]], aes(x = growth_rate,y = phi/1000,
                                        xmin = growth_rate - qnorm(0.975)*growth_error,
                                        xmax = growth_rate + qnorm(0.975)*growth_error))+
    geom_line(data = regression2, aes(growth_rate, phi/1000), 
              size = unit(0.3, "mm"), linetype = 3)+
    geom_point(data = tables[[3]], aes(growth_rate, phi/1000, shape = strain),
               size = 2)+
    geom_errorbar(data=tables[[3]], aes(x = growth_rate,y = phi/1000,
                                        xmin = growth_rate - qnorm(0.975)*growth_error,
                                        xmax = growth_rate + qnorm(0.975)*growth_error))+
    geom_line(data = regression3, aes(growth_rate, phi/1000), 
              size = unit(0.3, "mm"), linetype = 3)+
    geom_point(data = tables[[4]], aes(growth_rate, phi/1000, shape = strain),
               size = 2)+
    geom_errorbar(data=tables[[4]], aes(x = growth_rate,y = phi/1000,
                                        xmin = growth_rate - qnorm(0.975)*growth_error,
                                        xmax = growth_rate + qnorm(0.975)*growth_error))+
    geom_line(data = regression4, aes(growth_rate, phi/1000), 
              size = unit(0.3, "mm"), linetype = 3)+
    geom_point(data = tables[[5]], aes(growth_rate, phi/1000, shape = strain),
               size = 2)+
    geom_errorbar(data=tables[[5]], aes(x = growth_rate,y = phi/1000,
                                        xmin = growth_rate - qnorm(0.975)*growth_error,
                                        xmax = growth_rate + qnorm(0.975)*growth_error))+
    geom_line(data = regression5, aes(growth_rate, phi/1000), 
              size = unit(0.3, "mm"), linetype = 3)+
    scale_shape_manual(values = c("MG1655" = 1,  
                                  "MG1655 lacI" = 13,
                                  "BL21 lacI"= 2, 
                                  "BLR" = 0,  
                                  "BLR lacI" = 7), 
                       name = "Strain")+
    ylim(c(0,NA))+
    xlab(expression(Growth~rate~(h^-1)))+
    ylab(expression(10^3~FU~OD^-1))+
    theme_classic()+
    theme(plot.margin = margin(1,1,1.5,1, "mm"),
          legend.title = element_text(size = unit(8, "mm"), face = "bold"),
          legend.text = element_text(size = unit(8, "mm")),
          legend.box.margin = margin(0,0,0,0),
          legend.key.size = unit(4, "mm"),
          axis.title.x = element_text(size = unit(8, "mm"), vjust = -1),
          axis.title.y = element_text(size = unit(8, "mm"), vjust = 2.2),
          axis.text.x = element_text(size = unit(8, "mm")),
          axis.text.y = element_text(size = unit(8, "mm")),
          aspect.ratio=1/1)
  pic
  ggsave("new_Hlines.tiff", pic, width = 84, height = 80, units = "mm", dpi = 600)
}
