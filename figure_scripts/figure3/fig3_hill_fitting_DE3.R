library(tidyverse)
library(ggpubr)
library(expandFunctions)
library(pracma)
#------------------------ non-linear regression --------------------------------------------
{
  s <- c("MG1655", "MG1655 lacI", "BLR", "BLR lacI", "BL21 lacI")
  DE3predictions_list <-lapply(1:length(s), function(i){
    
    bl <- tidy_DE3_Hlim %>% 
      filter(strain == s[i], between(time,0.5,10)) %>%
      filter(iptg == min(iptg)) %>% 
      select(phi)
    
    x <- tidy_DE3_Hlim %>% 
      filter(strain == s[i], between(time,0.5,10)) %>% 
      group_by(iptg) %>% 
      mutate(phi = phi - bl) %>%
      reframe(phi = max(phi))
    
    model <- x %>% 
      nls(phi~a*(iptg^k / (iptg^k + b^k)), 
          data = ., start = list(a=8000, b=50, k = 2),
          algorithm="port", lower=c(0,0,1))
    
    iptg <- 0:1000
    fi_hat <- predict(model, newdata = tibble(iptg = iptg))
    
    # Calculate standard errors for predictions using DELTA METHOD:
    a <- coef(model)["a"]
    b <- coef(model)["b"]
    k <- coef(model)["k"]
    parameter_standard_errors <- sqrt(diag(vcov(model)))
    
    prediction_error <- sqrt(
      ((iptg)^k / ((iptg)^k + b^k))^2 * parameter_standard_errors["a"]^2 +
        (a * k * b^(k-1) * (iptg)^k / ((iptg)^k + b^k)^2 )^2 * parameter_standard_errors["b"]^2 +
        (a * b^k * log(iptg / b) * (iptg)^k / ((iptg)^k + b^k)^2 )^2 * parameter_standard_errors["k"]^2
    )
    
    tibble(strain = s[i], iptg = 0:1000, fi = fi_hat,
           # Calculate lower and upper bounds for the confidence intervals
           lower = fi_hat - qnorm(0.975) * prediction_error, 
           upper = fi_hat + qnorm(0.975) * prediction_error, 
           error = prediction_error)
    
    
  })
  DE3predictions_table <- bind_rows(DE3predictions_list)
  
  ##----------------------------  Plot -----------------------------------------------------------------------------
  #----------- Heterologous fraction as a hyperbolic function ---------------------------------------------
  
  bl <- tidy_DE3_Hlim %>% 
    group_by(strain) %>%
    filter(iptg == min(iptg), between(time,0.5,10)) %>% 
    select(time, phi) %>% 
    rename(bl = phi) %>% 
    ungroup
  
  DE3_plot <- tidy_DE3_Hlim %>% 
    left_join(bl, by = c("strain", "time")) %>%
    filter(between(time,0.5,10))%>%
    group_by(strain, iptg) %>% 
    mutate(phi = phi - bl) %>%
    summarize(phi = max(phi)) %>%
    filter(!iptg %in% c(0,1))%>%
    ggplot(aes(iptg/1000, phi/1000, shape = strain))+
    theme_classic()+
    geom_line(data = DE3predictions_table, aes(iptg/1000, fi/1000, linetype = strain), 
              size = unit(0.3, "mm"), show.legend = F)+
    geom_point(aes(shape = strain), size = 3, show.legend = F)+
    #geom_point(data = data.frame(iptg) , aes(iptg/1000, H/1000, shape="NEB Stable"), size = 3)+
    xlab("IPTG concentration (mM)")+
    ylab(expression(10^3~FU~OD^-1))+
    scale_y_continuous(limit = c(0,14), breaks = seq(0, 14, by = 4))+
    scale_shape_manual(values = c("MG1655" = 1, "BLR" = 0, 
                                  "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                  "BL21 lacI" = 2), 
                       name = "Strain")+
    scale_linetype_manual(values = c("MG1655" = 3, "BLR" = 2, 
                                     "MG1655 lacI" = 3, "BLR lacI" = 2, 
                                     "BL21 lacI" = 1), 
                          name = "Strain")+
    theme(plot.margin = margin(0,0,0,0, "mm"),
          legend.title = element_text(size = unit(6, "mm"), face = "bold"),
          legend.text = element_text(size = unit(6, "mm")),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.key.size = unit(5, "mm"),
          axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
          axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
          axis.text.x = element_text(size = unit(8, "mm")),
          axis.text.y = element_text(size = unit(8, "mm")))+
    theme(aspect.ratio=4/6)
  
  DE3_plot
}
#-------------------------------------------------------------------------------------------------------