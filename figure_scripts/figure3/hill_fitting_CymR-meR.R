library(tidyverse)
library(ggpubr)
library(caret)
library(minpack.lm)
#--------------------------------- merR ---------------------------------------------------------------------
{
  
  #blc <- tidy_Hg_Hlim %>% 
    #filter(time > 0.5 & time <= 2.5 & Hg == min(Hg)) %>% 
    #select(time, phi)%>%
    #rename(bl = phi)
  
  #merR_tab <- tidy_Hg_Hlim %>% 
    #filter(time > 0.5 & time <= 2.5) %>% 
    #left_join(blc, by="time") %>%
    #mutate(phi = phi - bl) %>%
    #group_by(Hg) %>%
    #summarize(phi = max(phi))
  
  # == a Hg_slopes
  merR_tab <- tidy_Hg_Hlim %>% 
    filter(between(time, 2, 8)) %>%
    group_by(Hg)%>%
    summarize(slope=lm(production_rate~growth_rate)$coefficients["growth_rate"])
  
  model <- nls(slope~a*((Hg^k) /((Hg^k) + (b^k))), 
                    data = merR_tab%>%filter(Hg!=2000),
                    start = list(a=2000, b=100, k =1), 
               algorithm = 'port', lower=c(0,0,1))
  
  hg <- 0:1000
  fi_hat <- predict(model, newdata = tibble(Hg = hg))

  # Calculate standard errors for predictions using DELTA METHOD:
  a <- coef(model)["a"]
  b <- coef(model)["b"]
  k <- coef(model)["k"]
  parameter_standard_errors <- sqrt(diag(vcov(model)))
  
  prediction_error <- sqrt(
    ((hg)^k / ((hg)^k + b^k))^2 * parameter_standard_errors["a"]^2 +
      (a * k * b^(k-1) * (hg)^k / ((hg)^k + b^k)^2 )^2 * parameter_standard_errors["b"]^2 +
      (a * b^k * log(hg / b) * (hg)^k / ((hg)^k + b^k)^2 )^2 * parameter_standard_errors["k"]^2
  )
  
  tab_merR <- tibble(Hg = hg, fi = fi_hat,
         # Calculate lower and upper bounds for the confidence intervals
         lower = fi_hat - qnorm(0.975) * prediction_error, 
         upper = fi_hat + qnorm(0.975) * prediction_error,
         error = prediction_error)
  
#----------- Heterologous fraction as a hyperbolic function ---------------------------------- 
  #__wrangling__
  #merR_plot <- tidy_Hg_Hlim %>% 
    #filter(time > 0.5) %>% 
    #left_join(blc, by="time")%>%
    #mutate(phi = phi - bl) %>%
    #group_by(Hg) %>% 
    #summarize(phi = max(phi, na.rm = TRUE)) %>%
    #ungroup()%>%
  merR_plot <- merR_tab %>%
  # __plot__  
    ggplot(aes(Hg, slope/1000))+
    theme_classic()+
    geom_point(shape = 5, size = 3)+
    geom_line(data = tab_merR, aes(Hg, fi/1000), size = unit(0.3, "mm"))+
    xlab(expression(Hg~concentration~(nM)))+
    xlim(c(0,1000))+
    ylab("")+
    ylim(c(0,10))+
    theme(plot.margin = margin(0,0,0,0, "mm"),
          legend.title = element_text(size = unit(6, "mm"), face = "bold"),
          legend.text = element_text(size = unit(6, "mm")),
          legend.box.margin = margin(0, 1, 0, 0),
          legend.key.size = unit(5, "mm"),
          axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
          axis.title.y = element_text(size = unit(8, "mm"), vjust = 2),
          axis.text.x = element_text(size = unit(8, "mm")),
          axis.text.y = element_text(size = unit(8, "mm")),
          aspect.ratio=2/3)

merR_plot
#--------------------------------- CymR ---------------------------------------------------------------------
{
  #blc <- tidy_j23_Hlim %>% 
    #filter(time > 0.5, cumate == min(cumate)) %>% 
    #select(time, phi)%>%
    #rename(bl = phi)
  
  #CymR_tab <- tidy_j23_Hlim%>% 
    #filter(time > 0.5) %>% 
    #left_join(blc, by="time")%>%
    #mutate(phi = phi - bl) %>%
    #group_by(cumate) %>%
    #summarize(phi = max(phi))
  
  CymR_tab <- tidy_j23_Hlim %>% 
    filter(time < 3) %>%
    group_by(cumate)%>%
    summarize(slope=lm(production_rate~growth_rate)$coefficients["growth_rate"],
              b=lm(production_rate~growth_rate)$coefficients["(Intercept)"])
  
  model <- nls(slope~a*((cumate^k) /((cumate^k) + (b^k))), 
               data = CymR_tab,
               start = list(a=2000, b=100, k =1), 
               algorithm = 'port', lower=c(0,0,1))
  
  cumate <- 0:1000
  fi_hat <- predict(model, newdata = tibble(cumate = cumate))
  
  
  # Calculate standard errors for predictions using DELTA METHOD:
  a <- coef(model)["a"]
  b <- coef(model)["b"]
  k <- coef(model)["k"]
  parameter_standard_errors <- sqrt(diag(vcov(model)))
  
  prediction_error <- sqrt(
    ((cumate)^k / ((cumate)^k + b^k))^2 * parameter_standard_errors["a"]^2 +
      (a * k * b^(k-1) * (cumate)^k / ((cumate)^k + b^k)^2 )^2 * parameter_standard_errors["b"]^2 +
      (a * b^k * log(cumate / b) * (cumate)^k / ((cumate)^k + b^k)^2 )^2 * parameter_standard_errors["k"]^2
  )
  
  tab_CymR <- tibble(cumate = cumate, fi = fi_hat,
         # Calculate lower and upper bounds for the confidence intervals
         lower = fi_hat - qnorm(0.975) * prediction_error, 
         upper = fi_hat + qnorm(0.975) * prediction_error, 
         error = prediction_error)
} 
  
#------------ Heterologous fraction as a hyperbolic function ------------------------------------------------------
CymR_plot <- CymR_tab%>% #tidy_j23_Hlim %>% 
    #filter(time > 0.5) %>% 
    #left_join(blc, by="time")%>%
    #mutate(phi = phi - bl) %>%
    #group_by(cumate) %>% 
    #summarize(phi = max(phi, na.rm = TRUE)) %>%
    #ungroup()%>%
    ggplot(aes(cumate/1000, slope/1000))+
    geom_point(shape = 1, size = 3)+
    geom_line(data = tab_CymR, aes(cumate/1000, fi/1000), size = unit(0.3, "mm"))+
    theme_classic()+
    xlab(expression(Cumate~concentration~(mu*M)))+
    xlim(c(0,1.1))+
    ylab("")+
    #ylim(c(0,1.55))+
    theme(plot.margin = margin(0,0,0,0, "mm"),
          legend.title = element_text(size = unit(6, "mm"), face = "bold"),
          legend.text = element_text(size = unit(6, "mm")),
          legend.box.margin = margin(0, 0, 0, 0),
          legend.key.size = unit(5, "mm"),
          axis.title.x = element_text(size = unit(9, "mm"), vjust = 1.5),
          axis.title.y = element_text(size = unit(8, "mm"), vjust = 2),
          axis.text.x = element_text(size = unit(8, "mm")),
          axis.text.y = element_text(size = unit(8, "mm")),
          aspect.ratio=2/3)
}


CymR_plot
merR_plot
