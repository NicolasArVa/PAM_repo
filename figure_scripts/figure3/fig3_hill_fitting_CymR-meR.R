library(tidyverse)
library(ggpubr)
library(caret)
library(minpack.lm)
#--------------------------------- merR ---------------------------------------------------------------------
{
  
  blc <- tidy_Hg_Hlim %>% 
    filter(time > 0.5 & time <= 2.5 & Hg == min(Hg)) %>% 
    select(time, phi)%>%
    rename(bl = phi)
  
  merR_tab <- tidy_Hg_Hlim %>% 
    filter(time > 0.5 & time <= 2.5) %>% 
    left_join(blc, by="time") %>%
    mutate(phi = phi - bl) %>%
    group_by(Hg) %>%
    summarize(phi = max(phi))
  
  merR_fit <- nls(phi~a*(Hg /(Hg + b)), 
                    data = merR_tab,
                    start = list(a=2000, b=100), algorithm = 'port')

  nls_merR <- merR_fit %>% 
    summary() %>%
    .$parameters %>% 
    as_tibble() %>% 
    mutate(parameter=c("a", "b"), .before = Estimate)
  nls_merR
  
  a <- nls_merR%>% filter(parameter == "a") %>% pull(Estimate)
  b <- nls_merR%>% filter(parameter == "b") %>% pull(Estimate)
  
  hill_pHg <- function(x){ 
    a*x/(x + b)
    }
  
  tab <- hill_pHg(1:250)
  tab_merR <- data.frame(Hg = c(1:250), H = tab)

  
#----------- Heterologous fraction as a hyperbolic function ---------------------------------- 
merR_plot <- tidy_Hg_Hlim %>% 
    filter(time > 0.5) %>% 
    left_join(blc, by="time")%>%
    mutate(phi = phi - bl) %>%
    group_by(Hg) %>% 
    summarize(phi = max(phi, na.rm = TRUE)) %>%
    ungroup()%>%
    ggplot(aes(Hg, phi/1000))+
    theme_classic()+
    geom_point(shape = 5, size = 3)+
    geom_line(data = tab_merR, aes(Hg, H/1000), size = unit(0.3, "mm"))+
    xlab(expression("Hg concentration ("~n*M~")" ))+
    xlim(c(0,260))+
    ylab("")+
    ylim(c(0,2.55))+
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


#--------------------------------- CymR ---------------------------------------------------------------------
{
  blc <- tidy_j23_Hlim %>% 
    filter(time > 0.5, cumate == min(cumate)) %>% 
    select(time, phi)%>%
    rename(bl = phi)
  
  CymR_tab <- tidy_j23_Hlim%>% 
    filter(time > 0.5) %>% 
    left_join(blc, by="time")%>%
    mutate(phi = phi - bl) %>%
    group_by(cumate) %>%
    summarize(phi = max(phi))
  
  CymR_fit <- CymR_tab %>%
    nls(phi~a*(cumate^m /(cumate^m + b^m)), 
        data = .,start = list(a=1000, b=100, m=1), 
        algorithm="port", 
        lower=c(0,0,1), upper=c(5000,1000,2))
  
  nls_CymR <- CymR_fit %>% 
    summary() %>%
    .$parameters %>% 
    as_tibble() %>% 
    mutate(parameter =c("a", "b", "c"), .before = Estimate)
  
  a <- nls_CymR%>% filter(parameter == "a") %>% pull(Estimate)
  b <- nls_CymR%>% filter(parameter == "b") %>% pull(Estimate)
  c <- nls_CymR%>% filter(parameter == "c") %>% pull(Estimate)
  
  hill_pj23 <- function(x){ 
    a*x^c / (x^c + b^c)
    }
  
  tab <- hill_pj23(1:1000)
  tab_CymR <- data.frame(cumate = c(1:1000), H = tab)
} 
  
#------------ Heterologous fraction as a hyperbolic function ------------------------------------------------------
CymR_plot <-  tidy_j23_Hlim %>% 
    filter(time > 0.5) %>% 
    left_join(blc, by="time")%>%
    mutate(phi = phi - bl) %>%
    group_by(cumate) %>% 
    summarize(phi = max(phi, na.rm = TRUE)) %>%
    ungroup()%>%
    ggplot(aes(cumate/1000, phi/1000))+
    geom_point(shape = 1, size = 3)+
    geom_line(data = tab_CymR, aes(cumate/1000, H/1000), size = unit(0.3, "mm"))+
    theme_classic()+
    xlab(expression("Cumate concentration ("~mu*M~")" ))+
    xlim(c(0,1.1))+
    ylab("")+
    ylim(c(0,1.55))+
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
