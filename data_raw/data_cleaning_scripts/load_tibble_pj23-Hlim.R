#---------------------------- CymR ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(magrittr)
{
  cn <- c("time", "od1", "od2","od3","od4", "od5", "od6","od7","od8", "od9", 
          "-", "time2", "flu1", "flu2", "flu3", "flu4", "flu5", "flu6", "flu7", "flu8", "flu9")
  
  con <- c(0,125,250,500,1000) # CymR concentrations
  
  # Assuming the file is in current working directory
  for (i in 1:length(con)){
    tab <- read_excel("data_raw/MG1655pj23_Hlim.xlsx", sheet = i) %>% 
      set_names(cn)  
    tab <- tab %>%  set_names(cn)
    t <- tab %>% select(time)
    od <- tab %>% select(od1:od9) %>% mutate_all(~.-0.1)
    flu <- tab %>% select(flu1:flu9)
    phi <- flu/od
    
    diff_od <- apply(od, 2, diff)
    diff_flu <- apply(flu, 2, diff)
    diff_t <- apply(t, 2, diff)
    
    gr <- diff_od/(od*diff_t)
    pr <- diff_flu/(od*diff_t)
    
    base <- bind_cols(t, od, flu, phi, gr, pr) %>% 
      mutate(conc = con[i])
    
    bn <- paste0("base", i)
    assign(bn, base)
  }
  
  r <- 1:9
  names <- c("time", paste0("od", r), paste0("flu", r), paste0("phi", r), 
             paste0("gr", r), paste0("pr", r), "conc")
  
  tidy_base <- bind_rows(base1, base2, base3, base4, base5)%>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  conc <- tidy_base %>% select(conc)
  
  od <- tidy_base %>% select(od1:od9) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu9) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi9) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr9) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr9) %>% apply(1, mean)
  
  od_se <- tidy_base %>% select(od1:od9) %>% apply(1, sd)%>% divide_by(sqrt(9))
  flu_se <- tidy_base %>% select(flu1:flu9) %>% apply(1, sd)%>% divide_by(sqrt(9))
  phi_se <- tidy_base %>% select(phi1:phi9) %>% apply(1, sd)%>% divide_by(sqrt(9))
  gr_se <- tidy_base %>% select(gr1:gr9) %>% apply(1, sd)%>% divide_by(sqrt(9))
  pr_se <- tidy_base %>% select(pr1:pr9) %>% apply(1, sd)%>% divide_by(sqrt(9))
  
  tidy_j23_Hlim <- bind_cols(time, conc, od, flu, phi, gr, pr, 
                             od_se, flu_se, phi_se, gr_se, pr_se) %>% 
    set_names(c("time", "cumate", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate",
                "od_se", "fluorescence_se", 
                "phi_se", "growth_rate_se", "production_rate_se"))
  
}
tidy_j23_Hlim%>%
  ggplot(aes(time,od,group=cumate, color=cumate))+
  geom_line()+
  geom_point()
tidy_j23_Hlim%>%
  ggplot(aes(time,fluorescence,group=cumate, color=cumate))+
  geom_line()+
  geom_point()
tidy_j23_Hlim%>%
  ggplot(aes(time,phi,group=cumate, color=cumate))+
  geom_line()+
  geom_point()
tidy_j23_Hlim%>%
  ggplot(aes(time,production_rate,group=cumate, color=cumate))+
  geom_line()+
  geom_point()
