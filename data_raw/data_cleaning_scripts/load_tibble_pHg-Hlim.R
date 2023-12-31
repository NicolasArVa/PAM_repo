#---------------------------- merR ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
{
  
  cn <- c("time", "od1", "od2","od3","od4", "od5", "od6","od7","od8", "od9", 
          "-", "time2", "flu1", "flu2", "flu3", "flu4", "flu5", "flu6", "flu7", "flu8", "flu9")
  
  
  con <- c(0,1,2,5,10,25,50,125,250) # merR concentrations 
  
  
  # Assuming the file is in current working directory
  for (i in 1:length(con)){
    tab <- read_excel("nebpHg_Hlim.xlsx", sheet = i) %>% 
      set_names(cn)  
    tab <- tab %>%  set_names(cn)
    t <- tab %>% select(time)
    od <- tab %>% select(od1:od9)
    od <- od - 0.1
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
  tidy_base <- bind_rows(base1, base2, base3, base4, base5, base6, base7, base8, base9) %>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  od <- tidy_base %>% select(od1:od9) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu9) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi9) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr9) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr9) %>% apply(1, mean)
  conc <- tidy_base %>% select(conc)
  
  tidy_Hg_Hlim <- bind_cols(time, conc, od, flu, phi, gr, pr) %>% 
    set_names(c("time", "Hg", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate"))
  
}
