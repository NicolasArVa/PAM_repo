#---------------------------- merR ---------------------------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(magrittr)

{
  con <- c(0,1,2,5,10,25,50,125,250,500,1000,2000) # merR concentrations 
  list <- list()
  
  # Assuming the file is in current working directory
  for (i in 1:length(con)){
    tab <- read_excel("data_raw/merR_rawData_0-2000nM.xlsx", sheet = i) %>%
      set_names(c("time", paste0("od.", 1:9), "-", paste0("flu.", 1:9)))
    
    od <- tab %>% select(matches("od.*")) %>% slice(1:(nrow(.)-4)) %>% mutate_all(~.-0.08)
    flu <- tab %>% select(matches("flu.*")) %>% slice(5:nrow(.))
    phi <- as_tibble(flu/od) %>% 
      set_names(str_replace(names(.), "flu", "phi"))
    
    diff_od <- apply(od, 2, diff)
    diff_flu <- apply(flu, 2, diff)
    diff_phi <- apply(phi, 2, diff)
    dt <- diff(tab$time)[1:nrow(diff_od)]
    denom <- apply(od[1:length(dt),], 2, function(x) x*dt)
    
    gr <- as_tibble(diff_od/denom) %>% 
      set_names(str_replace(names(.), "od", "gr"))
    pr <- as_tibble(diff_flu/denom) %>% 
      set_names(str_replace(names(.), "flu", "pr"))
    phir <- apply(diff_phi, 2, function(x) x/dt) %>% 
      as_tibble %>%
      set_names(str_replace(names(.), "phi", "phir"))
    
    list[[i]] <- tibble(time = tab$time[1:nrow(gr)], inductor = con[i], strain = "Neb", 
                   od[1:nrow(gr),], flu[1:nrow(gr),], phi[1:nrow(gr),], 
                   gr, pr, phir) %>% 
      pivot_longer(cols = matches("[a-z]*\\.\\d*"), 
                   names_to = c("measure", "rep"), 
                   names_sep = "\\.",
                   values_to = "value") %>%
      pivot_wider(names_from = measure, values_from = value) %>%
      arrange(rep, time)
  }
  tidy_Hg_Hlim <- list %>% bind_rows
}
