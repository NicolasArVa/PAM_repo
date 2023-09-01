library(readxl)
library(tidyverse)
setwd(paste0(getwd(), "/data_raw"))
#------------------------- Data Wrangling ----------------------------------------------------------------------------------
{ con <- c(1, 10, 62, 100, 250, 1000)
  cn <- c("time", "od1", "od2","od3","od4", "-", "time2", "flu1", "flu2", "flu3", "flu4")
  k1 <- c("MG1655", "BLR", "MG1655 lacI", "BLR lacI") 
  k2 <- c("MG1655", "BLR", "MG1655-lacI", "BLR-lacI")
  
  
  for(j in 1:4){
    s <- c("MG1655pT7_Hlim_all", "BLRpT7_Hlim_all", "MG1655lacIpT7_Hlim_all", "BLRlacIpT7_Hlim_all")
    
    # Assuming the file is in current working directory
    for(i in 1:length(con)){
      filename <- paste0(k2[j], "DE3_Hlim.xlsx")
      tab <- read_excel(filename, sheet = i) 
      tab <- tab %>%  set_names(cn)
      t <- tab %>% select(time)
      od <- tab %>% select(od1:od4) %>% mutate_all(~.-0.1)
      flu <- tab %>% select(flu1:flu4)
      phi <- flu/od
      
      diff_od <- apply(od, 2, diff)
      diff_flu <- apply(flu, 2, diff)
      diff_t <- apply(t, 2, diff)
      
      gr <- diff_od/(od*diff_t)
      pr <- diff_flu/(od*diff_t)
      
      base <- bind_cols(t, od, flu, phi, gr, pr) %>% 
        mutate(conc = con[i], strain = k1[j])
      
      bn <- paste0("base", i)
      assign(bn, base)
    }
    r <- 1:4
    names <- c("time", paste0("od", r), paste0("flu", r), paste0("phi", r), 
      paste0("gr", r), paste0("pr", r), "conc", "strain")
    tidy_base <- bind_rows(base1, base2, base3, base4, base5, base6) %>%
      set_names(names)
    
    
    time <- tidy_base %>% select(time)
    od <- tidy_base %>% select(od1:od4) %>% apply(1, mean)
    flu <- tidy_base %>% select(flu1:flu4) %>% apply(1, mean)
    phi <- tidy_base %>% select(phi1:phi4) %>% apply(1, mean)
    gr <- tidy_base %>% select(gr1:gr4) %>% apply(1, mean)
    pr <- tidy_base %>% select(pr1:pr4) %>% apply(1, mean)
    conc <- tidy_base %>% select(conc)
    strain <- tidy_base %>% select(strain)
      
    tab <- bind_cols(time, strain, conc, od, flu, phi, gr, pr) %>% 
      set_names(c("time", "strain", "iptg", "od", "fluorescence", 
                  "phi", "growth_rate", "production_rate"))
    assign(s[j],tab)
    }
}
################################--BL21 case--#################################################
{
    cn <- c("time", "od1", "od2","od3","od4", "od5", "od6","od7","od8",
            "-", "time2", "flu1", "flu2", "flu3", "flu4", "flu5", "flu6", "flu7", "flu8")
    con <- c(0, 1, 10, 62, 80, 100, 125, 250, 500, 1000)
    
    
    # Assuming the file is in current working directory
    for(i in 1:length(con)){
      tab <- read_excel("BL21-lacIDE3_Hlim.xlsx", sheet = i)
      tab <- tab %>%  set_names(cn)
      t <- tab %>% select(time)
      od <- tab %>% select(od1:od8) %>% mutate_all(~.-0.1)
      flu <- tab %>% select(flu1:flu8)
      phi <- flu/od
      
      diff_od <- apply(od, 2, diff)
      diff_flu <- apply(flu, 2, diff)
      diff_t <- apply(t, 2, diff)
      
      gr <- diff_od/(od*diff_t)
      pr <- diff_flu/(od*diff_t)
      
      base <- bind_cols(t, od, flu, phi, gr, pr) %>% 
        mutate(conc = con[i], strain = "BL21 lacI")
      
      bn <- paste0("base", i)
      assign(bn, base)
    }
    r <- 1:8
    names <- c("time", paste0("od", r), paste0("flu", r), paste0("phi", r), 
               paste0("gr", r), paste0("pr", r), "conc", "strain")
    tidy_base <- bind_rows(base1, base2, base3, base4, base5, base6, 
                           base7, base8, base9, base10) %>%
      set_names(names)
    
    time <- tidy_base %>% select(time)
    od <- tidy_base %>% select(od1:od8) %>% apply(1, mean)
    flu <- tidy_base %>% select(flu1:flu8) %>% apply(1, mean)
    phi <- tidy_base %>% select(phi1:phi8) %>% apply(1, mean)
    gr <- tidy_base %>% select(gr1:gr8) %>% apply(1, mean)
    pr <- tidy_base %>% select(pr1:pr8) %>% apply(1, mean)
    conc <- tidy_base %>% select(conc)
    strain <- tidy_base %>% select(strain)
    
    BL21lacIpT7_Hlim_all <- bind_cols(time, conc, od, flu, phi, gr, pr, 
                                      od_se, flu_se, phi_se, gr_se, pr_se) %>% 
      set_names(c("time", "strain", "iptg", "od", "fluorescence", 
                  "phi", "growth_rate", "production_rate"))
}




tidy_DE3_Hlim <- bind_rows(MG1655pT7_Hlim_all, MG1655lacIpT7_Hlim_all,
                           BLRpT7_Hlim_all, BLRlacIpT7_Hlim_all, 
                           BL21lacIpT7_Hlim_all)

