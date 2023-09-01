library(tidyverse)
library(readxl)

{
  cn <- c("time", paste0("od", 1:6), "-", "-", "-", "time2", paste0("flu", 1:6))
  con <- c("Ace", "Ace caa", "Gly" , "Glu", "Gly caa" , "Glu caa")
  
  # Assuming the file is in current working directory
  for(i in 1:length(con)){
    tab <- read_excel("nebpHg_Clim.xlsx", sheet = i)
    tab <- tab[, 1:17]
    tab <- tab %>%  set_names(cn)
    t <- tab %>% select(time)
    od <- tab %>% select(od1:od6)
    od <- od - 0.1
    flu <- tab %>% select(flu1:flu6)
    phi <- flu/od
    
    diff_od <- apply(od, 2, diff)
    diff_flu <- apply(flu, 2, diff)
    diff_t <- apply(t, 2, diff)
    
    gr <- diff_od/(od*diff_t)
    pr <- diff_flu/(od*diff_t)
    
    od_bl <- od %>% select(od1:od3)
    od <- od %>% select(od4:od6)
    flu_bl <- flu %>% select(flu1:flu3)
    flu <- flu %>% select(flu4:flu6)
    phi_bl <- phi %>% select(flu1:flu3)
    phi <- phi %>% select(flu4:flu6)
    
    gr_bl <- gr %>% as_tibble() %>% select(od1:od3)
    gr <- gr %>% as_tibble() %>% select(od4:od6)
    pr_bl <- pr %>% as_tibble() %>% select(od1:od3)
    pr <- pr %>% as_tibble() %>% select(od4:od6)
    
    base_bl <- bind_cols(t, od_bl, flu_bl, phi_bl, gr_bl, pr_bl) %>% 
      mutate(conc = con[i])
    bn_bl <- paste0("base_bl", i)
    assign(bn_bl, base_bl)
    
    base <- bind_cols(t, od, flu, phi, gr, pr) %>% 
      mutate(conc = con[i])
    bn <- paste0("base", i)
    assign(bn, base)
  }
  r <- 1:3
  names <- c("time", paste0("od", r), paste0("flu", r), paste0("phi", r), 
             paste0("gr", r), paste0("pr", r), "conc")
  
  tidy_base <- bind_rows(base1, base2, base3, base4, base5, base6) %>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  conc <- tidy_base %>% select(conc)
  
  od <- tidy_base %>% select(od1:od3) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu3) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi3) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr3) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr3) %>% apply(1, mean)
  
  od_se <- tidy_base %>% select(od1:od3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  flu_se <- tidy_base %>% select(flu1:flu3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  phi_se <- tidy_base %>% select(phi1:phi3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  gr_se <- tidy_base %>% select(gr1:gr3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  pr_se <- tidy_base %>% select(pr1:pr3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  
  tidy_Hg_Clim <- bind_cols(time, conc, od, flu, phi, gr, pr, 
                            od_se, flu_se, phi_se, gr_se, pr_se) %>% 
    set_names(c("time", "n_source", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate",
                "od_se", "fluorescence_se", 
                "phi_se", "growth_rate_se", "production_rate_se"))
  
  #-----------------------------------------------------------------------------------------------
  tidy_base_bl <- bind_rows(base_bl1, base_bl2, base_bl3, base_bl4, 
                            base_bl5, base_bl6) %>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  conc <- tidy_base %>% select(conc)
  
  od <- tidy_base %>% select(od1:od3) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu3) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi3) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr3) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr3) %>% apply(1, mean)
  
  od_se <- tidy_base %>% select(od1:od3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  flu_se <- tidy_base %>% select(flu1:flu3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  phi_se <- tidy_base %>% select(phi1:phi3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  gr_se <- tidy_base %>% select(gr1:gr3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  pr_se <- tidy_base %>% select(pr1:pr3) %>% apply(1, sd) %>% divide_by(sqrt(3))
  
  tidy_Hg_Clim_bl <- bind_cols(time, conc, od, flu, phi, gr, pr, 
                               od_se, flu_se, phi_se, gr_se, pr_se) %>% 
    set_names(c("time", "n_source", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate",
                "od_se", "fluorescence_se", 
                "phi_se", "growth_rate_se", "production_rate_se"))
  
}
