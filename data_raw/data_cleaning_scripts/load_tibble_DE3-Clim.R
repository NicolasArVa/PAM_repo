library(tidyverse)
library(readxl)

{
  cn <- c("time", paste0("od", 1:12), "-", "time2", paste0("flu", 1:12))
  con <- c("Ace", "Ace caa", "Gly" , "Glu", "Gly caa" , "Glu caa")

  
  # Assuming the file is in current working directory
  for(i in 1:length(con)){
    tab <- read_excel("MG1655DE3_Clim.xlsx", sheet = i) 
    tab <- tab %>%  set_names(cn)
    t <- tab %>% select(time)
    od <- tab %>% select(od1:od12)
    od <- od - 0.1
    flu <- tab %>% select(flu1:flu12)
    phi <- flu/od
    
    diff_od <- apply(od, 2, diff)
    diff_flu <- apply(flu, 2, diff)
    diff_t <- apply(t, 2, diff)
    
    gr <- diff_od/(od*diff_t)
    pr <- diff_flu/(od*diff_t)
    
    od_bl <- od %>% select(od1:od6)
    od <- od %>% select(od7:od12)
    flu_bl <- flu %>% select(flu1:flu6)
    flu <- flu %>% select(flu7:flu12)
    phi_bl <- phi %>% select(flu1:flu6)
    phi <- phi %>% select(flu7:flu12)
    
    gr_bl <- gr %>% as_tibble() %>% select(od1:od6)
    gr <- gr %>% as_tibble() %>% select(od7:od12)
    pr_bl <- pr %>% as_tibble() %>% select(od1:od6)
    pr <- pr %>% as_tibble() %>% select(od7:od12)
    
    base_bl <- bind_cols(t, od_bl, flu_bl, phi_bl, gr_bl, pr_bl) %>% 
      mutate(conc = con[i])
    bn_bl <- paste0("base_bl", i)
    assign(bn_bl, base_bl)
    
    base <- bind_cols(t, od, flu, phi, gr, pr) %>% 
      mutate(conc = con[i])
    bn <- paste0("base", i)
    assign(bn, base)
  }
  r <- 1:6
  names <- c("time", paste0("od", r), paste0("flu", r), paste0("phi", r), 
             paste0("gr", r), paste0("pr", r), "conc")
  tidy_base_bl <- bind_rows(base_bl1, base_bl2, base_bl3, base_bl4, 
                            base_bl5, base_bl6) %>%
    set_names(names)
  tidy_base <- bind_rows(base1, base2, base3, base4, base5, base6) %>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  conc <- tidy_base %>% select(conc)
  
  od <- tidy_base %>% select(od1:od6) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu6) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi6) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr6) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr6) %>% apply(1, mean)
  
  od_se <- tidy_base %>% select(od1:od6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  flu_se <- tidy_base %>% select(flu1:flu6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  phi_se <- tidy_base %>% select(phi1:phi6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  gr_se <- tidy_base %>% select(gr1:gr6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  pr_se <- tidy_base %>% select(pr1:pr6) %>% apply(1, sd) %>% divide_by(sqrt(6))

  tidy_DE3_Clim <- bind_cols(time, conc, od, flu, phi, gr, pr, 
                             od_se, flu_se, phi_se, gr_se, pr_se) %>% 
    set_names(c("time", "n_source", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate",
                "od_se", "fluorescence_se", 
                "phi_se", "growth_rate_se", "production_rate_se"))
  
  #-----------------------------------------------------------------------------------------------
  
  
  time <- tidy_base %>% select(time)
  conc <- tidy_base %>% select(conc)
  
  od <- tidy_base %>% select(od1:od6) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu6) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi6) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr6) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr6) %>% apply(1, mean)
  
  od_se <- tidy_base %>% select(od1:od6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  flu_se <- tidy_base %>% select(flu1:flu6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  phi_se <- tidy_base %>% select(phi1:phi6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  gr_se <- tidy_base %>% select(gr1:gr6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  pr_se <- tidy_base %>% select(pr1:pr6) %>% apply(1, sd) %>% divide_by(sqrt(6))
  
  DE3_Clim_tidy_bl <- bind_cols(time, conc, od, flu, phi, gr, pr, 
                                od_se, flu_se, phi_se, gr_se, pr_se) %>% 
    set_names(c("time", "n_source", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate",
                "od_se", "fluorescence_se", 
                "phi_se", "growth_rate_se", "production_rate_se"))
  
}
