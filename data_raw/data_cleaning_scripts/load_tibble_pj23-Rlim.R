{
  cn <- c("time", paste0("od", 1:6), "-", "-", "-", "time2", paste0("flu", 1:6))
  con <- c(0,2,4,8,12)
  
  
  
  # Assuming the file is in current working directory
  for(i in 1:length(con)){
    tab <- read_excel("MG1655pj23_Rlim.xlsx", sheet = i) 
    tab <- tab %>%  set_names(cn)
    t <- tab %>% select(time)
    od <- tab %>% select(od1:od6) %>% mutate_all(~.0.1)
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
  
  tidy_base <- bind_rows(base1, base2, base3, base4, base5) %>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  od <- tidy_base %>% select(od1:od3) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu3) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi3) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr3) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr3) %>% apply(1, mean)
  conc <- tidy_base %>% select(conc)
  
  tidy_j23_Rlim <- bind_cols(time, conc, od, flu, phi, gr, pr) %>% 
    set_names(c("time", "Chloramphenichol", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate"))
  
  #-----------------------------------------------------------------------------------------------
  tidy_base_bl <- bind_rows(base_bl1, base_bl2, base_bl3, base_bl4, 
                            base_bl5) %>%
    set_names(names)
  
  time <- tidy_base %>% select(time)
  od <- tidy_base %>% select(od1:od3) %>% apply(1, mean)
  flu <- tidy_base %>% select(flu1:flu3) %>% apply(1, mean)
  phi <- tidy_base %>% select(phi1:phi3) %>% apply(1, mean)
  gr <- tidy_base %>% select(gr1:gr3) %>% apply(1, mean)
  pr <- tidy_base %>% select(pr1:pr3) %>% apply(1, mean)
  conc <- tidy_base %>% select(conc)
  
  j23_Rlim_tidy_bl <- bind_cols(time, conc, od, flu, phi, gr, pr) %>% 
    set_names(c("time", "Chloramphenichol", "od", "fluorescence", 
                "phi", "growth_rate", "production_rate"))
  
}
