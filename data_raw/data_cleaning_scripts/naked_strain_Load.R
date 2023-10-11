# Load tidy data frame
library(readxl)

xlsx_names <- c("MG1655data_naked", "MG1655data_lacI", "BLRdata_naked", 
                "BLRdata_lacI", "BL21data_naked", "BL21data_lacI")
strains <- c("MG1655", "MG1655 lacI", "BLR", "BLR lacI", 
             "BL21", "BL21 lacI")

for(j in 1:length(strains)){
  file <- paste0("data_raw/strains_without_reporter/",xlsx_names[j], ".xlsx")
  strain <- strains[j]
  strain2 <- str_replace(strain, " ", "_")
  
  {
    onames <- c("time", paste0("od" ,1:27))
    fnames <- c("time", paste0("flu" ,1:27))
    
    {
      tabod <- read_excel(file, sheet = 1) %>%  set_names(onames)
      tabflu <- read_excel(file, sheet = 2) %>%  set_names(fnames)
      
      t <- tabod %>% select(time) %>% as_tibble()
      od <- tabod %>% select(od1:od27) 
      od <- od - 0.1 
      od <- od %>% as_tibble()
      
      diff_od <- apply(od, 2, diff)
      diff_t <- apply(t, 2, diff)
      
      n <- nrow(diff_od)
      
      gr <- diff_od/(od[1:n,]*diff_t)
      
      od_bl <- od[1:n,] %>% as_tibble() %>% select(od1:od9)
      od_0h <- od[1:n,] %>% as_tibble() %>% select(od10:od18)
      od_2h <- od[1:n,] %>% as_tibble() %>% select(od19:od27)
      
      gr_bl <- gr %>% as_tibble() %>% select(od1:od9)
      gr_0h <- gr %>% as_tibble() %>% select(od10:od18)
      gr_2h <- gr %>% as_tibble() %>% select(od19:od27)
    }
  }
  induction <- c("bl","0h","2h")
  for(i in 1:length(induction)){
    y <- c("od", "gr")
    x <- paste0(y, "_", induction[i])
    
    z <- sapply(1:length(y), function(i){
      avg <- get(x[i]) %>% apply(1, mean)
      se <- get(x[i]) %>% apply(1, sd)
      tibble(avg,se)
    }) %>% as_tibble()
    w <- bind_cols(z[1,1], z[2,1], z[1,2], z[2,2])%>% 
      unnest(cols = c(V1...1, V1...2, V2...3, V2...4)) %>%
      set_names(c("od_avg", "od_se", "gr_avg", "gr_se")) %>% 
      mutate(strain = strain, 
             induction_time = induction[i], 
             time = t$time[1:n], .before=od_avg)
    assign(paste0(strain2,"_", induction[i], "_tidy"), w)
  }
  assign(paste0(strain2,"_bl", "_raw"), gr_bl)
  assign(paste0(strain2,"_0h", "_raw"), gr_0h)
  assign(paste0(strain2,"_2h", "_raw"), gr_2h)
}
