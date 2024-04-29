library(readxl)
library(tidyverse)
library(stringr)
library(utils)

dirs <- list.dirs() %>% .[grep(., pattern = "^\\./dahlin_dir\\d.*")]
#n <- 1
#files <- list.files(dirs[n], pattern = "^pUC-cymR-.*-gfp.*")
#paths <- paste0(dirs[n],"/", files) %>% sort

pre_processing_dahlin <- function(file){
  #file <- paths[5]
  test <- read_xlsx(file, .name_repair = "minimal")
  n <- (nrow(test) - 65)/6
  
  test2 <- test[c(5:(5 + n), (n*4 + 6):(n*5 + 5)),]
  times <- bind_rows(test2[2:(n + 1) ,1],test2[2:(n + 1) ,1]) %>% .$OD600
  
  index <- is.na(test2) %>% apply(2, all)
  test3 <- test2[,!index]
  
  headers <- test3[1,] %>% unname %>% unlist
  headers[1] <- "time"
  
  test4 <- test3[-1,]
  names(test4) <- headers
  times <- times %>% str_remove("\\s?s") %>% as.numeric()
  test4[,1] <- times/3600
  
  levels <- unique(names(test4)) %>% .[2:length(.)]
  
  test5 <- test4 %>%
    pivot_longer(any_of(levels), names_to = c("cumate"))
  
  mold <- test5$cumate
  groups <- mold[1:(length(mold)/2)] %>% rle %>% .$lengths %>% sequence()
  
  test5$cumate %>% unique()
  
  test6 <- test5 %>%
    mutate(value = as.numeric(value), 
           measurement = c(rep("OD", nrow(.)/2), rep("flu", nrow(.)/2)),
           group = rep(groups, 2)) %>% 
    pivot_wider(names_from = c(measurement, group),
                values_from = value,
                names_sep = "_")
  
  test7 <- test6 %>%
    mutate(cumate = str_replace_all(cumate, "1\\s?-?mM", "1000"),
           cumate = str_replace_all(cumate, "(?<=\\d{1,4})\\D*", ""))
  
  strain <- str_extract(file, pattern = ".*gfp\\s([A-Za-z\\d]*)(-extended)?.*", group = 1)
  test8 <- test7 %>% 
    mutate(strain = rep(strain, nrow(.)), .after = cumate)
  
  return(test8)
}
processing <- function(x){
  # x <- list[[1]]
  data <- x %>% arrange(cumate, time)
  headers <- c("optical_density", "fluorescence", "phi", 
               "growth_rate", "production_rate", "phi_rate")
  
  t <- data %>% select(time)
  condition <- data %>% select(cumate)
  strain <- data %>% select(strain)
  optical_density <- data %>% select(matches("OD_\\d*")) %>% mutate_all(~.-0.08)
  fluorescence <- data %>% select(matches("flu_\\d*"))
  phi <- fluorescence/optical_density
  
  diff_od <- apply(optical_density, 2, diff)
  diff_flu <- apply(fluorescence, 2, diff)
  diff_phi <- apply(phi, 2, diff)
  diff_t <- apply(t, 2, diff) %>% ifelse(. <= 0, mean(.), .)
  
  growth_rate <- diff_od/(optical_density[1:nrow(diff_od),]*diff_t)
  production_rate <- diff_flu/(optical_density[1:nrow(diff_flu),]*diff_t)
  phi_rate <- apply(diff_phi, 2, function(x) x/diff_t)
  
  for(j in seq_along(headers)){
    x <- apply(get(headers[j]), 1, function(x) mean(x, na.rm = T))
    n <- paste0(headers[j], "_mean")
    assign(n,x)
    
    y <- apply(get(headers[j]), 1, function(x) sd(x, na.rm = T)/sqrt(sum(!is.na(x))))
    m <- paste0(headers[j], "_se")
    assign(m,y)
  }
  
  data_list <- mget(c(paste0(a, "_mean"), paste0(a, "_se")))
  # Find the maximum length of vectors
  max_length <- max(sapply(data_list, length))
  # Pad with NA's the gaps
  frame <- sapply(data_list, function(x) {
    length_diff <- max_length - length(x)
    c(x, rep(NA, length_diff))
  })
  
  finalTable <- bind_cols(time = t, cumate = condition, strain = strain, frame) %>%
    as_tibble() %>%
    arrange(cumate, time)
  
  return(finalTable)
}
dir_processing <- function(dir, subdir = "xlsx"){
  #dir <- dirs[1]
  #subdir <- "xlsx"
  dirName <- paste0(dir, "/", subdir)
  files <- list.files(dirName, pattern = "^pUC-cymR-.*-gfp.*")
  paths <- paste0(dirName,"/", files) %>% sort

  list <- lapply(paths, pre_processing_dahlin)
  processedList <- lapply(list, processing)
  table <- bind_rows(processedList)
  return(table)
}

dahlin_essays <- lapply(dirs, dir_processing)
names(dahlin_essays) <- str_extract(dirs, pattern = ".*_dir\\d+-(.*)", group = 1)

dahlin_essays_extended <- lapply(c(dirs[1], dirs[4]), 
                        function(x){dir_processing(x, subdir = "xlsx-extended")})
names(dahlin_essays_extended) <- str_extract(c(dirs[1], dirs[4]), 
                                             pattern = ".*_dir\\d+-(.*)", 
                                             group = 1)


test <- paste0(dirs[3], "/xlsx/pUC-cymR-J23-gfp BLR.xlsx")
pre_processing_dahlin(test) %>% processing

dirs[3]
