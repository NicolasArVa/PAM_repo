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
  test <- read_xlsx(file)
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
  
  blank <- test6 %>%
    filter(cumate %in% c("sin nada", "MCG")) %>% 
    arrange(cumate, time)
  
  test7 <- test6 %>%
    filter(!cumate %in% c("sin nada", "MCG")) %>% 
    mutate(cumate = str_replace_all(cumate, "1mM", "1000"),
           cumate = str_replace_all(cumate, "\\D", ""),
           cumate = as.numeric(cumate)) %>%
    arrange(cumate, time)
  
  dir <- str_replace(file, "\\.xlsx?$", "")
  write.csv(blank, file = paste0(dir, "-blank.csv"))
  write.csv(test7, file = paste0(dir, "-data.csv"))
  
  return(list(blank = blank, data = test7))
}
dir_preprocessing <- function(dirName){
  files <- list.files(dirName, pattern = "^pUC-cymR-.*-gfp.*")
  paths <- paste0(dirName,"/", files) %>% sort

  list <- lapply(paths, pre_processing_dahlin)
  names(list) <- str_extract(paths, ".*pUC-cymR-.*-gfp\\s(.*)\\.xlsx", group = 1)
  assign(dirName, list)
}

allLists <- lapply(dirs, dir_preprocessing)
names(allLists) <- str_extract(dirs, "^\\./(.*)", group = 1) 

names(allLists)
allLists %>% View

allLists[[4]] %>% names
allLists$`dahlin_dir7-highCellDensity-inducer`$`BL21_01-expanded`$data
