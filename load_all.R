# DOWNLOAD DATA TABLES FROM FIGSHARE

# path to data downloaded from figshare 
target_dir <- paste0(getwd(), "/data_tidy") 

files <- list.files(target_dir, pattern = "\\.csv$")
names <- c("tidy_DE3_Hlim", "tidy_j23_Hlim", "tidy_Hg_Hlim")

for(i in seq_along(files)){
  file <- files[i]
  name <- names[i]
  assign(name, read.csv(file=file))
}
