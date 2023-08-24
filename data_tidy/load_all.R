setwd("C:/Users/User/projects/biosensors_paper/data_tidy")

files <- list.files(pattern = "\\.csv$")

for(file in files){
  name <- str_replace(file, ".csv", "")
  assign(name, read.csv(file=file))
}
