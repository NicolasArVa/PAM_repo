library("tidyverse")

tab	<- tibble(strains = c("MG1655","MG1655 lacI",
                          "BLR","BLR lacI","BL21 lacI"),
                 H=c(2029, 1458, 5915, 11499, 9757),
                 k=c(54.9, 164, 69, 49.2, 37.2),
                 n=c(4.27, 1.9, 6.75, 2.3, 2.19))

hill_func <- function(x, s){
  tab <- tab %>% filter(strain= s)
  H <- tab$H
  k <- tab$k
  n <- tab$n
  H*(x^n)/((k^n)+(x^n))
}

hill_func(1000, tab_BL21_lacI)/1000
map_df(tibble(x=c(0,62,100,250,1000)), function(x){
  hill_func(x, tab_BL21_lacI)/1000
})