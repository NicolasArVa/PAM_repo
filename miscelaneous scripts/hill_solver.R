library("tidyverse")

tab	<- tibble(strain = c("MG1655","MG1655 lacI",
                          "BLR","BLR lacI","BL21 lacI"),
                 H=c(2029, 1458, 5915, 11499, 9757),
                 k=c(54.9, 164, 69, 49.2, 37.2),
                 n=c(4.27, 1.9, 6.75, 2.3, 2.19))

hill_func <- function(x, s){
  tab <- tab %>% filter(strain == s)
  H <- tab$H
  k <- tab$k
  n <- tab$n
  H*(x^n)/((k^n)+(x^n))
}

strs = c("MG1655","MG1655 lacI", "BLR","BLR lacI","BL21 lacI")
c <- c(1,62,100,250,1000)

strains_fi <- bind_rows(lapply(strs, function(s){
  tibble(
    strain = s,
    iptg = c,
    fi = sapply(c, function(x){
      tab <- tab %>% filter(strain == s)
      H <- tab$H
      k <- tab$k
      n <- tab$n
      H*(x^n)/((k^n)+(x^n))
    })
    )
}))

v <- 1:nrow(strains_fi)
fi_predicted <- bind_rows(lapply(v, function(i){
  s <- strains_fi[i,]$strain
  c <- strains_fi[i,]$iptg
  m <- strains_fi[i,]$fi
  
  tibble(strain = s,iptg = c,
         x = seq(0,1.2, 0.1), y = m*x)
})) %>% mutate(iptg = as.factor(iptg))
