Hline <- function(x){
  names(x)[2] <- "n"
  na.omit(x) %>% group_by(n) %>%
    mutate(growth_rate = loess(growth_rate~time, span = 0.2)$fitted,
           phi = loess(phi~time, span = 0.2)$fitted) %>%
    filter(time > 0.75) %>%
    reframe(GR = max(growth_rate), phi = phi[which.max(growth_rate)])
}

Hline(tidy_DE3_Rlim)

