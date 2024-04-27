merR_tab_slopes <- tidy_Hg_Hlim_test %>%
  filter(time < 5, Hg < 2000) %>%
  group_by(Hg) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(production_rate ~ growth_rate, data = .x)),
         tidied = map(model, tidy)) %>%
  unnest(tidied) %>%
  filter(term == "growth_rate") %>%
  select(Hg, estimate) %>%
  rename(phi = estimate)

model <- nls(phi~a*((Hg^n) /((Hg^n) + (b^n))), 
             data = merR_tab_slopes,
             start = list(a=2000, b=100, n = 1), 
             algorithm = 'port', lower=c(0, 0, 1))



1-sum(residuals(model)^2)/sum((merR_tab_slopes$phi-mean(merR_tab_slopes$phi))^2)


hg <- 0:1000
fi_hat <- predict(model, newdata = tibble(Hg = hg))

# Calculate standard errors for predictions using DELTA METHOD:
a <- coef(model)["a"]
b <- coef(model)["b"]
k <- coef(model)["n"]
parameter_standard_errors <- sqrt(diag(vcov(model)))

prediction_error <- sqrt(
  ((hg)^k / ((hg)^k + b^k))^2 * parameter_standard_errors["a"]^2 +
    (a * k * b^(k-1) * (hg)^k / ((hg)^k + b^k)^2 )^2 * parameter_standard_errors["b"]^2 +
    (a * b^k * log(hg / b) * (hg)^k / ((hg)^k + b^k)^2 )^2 * parameter_standard_errors["n"]^2
)

tab_merR <- tibble(Hg = hg, fi = fi_hat,
                   # Calculate lower and upper bounds for the confidence intervals
                   lower = fi_hat - qnorm(0.975) * prediction_error, 
                   upper = fi_hat + qnorm(0.975) * prediction_error,
                   error = prediction_error)

#----------- Heterologous fraction as a hyperbolic function ---------------------------------- 
#__wrangling__
#merR_plot <- tidy_Hg_Hlim %>% 
#filter(time > 0.5) %>% 
#left_join(blc, by="time")%>%
#mutate(phi = phi - bl) %>%
#group_by(Hg) %>% 
#summarize(phi = max(phi, na.rm = TRUE)) %>%
#ungroup()%>%
merR_plot <- merR_tab_slopes %>%
  # __plot__  
  ggplot(aes(Hg, phi/1000))+
  theme_classic()+
  geom_point(shape = 5, size = 3)+
  geom_line(data = tab_merR, aes(Hg, fi/1000), size = unit(0.3, "mm"))+
  xlab(expression(Hg~concentration~(nM)))+
  xlim(c(0,1000))+
  ylab("")+
  #ylim(c(0,10))+
  theme(plot.margin = margin(0,0,0,0, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 1, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        aspect.ratio=2/3)

merR_plot
summary(model)
