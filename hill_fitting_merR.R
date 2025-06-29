library(tidyverse)
{
hill <- function(x, H, k, n){
  H*(x^n)/((x^n)+(k^n) + 1e-5) # added a small constant to avoid zeros on the denominator
}
# Blanking
{
hg_cases <- tidy_Hg_Hlim %>%
  filter(between(time, 0, 14)) %>%
  split(list(.$strain, .$rep), drop = T)
hg_cases_blanked <- lapply(hg_cases, function(x){
  blank <- x %>% 
    filter(inductor == 0) %>%
    .$pr
  
  x %>%
    group_by(inductor) %>%
    mutate(pr = pr - blank)%>%
    ungroup()
}) %>% 
  bind_rows()

# filter using the  time where the mean growth rate is statistically zero (alpha = 0.05), identifying the exponential phase
points <- hg_cases_blanked %>% 
  split(list(.$strain, .$inductor, .$time), drop = T)
time_selection <- lapply(points, function(x){ 
  data <- x$gr
  test <- t.test(data, mu = 0)
  tibble(time = x$time[1], strain = x$strain[1], inductor = x$inductor[1],
         `gr = 0` = test$p.value >= 0.05)
}) %>% 
  bind_rows() %>% 
  arrange(strain, inductor, time) %>%
  split(list(.$strain, .$inductor), drop = T)
t_table <- lapply(time_selection, function(x){
  x[x$`gr = 0`,] %>%
    filter(time > 2) %>%
    summarise(strain = strain[1], inductor = inductor[1], t = min(time))
}) %>% bind_rows %>% arrange(strain, inductor)

tidy_hg_exp <- hg_cases_blanked %>% left_join(t_table) %>%
  group_by(strain, inductor) %>%
  filter(time < t) %>% 
  ungroup

# Linear regression: production rate vs growth rate
tidy_hg_exp_list <- tidy_hg_exp %>%
  split(list(.$strain, .$inductor), drop = T)

slope_table <- lapply(tidy_hg_exp_list, function(x){
  model <- lm(pr~gr, data = x)
  tab <- summary(model) %>% coef()
  tibble(inductor = x$inductor[1], strain = x$strain[1], 
         m = tab[2,1], m_err = tab[2,2],
         b = tab[1,1], b_err = tab[1,2])
}) %>% 
  bind_rows() %>% 
  arrange(strain, inductor)

# Hill function fitting
points <- slope_table #%>% filter(inductor != 2000)

  model <- nls(m~hill(inductor, H, k, n),
               data = points,
               start = list(H = 1000, k = 10, n = 1),
               algorithm = "port", lower = c(0,0,0),
               weights = 1/((points$m_err)^2 + 1e-4))
  cons <- summary(model) %>% coef()
  
  m_hat <- predict(model, tibble(inductor = 0:2000))
  tab <- tibble(strain = points$strain[1], 
                m_hat = m_hat, inductor = 0:2000)
  results <- list(constants = cons, prediction = tab)


}
# Extract results from the regression
slope_constants <- results$constants
slope_constants
slope_curves <- results$prediction

# Plot the results
merR_plot <- slope_table %>% 
  ggplot(aes(inductor/1000, m/1000))+
  theme_classic()+
  geom_point()+
  geom_errorbar(aes(ymin = (m - m_err)/1000,
                    ymax = (m + m_err)/1000))+
  geom_line(data = slope_curves, aes(inductor/1000, m_hat/1000))+
  ylim(c(0,15))+
  xlab(expression(Hg^"2+"~concentration~(mu*M)))+
  ylab(expression(Slope~(10^3~FU~OD[600]^-1)))+
  theme(plot.margin = margin(2,2,2,2, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))+
  theme(aspect.ratio=1/1)
merR_plot



slope_grid <- tidy_hg_exp %>%
  mutate(inductor = str_replace(inductor, "$", "~n*M"),
         inductor = factor(inductor, 
                           levels = c("0~n*M", "1~n*M", "2~n*M", "5~n*M",
                                      "10~n*M", "25~n*M", "50~n*M", "125~n*M",
                                      "250~n*M", "500~n*M", "1000~n*M", "2000~n*M"))) %>%
  ggplot(aes(gr, pr))+
  theme_classic()+
  geom_point(shape = 20, size = 1)+
  geom_smooth(method = "lm", color = "red", fullrange = T,
              linetype = "dashed", linewidth = 0.8)+
  facet_wrap(~inductor, labeller=label_parsed, ncol = 6)+
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))+
  xlab(expression(Growth~rate~(h^-1)))+
  theme(plot.margin = margin(2,2,2,2, "mm"),
        strip.text = element_text(size = unit(9, "mm")),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(aspect.ratio=1.1/1)

slope_grid
}
figure5_part1 <- ggarrange(slope_grid, merR_plot, labels = c("A", "B"), widths = c(6,3))
figure5 <- ggarrange(figure5_part1, figure5_part2, nrow = 2, heights = c(4.5,6))
figure5

ggsave("Figure5.tiff", figure5, width = 174, height = 140, units = "mm", dpi = 1200)
ggsave("Figure5.png", figure5, width = 174, height = 140, units = "mm", dpi = 1200)

