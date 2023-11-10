library(tidyverse)
strs <- factor(c("MG1655", "MG1655 lacI", "BLR", "BLR lacI", "BL21 lacI"))
c <- c(1, 62, 100, 250, 1000)

# data frame for hill function parameters
tab <- tibble(
  strain = strs,
  H = c(2029, 1458, 5915, 11499, 9757),
  k = c(54.9, 164, 69, 49.2, 37.2),
  n = c(4.27, 1.9, 6.75, 2.3, 2.19)
)

# Calculation of fi from hill functions (fig 3 script):
predicted_fi <- DE3predictions_table %>% 
  rename(fi_lower = lower, fi_upper = upper)%>%
  mutate(iptg = factor(iptg)) %>%
  filter(iptg %in% c(1,62,100,250,1000))

# Calculation of fi from slopes of the hook graphs:
calculated_fi <- data %>% 
  filter(iptg %in% c(1,62,100,250,1000), time > 2, time <6) %>% 
  group_by(strain, iptg) %>%
  summarize(m = coef(lm(production_rate~growth_rate))["growth_rate"],
            m_lower = confint(lm(production_rate~growth_rate))[2,1],
            m_upper = confint(lm(production_rate~growth_rate))[2,2],
            b = coef(lm(production_rate~growth_rate))["(Intercept)"],
            b_lower = confint(lm(production_rate~growth_rate))[1,1],
            b_upper = confint(lm(production_rate~growth_rate))[1,2])

# Combination of both in one graph:
fi_comparison_table <- calculated_fi %>% left_join(predicted_fi, by = c("iptg", "strain")) %>%
  mutate(m = round(m/1000, 2), m_lower = round(m_lower/1000, 2), m_upper = round(m_upper/1000, 2),
         b = round(b/1000, 2), b_lower = round(b_lower/1000, 2), b_upper = round(b_upper/1000, 2), 
         fi = round(fi/1000, 2), fi_lower = round(fi_lower/1000, 2), fi_upper = round(fi_upper/1000, 2))


pr_prediction <- expand.grid(strain = strs, iptg = factor(c), 
                             gr = seq(0, 0.6, 0.1)) %>% 
  left_join(fi_comparison_table, by = c("iptg", "strain")) %>% 
  select(strain, iptg, gr, fi) %>% arrange(strain, iptg) %>%
  mutate(pr = fi*gr)

###PLOT CON TODAS LAS RECTAS
slope_grid <- tidy_DE3_Hlim %>%
  mutate(iptg=factor(iptg))%>%
  filter(iptg %in% c(1,62,100,250,1000), time > 2, time <6) %>% 
  ggplot(aes(growth_rate, production_rate/1000))+
  theme_classic()+
  geom_smooth(method = "lm", linewidth=0.5,linetype="dotted", color = "black")+
  geom_point(shape=1,show.legend = F)+
  geom_line(data = pr_prediction, aes(gr, pr))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))+
  xlab(expression(Growth~rate~(h^-1)))+
  #scale_shape_manual(values = c("MG1655" = 1, "BLR" = 0, 
     #                           "MG1655 lacI" = 13, "BLR lacI" = 7, 
    #                            "BL21 lacI" = 2), 
       #              name = "Strain")+
  facet_grid(iptg~strain, labeller = eval(parse_exprs(c("BL21+lacI^OV", "BLR", "BLR+lacI^OV", "MG1655", "MG1655+lacI^OV"))))+
  theme(plot.margin = margin(2,2,2,2, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        strip.text = element_text(size = unit(6, "mm")),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(aspect.ratio=1/1)
slope_grid 

calculated_fi%>%
  ggplot(aes(as.numeric(as.character(iptg)),m/1000, group = strain, color = strain))+
  theme_classic()+
  geom_line()+
  geom_point()

  
fi_comparison_table %>% ggplot(aes(shape = strain))+
  theme_classic()+
  geom_point(aes(iptg, m), color = "red")+
  geom_errorbar(aes(x = iptg, y = m, ymin = m_lower, ymax = m_upper), color = "red")+
  geom_point(aes(iptg, fi), color = "blue")+
  geom_errorbar(aes(x = iptg, y = fi, ymin = fi_lower, ymax = fi_upper), color = "blue")+
  facet_wrap(~strain, scales = "free_y")
