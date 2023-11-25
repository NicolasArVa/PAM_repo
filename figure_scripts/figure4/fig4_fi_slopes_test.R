library(tidyverse)
library(rlang)
library(stringr)

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
  filter(iptg %in% c(1,62,100,250,1000))%>%
  mutate(strain=str_replace(strain,"lacI", "+lacI^OV"),
         iptg=factor(str_replace(iptg, "$", "~mu*M"),
                     c("1~mu*M","62~mu*M","100~mu*M","250~mu*M","1000~mu*M")))


slope_grid <- tidy_DE3_Hlim %>%
  filter(iptg %in% c(1,62,100,250,1000), time > 2, time <6) %>%
  mutate(strain=str_replace(strain,"lacI", "+lacI^OV"),
         iptg=factor(str_replace(iptg, "$", "~mu*M"),
                     c("1~mu*M","62~mu*M","100~mu*M","250~mu*M","1000~mu*M")))%>%
  ggplot(aes(growth_rate, production_rate/1000))+
  theme_classic()+
  geom_smooth(method = "lm", linewidth=0.5,linetype="dotted", color = "black")+
  geom_point(shape=1,show.legend = F)+
  geom_abline(data = predicted_fi, aes(slope= fi/1000, intercept=0))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))+
  xlab(expression(Growth~rate~(h^-1)))+
  facet_grid(iptg~strain, labeller=label_parsed)+
  theme(plot.margin = margin(2,2,2,2, "mm"),
        strip.text = element_text(size = unit(9, "mm")),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())+
  theme(aspect.ratio=1/1)
slope_grid

ggsave("Figure4.tiff", slope_grid, width = 174, height = 174, units = "mm", dpi = 600)




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
