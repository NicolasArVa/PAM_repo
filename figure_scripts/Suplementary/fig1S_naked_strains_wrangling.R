library(tidyverse)
library(readxl)


# function for standard error
se <- function(x){
  sd(x)/sqrt(length(x))
}

# Bind all strain dataframes into one data frame
{  
  nkd_str_tidy <- bind_rows(
  MG1655_bl_tidy, MG1655_0h_tidy, MG1655_2h_tidy,
  MG1655_lacI_bl_tidy, MG1655_lacI_0h_tidy, MG1655_lacI_2h_tidy,
  BLR_bl_tidy, BLR_0h_tidy, BLR_2h_tidy,
  BLR_lacI_bl_tidy, BLR_lacI_0h_tidy, BLR_lacI_2h_tidy,
  BL21_bl_tidy, BL21_0h_tidy, BL21_2h_tidy,
  BL21_lacI_bl_tidy, BL21_lacI_0h_tidy, BL21_lacI_2h_tidy)
  ab <- nkd_str_tidy
  
# Data cleaning 
  v <- c(8 + c(0:17)*63, 9 + c(0:17)*63) %>% sort() # indexes for outliers
  ab$gr_avg[v] <- NA                                #replace outliers with NA
  
  ab_filter <- ab %>% filter(induction_time %in% c("bl", "0h"), time <= 5) %>%
    mutate(induction = ifelse(induction_time == 'bl', '-', '+')) %>% 
    select(-induction_time) %>%
    na.omit()
}

# Table with errors
max_error <- ab_filter %>%
  mutate(strain=str_replace(strain, " lacI", "~lacI^OV")) %>%
  group_by(strain, induction) %>%
  summarise(max = max(gr_avg), time = time[which.max(gr_avg)],
            upper = max(gr_avg) + qnorm(0.975)*gr_se[which.max(gr_avg)],
            lower = max(gr_avg) - qnorm(0.975)*gr_se[which.max(gr_avg)])

labels<-data.frame(x = 0,y = 1.5, label = rev(LETTERS[1:6]),strain=factor(c("MG1655~lacI^OV","MG1655","BLR~lacI^OV","BLR","BL21~lacI^OV", "BL21")))

# Plot
base_strains <-ab_filter %>% rename(IPTG=induction)%>%
  mutate(strain=str_replace(strain, " lacI", "~lacI^OV"))%>%
  ggplot(aes(time, gr_avg, color = IPTG))+
  theme_classic()+
  geom_point(aes(shape = IPTG))+
  geom_errorbar(data = max_error, 
                aes(time, max,
                    ymin = lower, ymax = upper,
                    color = induction))+
  geom_line()+
  geom_text(data=labels,aes(x = x,y = y, label = label), color = "black")+
  scale_x_continuous(name = 'Time (h)')+
  scale_y_continuous(name = expression(Growth~rate~(h^1)), limit = c(0,1.5))+
  facet_wrap(~strain, labeller = label_parsed, ncol = 2)+
  theme(strip.background = element_rect(color = 'white'))

ggsave("Figure1S.tiff", base_strains, width = 174, height = 180, units = "mm", dpi = 600)
