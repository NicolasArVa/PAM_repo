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
max_error <- ab_filter %>% group_by(strain, induction) %>%
  summarise(max = max(gr_avg), time = time[which.max(gr_avg)],
            upper = max(gr_avg) + qnorm(0.975)*gr_se[which.max(gr_avg)],
            lower = max(gr_avg) - qnorm(0.975)*gr_se[which.max(gr_avg)])
#labels
old_labels <- sort(unique(ab_filter$strain))
new_labels <- paste0(LETTERS[1:6],". ", sort(unique(ab_filter$strain)))

test <- ab_filter
ab_filter %>% count(strain)



# Plot
base_strains <- ab_filter %>% 
  ggplot(aes(time, gr_avg, color = induction))+
  theme_classic()+
  geom_point(aes(shape = induction))+
  geom_errorbar(data = max_error, 
                aes(time, max,
                    ymin = lower, ymax = upper,
                    color = induction))+
  geom_line()+
  geom_text(data = labels, aes(x = 0,y = 1.5, label = LETTERS[1:6]), color = 'black')+
  scale_x_continuous(name = 'Time (h)')+
  scale_y_continuous(name = expression(Growth~rate~(h^1)), limit = c(0,1.5))+
  facet_wrap(~strain, ncol = 2)+
  theme(strip.background = element_rect(color = 'white'))

ggsave("Figure1S.tiff", base_strains, width = 174, height = 180, units = "mm", dpi = 600)
