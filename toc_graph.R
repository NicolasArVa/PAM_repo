install.packages("remotes")    # Install remotes package
remotes::install_github("coolbutuseless/ggpattern")
library("ggpattern")     # Load ggpattern package

library(tidyverse)

unicode_phi <- c('\u03A6_R', '\u03A6_C', '\u03A6_Q', '\u03A6_H')

fraction_names <- factor(unicode_phi,
                         unicode_phi)

{tab_1A <- tibble(condition = 'A. Rich medium', 
                  values = c(20,30,50,0),
                  Fraction = fraction_names)
  
  tab_1B <- tibble(condition = 'B. Poor medium', 
                   values = c(10,40,50,0),
                   Fraction = fraction_names)
  
  tab_1C <- tibble(condition = 'C. Induction', 
                   values = c(20,30,50,10),
                   Fraction = fraction_names)
  
  
  tab_1 <- bind_rows(tab_1C, tab_1B, tab_1A) %>%
    mutate(condition = factor(condition)
    )
}

TOC_fig <-tab_1C %>% 
  ggplot(aes( "", values, fill = Fraction, color = Fraction))+
  geom_bar(width = 1, stat = 'identity', position = 'fill', color = 'white', show.legend = F)+
  coord_polar('y', start = pi/2)+
  theme_void()+
  scale_fill_manual(values = c('\u03A6_R'= "#8B103E",
                                               '\u03A6_C'= '#065381',
                                               '\u03A6_Q'= '#FE912A',
                                               '\u03A6_H'= '#64D0DA'))
TOC_fig
ggsave('TOC_Figure.png', TOC_fig, width = 1.75, height = 1.75, units = 'in', dpi = 300)
