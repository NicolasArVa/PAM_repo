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

Figure_2 <- tab_1 %>% ggplot( aes( "", values, fill = Fraction, 
                     pattern = Fraction
                     )
                 )+
  geom_bar_pattern(pattern_fill = 'black',
                   width = 1, stat = 'identity',
                   position = 'fill', color = 'white'
           )+
  coord_polar( 'y', start = pi/2
              )+
  facet_wrap(~condition)+
  scale_fill_grey()+
  theme_void()+
  theme(
    strip.text.x = element_text(
      size = 15
      ),
    legend.box.margin = margin(t=1,r=10,b=1,l=1)
    )+
  scale_pattern_manual(values = c('\u03A6_R'= 'stripe',
                                  '\u03A6_C'= 'crosshatch',
                                  '\u03A6_Q'= 'circle',
                                  '\u03A6_H'= 'none'))

Figure_2

ggsave('Figure_2.tiff', Figure_2, width = 174, height = 58, units = 'mm', dpi = 600)
