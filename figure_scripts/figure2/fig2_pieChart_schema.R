install.packages("remotes")    # Install remotes package
remotes::install_github("coolbutuseless/ggpattern")
library("ggpattern")     # Load ggpattern package

library(tidyverse)

unicode_phi <- c('\u03A6[R]', '\u03A6[C]', '\u03A6[Q]', '\u03A6[H]')

fraction_names <- factor(unicode_phi,
                         unicode_phi)

{tab_1A <- tibble(condition = 'A. Rich medium', 
                  values = c(30,20,50,0),
                  Fraction = fraction_names)
  
  tab_1B <- tibble(condition = 'B. Poor medium', 
                   values = c(10,40,50,0),
                   Fraction = fraction_names)
  
  tab_1C <- tibble(condition = 'C. Induction', 
                   values = c(30,20,50,10),
                   Fraction = fraction_names)
  
  
  tab_1 <- bind_rows(tab_1C, tab_1B, tab_1A) %>%
    mutate(condition = factor(condition))
}

Figure_2 <- tab_1 %>% 
  ggplot(aes( "", values, fill = Fraction))+
  geom_bar(stat = "identity", width = 1, position = 'fill') +
  coord_polar('y', start = pi/2)+
  facet_wrap(~condition)+
  scale_fill_grey()+
  scale_fill_manual(values = c('\u03A6[R]'= rgb(109/255, 204/255, 220/255),
                               '\u03A6[C]'= rgb(185/255, 81/255, 159/255),
                               '\u03A6[Q]'= rgb(242/255, 235/255, 23/255),
                               '\u03A6[H]'= 'green'),
                    labels= eval(parse_exprs(
                      c('\u03A6[R]', '\u03A6[C]', '\u03A6[Q]', '\u03A6[H]')))
                    )+
  theme_void()+
  theme(strip.text.x = element_text(size = 15), 
        legend.box.margin = margin(t=1,r=10,b=1,l=1))


Figure_2
ggsave('Figure2.tiff', Figure_2, width = 174, height = 58, units = 'mm', dpi = 1200)
ggsave('Figure2.png', Figure_2, width = 174, height = 58, units = 'mm', dpi = 1200)
