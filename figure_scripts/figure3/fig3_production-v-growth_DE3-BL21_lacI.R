library(tidyverse)
library(ggpubr)
library(ggrepel)
library(expandFunctions)

bl <- tidy_DE3_Hlim %>%
  filter(strain == "BL21 lacI", iptg == min(iptg)) %>% 
  select(time, phi) %>% 
  rename(bl = phi) %>% 
  ungroup()

tidy_BL21 <- tidy_DE3_Hlim %>% filter(strain == "BL21 lacI")%>% 
  left_join(bl, by = c("time")) %>%
  filter(iptg %in% c(1,62,100,250,1000)) %>% 
  mutate(phi = phi - bl)%>%
  mutate(iptg = as.numeric(iptg))%>%
  arrange(iptg)%>%
  mutate(iptg = as.factor(iptg)) 

indexes <- tidy_BL21 %>% filter(between(time, 0, 10)) %>% 
  group_by(iptg) %>%
  summarize(phi_index=which.max(phi),
            gr_index=which.max(growth_rate))
{
  # tables to circle analysis points by us and by Hwa
  a <- c(1,62,100,250,1000)
  b <- indexes$gr_index
  c <- indexes$phi_index
  # Hwa table
  tab_Hwa <- sapply(2:length(a),function(i){
    tab <- tidy_BL21 %>% filter(iptg==a[i]) %>%
      select(production_rate, growth_rate) %>% .[b[i],]
    tab <- bind_cols(tab, a[i])
  }) %>% t() %>% as_tibble %>% 
    unnest(cols = c(`...3`, production_rate, growth_rate)) %>% 
    rename(iptg = `...3`) %>% relocate(iptg)
  # our table
  tab_Vacc<- sapply(2:length(a),function(i){
    tab <- tidy_BL21 %>% filter(iptg==a[i]) %>%
      select(production_rate, growth_rate) %>% .[c[i],]
    tab <- bind_cols(tab, a[i])
  }) %>% t() %>% as_tibble %>% 
    unnest(cols = c(`...3`, production_rate, growth_rate)) %>% 
    rename(iptg = `...3`) %>% relocate(iptg) 

  # Table to mark key time points with labels 
  a <- c(0,62,100,250,1000)
  b <- c(1,4,13)
  
  tab_times <- sapply(2:length(a),function(i){
    tab <- tidy_BL21 %>% filter(iptg==a[i]) %>%
      select(production_rate, growth_rate) %>% .[b,]
    tab <- bind_cols(a[i], tab, c('0 h', '0.75 h', '3 h'))
  }) %>% t() %>% as_tibble %>% 
    unnest(cols = c(`...1`, production_rate, growth_rate, `...4`)) %>% 
    rename(iptg = `...1`, time = `...4`)

}

PvG_BL21 <- tidy_BL21 %>% filter(time < 11)%>%
  ggplot(aes(growth_rate, production_rate/1000, size = iptg, color = iptg))+
  theme_classic()+
  geom_path(show.legend = F)+
  geom_label_repel(data = tab_times, aes(growth_rate, production_rate/1000, 
                                   label = time, color = as.factor(iptg)), 
             size = 2, show.legend = F)+
  geom_point(data = tab_Vacc, aes(growth_rate, production_rate/1000), 
             color = 'green', size = 5, shape=22)+
  geom_point(data = tab_Hwa, aes(growth_rate, production_rate/1000), 
               color = 'red3', size = 5, shape = 21)+
  scale_color_manual(values = c("1000" = "black", "250"="grey30",
                                "100"="grey50","62"= "grey70", 
                                "0"= "grey80"
                                )
                     )+
  scale_size_manual(values = c("1000" = unit(1.3, "mm"), "250"=unit(1, "mm"),
                               "100"=unit(0.7, "mm"),"62"= unit(0.4, "mm"), 
                               "0"= unit(0.1, "mm")
                               )
                    )+
  geom_curve(x = 0.8, y = 0.5, xend = 0.7, yend = 4,
             linetype = 1, curvature = 0.1, color = "red", 
             size = unit(0.3, "mm"),
             arrow = arrow(length = unit(0.03, "npc"),
                           type="closed"))+
  geom_text(aes(x = c(0.8, 0.7), y = c(0.7, 4), label = c("-", "+")), 
            data = tibble(), size = 3,
            nudge_x = c(-0.04, -0.04), nudge_y = c(0, -0.1), 
            color = "black")+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        plot.title = element_text(size = unit(8, 'mm'), hjust = 0.5),
        axis.title.x = element_text(size = unit(8, "mm")),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        aspect.ratio=3/3)+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3~FU~OD[600]^-1~h^-1)))+
  xlim(c(0,1.25))+
  ylim(c(0,8))

PvG_BL21



