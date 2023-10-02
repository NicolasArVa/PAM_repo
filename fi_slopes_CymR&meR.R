library(tidyverse)

predicted_fi <- tab_CymR %>% filter(cumate %in% c(0,125,250,500,1000)) %>%
  select(cumate, fi, error)
GP <- expand.grid(fi = predicted_fi$fi, gr = seq(0,0.5, 0.1)) %>%
  mutate(pr = fi*gr)
GP_lines <- predicted_fi %>% left_join(GP, by = "fi") %>%
  mutate(lower = pr - qnorm(0.975)*error*gr,
         upper = pr + qnorm(0.975)*error*gr) %>% select(-error)

tidy_j23_Hlim %>% 
  filter(time < 3) %>%
  mutate(cumate = factor(cumate))%>%
ggplot()+
  geom_smooth(method = "lm",
              aes(growth_rate, production_rate/1000, color = cumate))+
  geom_point(aes(growth_rate, production_rate/1000, color = cumate))+
  theme_classic()+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  geom_line(data=GP_lines %>% mutate(cumate = factor(cumate)),
            aes(gr, pr/1000, color = cumate))+
  #geom_point(data=GP_lines %>% mutate(cumate = factor(cumate)),
            # aes(gr, pr/1000, color = cumate))+
  #geom_errorbar(data = GP_lines %>%
                 # mutate(cumate = factor(cumate)), 
                #aes(x=gr, y=pr/1000, ymin=lower/1000, ymax=upper/1000, 
    #                                 color = cumate))+
  facet_wrap(~cumate)+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))

################################# merR ########################################################

predicted_fi <- tab_merR %>% 
  filter(Hg %in% c(0, 10, 25, 50, 125, 250, 500, 1000, 2000)) %>% 
  select(Hg, fi, error)
GP <- expand.grid(fi = predicted_fi$fi, gr = seq(0,3, 0.1)) %>%
  mutate(pr = fi*gr)
GP_lines <- predicted_fi %>% left_join(GP, by = "fi") %>%
  mutate(lower = pr - qnorm(0.975)*error*gr,
         upper = pr + qnorm(0.975)*error*gr) %>% select(-error)

tidy_Hg_Hlim %>% 
  filter(between(time, 2, 8), !Hg %in% c(1,2,5)) %>%
  mutate(Hg = factor(Hg))%>%
 ggplot()+
  geom_smooth(method = "lm",aes(growth_rate, production_rate/1000, color = Hg))+
  geom_point(aes(growth_rate, production_rate/1000, color = Hg))+
  theme_classic()+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  geom_line(data=GP_lines %>% mutate(Hg = factor(Hg)),
            aes(gr, pr/1000, color = Hg))+
  #geom_point(data=GP_lines %>% mutate(Hg = factor(Hg)),aes(gr, pr/1000, color = Hg))+
  #geom_errorbar(data=GP_lines %>%mutate(Hg = factor(Hg)), aes(x = gr, pr/1000, ymin = lower/1000, ymax = upper/1000,color = Hg))+
  facet_wrap(~Hg)+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(Production~rate~(10^3*FU~OD[600]^-1*h^-1)))

Hg_slopes <- tidy_Hg_Hlim %>% 
  filter(between(time, 2, 8)) %>%
  group_by(Hg)%>%
  summarize(slope=lm(production_rate~growth_rate)$coefficients["growth_rate"])
Hg_slopes

Hg_slopes %>%
  ggplot()+
  geom_point(aes(Hg,slope/1000))+
  geom_smooth(aes(Hg,slope/1000))

