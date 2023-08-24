a <- summary(lm(phi~GR, MG1655_Hlim_fit)) %>% 
  .$coefficients %>% 
  as_tibble() %>%
  mutate(strain = "MG1655", parameter = c("b", "m"), .before = "Estimate")
b <- summary(lm(phi~GR, MG1655_lacI_Hlim_fit))%>% 
  .$coefficients %>% 
  as_tibble() %>%
  mutate(strain = "MG1655 lacI", parameter = c("b", "m"), .before = "Estimate")
c <- summary(lm(phi~GR, BLR_Hlim_fit))%>% 
  .$coefficients %>% 
  as_tibble() %>%
  mutate(strain = "BLR", parameter = c("b", "m"), .before = "Estimate")
d <- summary(lm(phi~GR, BLR_lacI_Hlim_fit))%>% 
  .$coefficients %>% 
  as_tibble() %>%
  mutate(strain = "BLR lacI", parameter = c("b", "m"), .before = "Estimate")
e <- summary(lm(phi~GR, BL21_lacI_Hlim_fit))%>% 
  .$coefficients %>% 
  as_tibble() %>%
  mutate(strain = "BL21 lacI", parameter = c("b", "m"), .before = "Estimate")


library(gridExtra)
mytheme <- ttheme_default(base_size = unit(8, "mm"))

##############################################################################################
options(digits =5)

slope_table <- bind_rows(a,b,c,d,e)%>% 
  select(strain, parameter, Estimate)%>%
  mutate(Estimate = Estimate /1000)%>%
  pivot_wider(names_from = parameter, values_from = Estimate)%>%
  arrange(c(4,5,2,3,1))%>%
  select(strain, m)
  mutate(m = round(.$m,1))%>%
  rename(`slope (10^3)` = m)

##############################################################################################


bind_rows(a,b,c,d,e)%>% 
  filter(parameter == "m")%>%
  mutate(ymin = Estimate - qnorm(0.975)*`Std. Error`,
         ymax = Estimate + qnorm(0.975)*`Std. Error`)%>%
  ggplot(aes(strain, Estimate, ymin =ymin, ymax=ymax))+
  geom_errorbar()+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")+
  ggtitle("regression estimate m error")

bind_rows(a,b,c,d,e)%>% 
  filter(parameter == "b")%>%
  mutate(ymin = Estimate - qnorm(0.975)*`Std. Error`,
         ymax = Estimate + qnorm(0.975)*`Std. Error`)%>%
  ggplot(aes(strain, Estimate, ymin =ymin, ymax=ymax))+
  geom_errorbar()+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")+
  ggtitle("regression estimate b error")
