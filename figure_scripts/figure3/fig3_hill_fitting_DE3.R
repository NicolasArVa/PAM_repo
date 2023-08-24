library(tidyverse)
library(ggpubr)
library(expandFunctions)
library(pracma)

s <- c("MG1655", "MG1655 lacI", "BLR", "BLR lacI", "BL21 lacI")

#------------------------ non-linear regression --------------------------------------------
plateau <- function(x){
  diff <- eDiff(x)
  diff <- abs(diff)
  n <- which.min(diff)
  x[n]
}

{
options(digits = 10)                   
  for(i in 1:5){
    
    bl <- tidy_DE3_Hlim %>% 
      filter(strain == s[i], between(time,0.5,10)) %>%
      filter(iptg == min(iptg)) %>% 
      select(phi)
    
    x <- tidy_DE3_Hlim %>% 
      filter(strain == s[i], between(time,0.5,10)) %>% 
      group_by(iptg) %>% 
      mutate(phi = phi - bl) %>%
      reframe(phi = max(phi))
    
    model <- x %>% 
      nls(phi~a*(iptg^k / (iptg^k + b^k)), 
          data = ., start = list(a=8000, b=50, k = 2),
          algorithm="port", lower=c(0,0,1)) 
    table <- model %>% 
      summary() %>% .$parameters %>% as_tibble() %>% 
      mutate(strain = s[i], parameter =c("a", "b", "k"), .before = Estimate)
    
    assign(paste0("nls", i), table)
  }
  
  hyperbolic_model_tab <- bind_rows(nls1, nls2, nls3, nls4, nls5)
  
  Hyperbolic_ind <- function(x, str="MG1655"){
  
  a <- hyperbolic_model_tab%>% filter(strain == str & parameter == "a") %>%
    pull(Estimate)
  b <- hyperbolic_model_tab%>% filter(strain == str & parameter == "b") %>%
    pull(Estimate)
  k <- hyperbolic_model_tab%>% filter(strain == str & parameter == "k") %>%
    pull(Estimate)
  
  a*(x^k)/(x^k + b^k)
}
for(i in 1:length(s)){
  H <- Hyperbolic_ind(1:1000, s[i])
  table <- tibble(iptg = 1:1000, H = H, strain = s[i])
  assign(paste0("table",i), table)
}
model_tab <- bind_rows(mget(paste0("table", 1:5)))
##------------------------  Plot -----------------------------------------------------------------------------
#cols <- heat.colors(10)
#---------- Heterologous fraction as a hyperbolic function ---------------------------------------------

  bl <- tidy_DE3_Hlim %>% 
  group_by(strain) %>%
  filter(iptg == min(iptg), between(time,0.5,10)) %>% 
  select(time, phi) %>% 
  rename(bl = phi) %>% 
  ungroup

DE3_plot <- tidy_DE3_Hlim %>% 
  left_join(bl, by = c("strain", "time")) %>%
  filter(between(time,0.5,10))%>%
  group_by(strain, iptg) %>% 
  mutate(phi = phi - bl) %>%
  summarize(phi = max(phi)) %>%
  filter(!iptg %in% c(0,1))%>%
  ggplot(aes(iptg/1000, phi/1000, shape = strain))+
  theme_classic()+
  geom_line(data = model_tab, aes(iptg/1000, H/1000, linetype = strain), 
            size = unit(0.3, "mm"), show.legend = F)+
  geom_point(aes(shape = strain), size = 3, show.legend = F)+
  #geom_point(data = data.frame(iptg) , aes(iptg/1000, H/1000, shape="NEB Stable"), size = 3)+
  xlab("IPTG concentration (mM)")+
  ylab(expression(10^3~FU~OD^-1))+
  scale_y_continuous(limit = c(0,14), breaks = seq(0, 14, by = 4))+
  scale_shape_manual(values = c("MG1655" = 1, "BLR" = 0, 
                                "MG1655 lacI" = 13, "BLR lacI" = 7, 
                                "BL21 lacI" = 2), 
                     name = "Strain")+
  scale_linetype_manual(values = c("MG1655" = 3, "BLR" = 2, 
                                "MG1655 lacI" = 3, "BLR lacI" = 2, 
                                "BL21 lacI" = 1), 
                     name = "Strain")+
  theme(plot.margin = margin(0,0,0,0, "mm"),
        legend.title = element_text(size = unit(6, "mm"), face = "bold"),
        legend.text = element_text(size = unit(6, "mm")),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.key.size = unit(5, "mm"),
        axis.title.x = element_text(size = unit(9, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(9, "mm"), vjust = 2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")))+
  theme(aspect.ratio=4/6)
}

DE3_plot
#-------------------------------------------------------------------------------------------------------

hyperbolic_model_tab %>% filter(parameter=="b")%>%
  select(-`t value`, -`Pr(>|t|)`) %>% 
  mutate(low = Estimate - qnorm(0.975)*`Std. Error`, 
         high = Estimate + qnorm(0.975)*`Std. Error`)%>%
  ggplot(aes(strain, Estimate, ymin=low, ymax=high))+
  geom_errorbar()+
  geom_point()+
  geom_hline(yintercept = 2, color = "red")+
  ggtitle("Induction constant for different strains")+
  ylab(expression("Induction constant ("~mu*M~")"))+
  ylim(0, 360)

hyperbolic_model_tab %>% filter(parameter=="k")%>%
  select(-`t value`, -`Pr(>|t|)`) %>% 
  mutate(low = Estimate - qnorm(0.975)*`Std. Error`, 
         high = Estimate + qnorm(0.975)*`Std. Error`)%>%
  ggplot(aes(strain, Estimate, ymin=low, ymax=high))+
  geom_errorbar()+
  geom_point()+
  ggtitle("Induction constant for different strains")+
  ylab(expression("Induction constant ("~mu*M~")"))+
  ylim(0, 4)

hyperbolic_model_tab %>% filter(parameter=="a")%>%
  select(-`t value`, -`Pr(>|t|)`) %>% 
  mutate(low = Estimate - qnorm(0.975)*`Std. Error`, 
         high = Estimate + qnorm(0.975)*`Std. Error`)%>%
  ggplot(aes(strain, Estimate, ymin=low, ymax=high))+
  geom_errorbar()+
  geom_point()+
  ggtitle("Induction constant for different strains")+
  ylab("Induction constant (mM)")
ylim(0, 5000)


hyperbolic_model_tab %>% 
  select(strain,parameter, Estimate) %>% 
  pivot_wider(names_from = parameter, values_from = Estimate)%>% View
