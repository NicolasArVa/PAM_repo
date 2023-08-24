{
#-------------------------------- BL21 lacI -----------------------------------------------------

  {
    p1 <- tidy_DE3_Hlim %>% 
      filter(strain == "BL21 lacI") %>%
      mutate(iptg = as.factor(iptg)) %>%
      ggplot(aes(time, phi, color = iptg))+
      geom_point()+
      geom_smooth(se = FALSE, span = 0.2)
    phi <- ggplot_build(p1)$data[[2]]%>% 
      as_tibble() %>% 
      select(y)
    
    p2 <- tidy_DE3_Hlim %>% 
      filter(strain == "BL21 lacI") %>%
      mutate(iptg = as.factor(iptg)) %>%
      ggplot(aes(time, growth_rate, color = iptg))+
      geom_point()+
      geom_smooth(se = FALSE, span = 0.12)
    tab <- ggplot_build(p2)$data[[2]]%>% 
      as_tibble() %>% 
      select(colour, x, y)
    
    a <- unique(tab$colour)
    ind <- order(a)
    
    b <- c(0,1,10,62,80,100,125,250,500,1000)
    
    BL21_lacI_Hlim_fit <- bind_cols(tab, phi) %>% 
      set_names(c("iptg", "time", "growth_rate", "phi")) %>% 
      group_by(iptg) %>%
      summarise(GR = max(growth_rate), phi = phi[which.max(growth_rate)]) %>%
      mutate(iptg = b[ind]) %>% 
      arrange(iptg)
  }
{
  BL21_lacI_Hlim_fit %>% 
    ggplot(aes(GR, phi, color = iptg))+
  geom_point()+
  geom_path()

lr_C <- lm(phi~GR, BL21_lacI_Hlim_fit)

tab <- lr_C$coefficients %>% 
  set_names(c("b", "a"))
a <- tab[2]
b <- tab[1]

x <- seq(0, 2, 0.05)
y <- sapply(x, function(x) a*x + b)

BL21_lacI_Hline <- tibble(x = x, y = y)
}
#------------------------------- BLR lacI ----------------------------------------------------------------------------

{
  p1 <- tidy_DE3_Hlim %>% 
    filter(strain == "BLR lacI") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, phi, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.2)
  phi <- ggplot_build(p1)$data[[2]]%>% 
    as_tibble() %>% 
    select(y)
  
  p2 <- tidy_DE3_Hlim %>% 
    filter(strain == "BLR lacI") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, growth_rate, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.12)
  tab <- ggplot_build(p2)$data[[2]]%>% 
    as_tibble() %>% 
    select(colour, x, y)
  
  a <- unique(tab$colour)
  ind <- order(a)
  
  b <- c(1,10,62,100,250,1000)
  
  BLR_lacI_Hlim_fit <- bind_cols(tab, phi) %>% 
    set_names(c("iptg", "time", "growth_rate", "phi")) %>% 
    group_by(iptg) %>%
    summarise(GR = max(growth_rate), phi = phi[which.max(growth_rate)]) %>%
    mutate(iptg = b[ind]) %>% 
    arrange(iptg)
}
{BLR_lacI_Hlim_fit %>% 
    ggplot(aes(GR, phi, color = iptg))+
    geom_point()+
    geom_path()

lr_C <- lm(phi~GR, BLR_lacI_Hlim_fit)
tab <- lr_C$coefficients %>% 
  set_names(c("b", "a"))
a <- tab[2]
b <- tab[1]

x <- seq(0, 2, 0.05)
y <- sapply(x, function(x) a*x + b)

BLR_lacI_Hline <- tibble(x = x, y = y)
}
#---------------------------------- BLR ----------------------------------------------------------------------

{
  p1 <- tidy_DE3_Hlim %>% 
    filter(strain == "BLR") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, phi, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.2)
  phi <- ggplot_build(p1)$data[[2]]%>% 
    as_tibble() %>% 
    select(y)
  
  p2 <- tidy_DE3_Hlim %>% 
    filter(strain == "BLR") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, growth_rate, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.12)
  tab <- ggplot_build(p2)$data[[2]] %>% 
    as_tibble() %>% 
    select(colour, x, y)
  
  a <- unique(tab$colour)
  ind <- order(a)
  
  b <- c(1,10,62,100,250,1000)
  
  BLR_Hlim_fit <- bind_cols(tab, phi) %>% 
    set_names(c("iptg", "time", "growth_rate", "phi")) %>% 
    group_by(iptg) %>%
    summarise(GR = max(growth_rate), phi = phi[which.max(growth_rate)]) %>%
    mutate(iptg = b[ind]) %>% 
    arrange(iptg)
  }
{BLR_Hlim_fit %>% 
    ggplot(aes(GR, phi, color = iptg))+
    geom_point()+
    geom_path()

lr_C <- lm(phi~GR, BLR_Hlim_fit)
tab <- lr_C$coefficients %>% 
  set_names(c("b", "a"))
a <- tab[2]
b <- tab[1]

x <- seq(0, 2, 0.05)
y <- sapply(x, function(x) a*x + b)

BLR_Hline <- tibble(x = x, y = y)
}
#------------------------------- MG1655 lacI -----------------------------------------------------------------

{
  p1 <- tidy_DE3_Hlim %>% 
    filter(strain == "MG1655 lacI") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, phi, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.2)
  phi <- ggplot_build(p1)$data[[2]]%>% 
    as_tibble() %>% 
    select(y)
  
  p2 <- tidy_DE3_Hlim %>% filter(strain == "MG1655 lacI") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, growth_rate, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.12)
  tab <- ggplot_build(p2)$data[[2]]%>% 
    as_tibble() %>% 
    select(colour, x, y)
  
  a <- unique(tab$colour)
  ind <- order(a)
  
  b <- c(1,10,62,100,250,1000)
  
  MG1655_lacI_Hlim_fit <- bind_cols(tab, phi) %>% 
    set_names(c("iptg", "time", "growth_rate", "phi")) %>% 
    group_by(iptg) %>%
    summarise(GR = max(growth_rate), phi = phi[which.max(growth_rate)]) %>%
    mutate(iptg = b[ind]) %>% 
    arrange(iptg)
}
{MG1655_lacI_Hlim_fit %>% 
    ggplot(aes(GR, phi, color = iptg))+
    geom_point()+
    geom_path()

lr_C <- lm(phi~GR, MG1655_lacI_Hlim_fit)
tab <- lr_C$coefficients %>% 
  set_names(c("b", "a"))
a <- tab[2]
b <- tab[1]

x <- seq(0, 2, 0.05)
y <- sapply(x, function(x) a*x + b)

MG1655_lacI_Hline <- tibble(x = x, y = y)
}
#--------------------------------- MG1655 -------------------------------------------------------------------------------------------

{
  p1 <- tidy_DE3_Hlim %>% 
    filter(strain == "MG1655") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, phi, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.2)
  phi <- ggplot_build(p1)$data[[2]] %>% 
    as_tibble() %>% 
    select(y)
  
  p2 <- tidy_DE3_Hlim %>% 
    filter(strain == "MG1655") %>%
    mutate(iptg = as.factor(iptg)) %>%
    ggplot(aes(time, growth_rate, color = iptg))+
    geom_point()+
    geom_smooth(se = FALSE, span = 0.12)
  tab <- ggplot_build(p2)$data[[2]]%>% 
    as_tibble() %>% 
    select(colour, x, y)
  
  a <- unique(tab$colour)
  ind <- order(a)
  
  b <- c(1,10,62,100,250,1000)
  
  MG1655_Hlim_fit <- bind_cols(tab, phi) %>% 
    set_names(c("iptg", "time", "growth_rate", "phi")) %>% 
    group_by(iptg) %>%
    summarise(GR = max(growth_rate), phi = phi[which.max(growth_rate)]) %>%
    mutate(iptg = b[ind]) %>% 
    arrange(iptg)
  }
{
  MG1655_Hlim_fit %>% 
    ggplot(aes(GR, phi, color = iptg))+
    geom_point()+
    geom_path()

lr_C <- lm(phi~GR, MG1655_Hlim_fit)
tab <- lr_C$coefficients %>% 
  set_names(c("b", "a"))
a <- tab[2]
b <- tab[1]

x <- seq(0, 2, 0.05)
y <- sapply(x, function(x) a*x + b)

MG1655_Hline <- tibble(x = x, y = y)
}
#---------------------------------------------------------------------------------
}
####################################################################################################

labx <- expression("Maximum growth rate ("~h^-1~")")
laby <- expression("Specific fluorescence ("~10^3~UF~OD^-1~")")
cols <- rainbow(10)
ggplot()+
  geom_point(data = BL21_lacI_Hlim_fit, aes(GR, phi/1000, color = "BL21 lacI"),
             size = 5)+
  geom_line(data = BL21_lacI_Hline, aes(x,y/1000), color = cols[1], size = 1)+
  geom_point(data = BLR_lacI_Hlim_fit, aes(GR, phi/1000, color = "BLR lacI"), 
             size = 5)+
  geom_line(data = BLR_lacI_Hline, aes(x,y/1000), color = cols[3], size = 1)+
  geom_point(data = BLR_Hlim_fit, aes(GR, phi/1000, color = "BLR"), size = 5)+
  geom_line(data = BLR_Hline, aes(x,y/1000), color = cols[5], size = 1)+
  geom_point(data = MG1655_lacI_Hlim_fit, aes(GR, phi/1000, color = "MG1655 lacI"), size = 5)+
  geom_line(data = MG1655_lacI_Hline, aes(x,y/1000), color = cols[7], size = 1)+
  geom_point(data = MG1655_Hlim_fit, aes(GR, phi/1000, color = "MG1655"), size = 5)+
  geom_line(data = MG1655_Hline, aes(x,y/1000), color = cols[9], size = 1)+
  scale_color_manual(values = c("BL21 lacI" = cols[1], "BLR lacI" = cols[3],
                                "BLR" = cols[5], "MG1655 lacI" = cols[7], 
                                "MG1655" = cols[9]))+
  ggtitle("H-lim graph for different E. coli strains")+
  ylim(c(0,3))+
  xlim(c(0.5,1.25))+
  xlab(labx)+
  ylab(laby)+
  theme_classic()+
  theme(plot.title = element_text(size= 22, vjust = 3),
        plot.margin = margin(1,1,1,1.2, "cm"),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 15, vjust = -2),
        axis.title.y = element_text(size = 15, vjust = 6))
