func_stress_PAM <- function(iptg, g=17.129, b=2791.209, 
                            k2=0.1, k1 = 0.1, kf=0.1, kr=0.1, ki=0.02,
                            h=0.2, m =0.1, q=0.45, R0=0.06){
  
  FAM_wt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      
      fH <- h*(Ii/(Ii+ki))*(kf/(x+kf))
      fM <- m*(H/(H+0.1))
      fR <- (1-q)*(x/(x+kr))
      fC <- (1-q)*(kf/(x+kf))
      fQ <- q
      ftot <- fR + fC + fQ + fH + fM
      
      mu <- g*(R-R0)*(x/(x+k2))
      Y <- b*C*(S/(S+1))*(k1/(x+k1))
      Ir <- 1*(Ie-Ii)
      
      dIe <-  -1*(0.775*10^(-11))*N*Ir
      dIi <- Ir
      dN <- mu*N
      dS <- (-0.775*10^(-11))*N*Y
      dx <- 0.5*Y - 269.5652*mu
      dC <- mu*((fC/ftot)-C)
      dR <- mu*((fR/ftot)-R)
      dH <- mu*((fH/ftot)-H)
      dQ <- mu*((fQ/ftot)-Q)
      dM <- mu*((fM/ftot)-M)
      
      
      list(c(dIe, dIi, dN, dS, dx, dC, dR, dH, dQ, dM))
    })
  }
  
  parameters <- c(g = g, k2 = k2, b = b, R0=R0,
                  k1 = k1, kf = kf, h = h, kr=kr,
                  q = q, ki = ki)
  
  state      <- c(Ie = iptg, Ii = 0, N = 10^7,S = 2, x = 0, 
                  C = 0.05, R = 0.1, H = 0, Q = 0.1, M = 0)
  
  times      <- seq(0, 16, 0.01)
  
  o <- ode(y = state, times = times, func = FAM_wt, parms = parameters)
  o <- as_tibble(o)
  o %>% mutate(mu = g*(R-R0)*x/(x+k2)) %>%
    summarise(growth_rate = max(mu), H = H[which.max(mu)])
}

func_stress_PAM_orthogonal <- function(iptg, g=17.129, b=2791.209, 
                                       k2=0.1, k1 = 0.1, kf=0.1, kr=0.1, ki=0.02,
                                       h=0.2, m =0.1, q=0.45, R0=0.06){
  
  FAM_wt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      
      fH <- h*(Ii/(Ii+ki))
      fM <- m*(H/(H+0.1))
      fR <- (1-q)*(x/(x+kr))
      fC <- (1-q)*(kf/(x+kf))
      fQ <- q
      ftot <- fR + fC + fQ + fH + fM
      
      mu <- g*(R-R0)*(x/(x+k2))
      Y <- b*C*(S/(S+1))*(k1/(x+k1))
      Ir <- 1*(Ie-Ii)
      
      dIe <-  -1*(0.775*10^(-11))*N*Ir
      dIi <- Ir
      dN <- mu*N
      dS <- (-0.775*10^(-11))*N*Y
      dx <- 0.5*Y - 269.5652*mu
      dC <- mu*((fC/ftot)-C)
      dR <- mu*((fR/ftot)-R)
      dH <- mu*((fH/ftot)-H)
      dQ <- mu*((fQ/ftot)-Q)
      dM <- mu*((fM/ftot)-M)
      
      
      list(c(dIe, dIi, dN, dS, dx, dC, dR, dH, dQ, dM))
    })
  }
  
  parameters <- c(g = g, k2 = k2, b = b, R0=R0,
                  k1 = k1, kf = kf, h = h, kr=kr,
                  q = q, ki = ki)
  state      <- c(Ie = iptg, Ii = 0, N = 10^7,S = 2, x = 0, 
                  C = 0.05, R = 0.1, H = 0, Q = 0.1, M = 0)
  times      <- seq(0, 16, by = 0.01)
  
  o <- ode(y = state, times = times, func = FAM_wt, parms = parameters)
  o <- as_tibble(o)
  o %>% mutate(mu = g*(R-R0)*x/(x+k2)) %>%
    summarise(growth_rate = max(mu), H = H[which.max(mu)])
}

{
  # Simulations and table building:
  
  cap <- seq(0.3, 2, 0.1)
  
  Hline_Rlim <- map_df(cap*17.129, function(g) func_stress_PAM(g = g, iptg=1)) %>%
    mutate(circuit_type = "coupled",
           lim = "R-lim", .before = growth_rate)
  
  Hline_Rlim_ort <- map_df(cap*17.129, function(g) func_stress_PAM_orthogonal(g = g, iptg=1)) %>%
    mutate(circuit_type = "orthologous",
           lim = "R-lim", .before = growth_rate)
  
  Hline_Clim <- map_df(cap*2791.209, function(b) func_stress_PAM(b = b, iptg=1)) %>%
    mutate(circuit_type = "coupled",
           lim = "C-lim", .before = growth_rate)
  
  Hline_Clim_ort <- map_df(cap*2791.209, function(b) func_stress_PAM_orthogonal(b = b, iptg=1)) %>%
    mutate(circuit_type = "orthologous",
           lim = "C-lim", .before = growth_rate)
  
  Hlines <- bind_rows(Hline_Rlim, Hline_Clim,
                      Hline_Rlim_ort, Hline_Clim_ort) 
}


plot <- Hlines %>%  
  ggplot(aes(growth_rate, H, shape=interaction(lim,circuit_type)))+
  theme_classic()+
  geom_point()+
  scale_shape_manual(values = c("C-lim.coupled" = 2, "R-lim.coupled" = 17,
                                "C-lim.orthologous" = 1, "R-lim.orthologous" = 16),
                     name = "limiting \ncondition")+
  theme(plot.margin = margin(0,0,0,0, "mm"),
        legend.title = element_text(size = unit(8, "mm"), face = "bold"),
        legend.text = element_text(size = unit(8, "mm")),
        legend.box.margin = margin(0,0, 0, 0),
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_text(size = unit(8, "mm"), vjust = 2),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 8),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        aspect.ratio=1/1)+
  scale_y_continuous(name = "Heterologous fraction")+
  xlab(expression(Growth~rate~(h^-1)))


ggsave("Figure7.tiff",
       plot, width = 84, height = 45, units = "mm", dpi = 600)



Hline_orthogonal
Hline_coupled
