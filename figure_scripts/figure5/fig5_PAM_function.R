library(deSolve)
library(tidyverse)

# Create a function for each possible parameter modification 
func_PAM <- function(iptg, g=17.129, k2=0.1, b=2791.209,
                     k1 = 0.1, kf=0.1, kr=0.1, h=0.2, 
                     ki=0.02, q=0.45, R0=0.06){
  
  FAM_wt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      
      fH <- h*(Ii/(Ii+ki))*(kf/(x+kf))
      fR <- (1-q)*(x/(x+kr))
      fC <- (1-q)*(kf/(x+kf))
      fQ <- q
      ftot <- fR + fC + fQ + fH 
      mu <- g*(R-R0)*(x/(x+k2))
      Y <- b*C*(S/(S+1))*(k1/(x+k1))
      Ir <- 1*(Ie-Ii)
      dIe <-  -1*(0.775*10^(-11))*N*Ir
      dIi <- Ir
      dN <- mu*N
      dS <- (-0.775*10^(-11))*N*Y
      dx <- 0.5*Y - 0.2*(1347.82609)*mu
      dC <- mu*((fC/ftot)-C)
      dR <- mu*((fR/ftot)-R)
      dH <- mu*((fH/ftot)-H)
      dQ <- mu*((fQ/ftot)-Q)
      
      
      list(c(dIe, dIi, dN, dS, dx, dC, dR, dH, dQ))
    })
  }
  
  parameters <- c(g = g, k2 = k2, b = b, R0=R0,
                  k1 = k1, kf = kf, h = h, kr=kr,
                  q = q, ki = ki)
  state      <- c(Ie = iptg, Ii = 0, N = 10^7,S = 2, x = 0, 
                  C = 0.05, R = 0.1, H = 0, Q = 0.1)
  times      <- seq(0, 16, by = 0.01)
  
  o <- ode(y = state, times = times, func = FAM_wt, parms = parameters)
  o <- as_tibble(o)
  o %>% mutate(mu = g*(R-R0)*x/(x+k2)) %>%
    summarise(growth_rate = max(mu), H = H[which.max(mu)])
}

# respuesta a estres hipotetica
func_stress_PAM <- function(iptg, g=17.129, k2=0.1, b=2791.209,
                            k1 = 0.1, kf=0.1, kr=0.1, h=0.2, m =0.2,
                            ki=0.02, q=0.45, R0=0.06){
  
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
      dx <- 0.5*Y - 0.2*(1347.82609)*mu
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