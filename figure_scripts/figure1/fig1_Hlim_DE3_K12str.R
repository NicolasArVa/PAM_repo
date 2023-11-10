library(ggtext)

Hline_K <- ggplot()+
  geom_point(data = MG1655_lacI_Hlim_fit, aes(GR, phi/1000, shape = "MG1655 lacI"),
             size = 2)+
  geom_line(data = MG1655_lacI_Hline, aes(x,y/1000), 
            size = unit(0.3, "mm"), linetype = 3, show.legend = F)+
  geom_point(data = MG1655_Hlim_fit, aes(GR, phi/1000, shape = "MG1655"),
             size = 2, show.legend = F)+
  geom_line(data = MG1655_Hline, aes(x,y/1000), 
            size = unit(0.3, "mm"), linetype = 3, show.legend = F)+
  scale_shape_manual(values = c("MG1655" = 1,  
                                "MG1655 lacI" = 13), 
                     name = "Strain", labels=eval(parse_exprs(c("MG1655", "MG1655+lacI^OV"))))+
  ylim(c(0,2.8))+
  xlim(c(0.5,1.25))+
  xlab(expression(Growth~rate~(h^-1)))+
  ylab(expression(10^3~FU~OD^-1))+
  theme_classic()+
  theme(plot.margin = margin(1,1,1,1, "mm"),
        legend.title = element_text(size = unit(8, "mm"), face = "bold"),
        legend.text = element_text(size = unit(8, "mm")),
        legend.box.margin = margin(0,0,0,0),
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_text(size = unit(8, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 2.2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        aspect.ratio=1/1)

Hline_K
parse_exprs("LacI^OV")
