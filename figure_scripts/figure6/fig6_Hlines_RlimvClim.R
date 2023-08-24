# Requires Hline function

tables <- c("tidy_DE3_Rlim",
            "tidy_DE3_Clim",
            "tidy_j23_Rlim",
            "tidy_j23_Clim",
            "tidy_Hg_Rlim",
            "tidy_Hg_Clim")

for(table in tables){
  name <- str_replace_all(table, "tidy", "Hline")
  tab <- Hline(get(table)) %>% 
    mutate(conditions = str_replace_all(table, "tidy_", "")) %>%
    separate_wider_delim(cols=conditions, delim="_", 
                         names=c("circuit", "limitation"))
  
  names(tab)[1] <- "conc"
  assign(name, tab)
}
Hline_DE3_Clim <- Hline_DE3_Clim[3:6,]

Hlines <- do.call(rbind, mget(str_replace_all(tables, "tidy", "Hline")))
for(i in seq_along(Hlines$circuit)){
  if(Hlines$circuit[i] == "DE3"){
    Hlines$circuit[i] <- "A. DE3"
  } else if(Hlines$circuit[i] == "Hg"){
    Hlines$circuit[i] <- "B. merR"
  }else{
    Hlines$circuit[i] <- "C. CymR"
  }
}
#-------------------------------- plot -------------------------------------------------------------------------------

fig6 <- ggplot(data = Hlines, aes(GR, phi/1000, shape=limitation))+
  geom_point()+
  geom_smooth(method = "lm",
              se = F, linetype = "dashed",
              size = unit(0.3, "mm"))+
  scale_shape_manual(values = c("Clim" = 0, "Rlim" = 2))+
  ylab(expression(Specific~fluorescence~(10^3~UF~OD^-1)))+
  xlab(expression(Maximum~growth~rate~(h^-1)))+
  theme_classic()+
  theme(plot.margin = margin(0,0,0,0, "mm"),
        legend.title = element_text(size = unit(8, "mm"), face = "bold"),
        legend.text = element_text(size = unit(8, "mm")),
        legend.box.margin = margin(0,0, 0, 2),
        legend.key.size = unit(4, "mm"),
        axis.title.x = element_text(size = unit(8, "mm"), vjust = -1),
        axis.title.y = element_text(size = unit(8, "mm"), vjust = 2.2),
        axis.text.x = element_text(size = unit(8, "mm")),
        axis.text.y = element_text(size = unit(8, "mm")),
        strip.background = element_rect(color="white"),
        strip.text = element_text(face = "bold"))+
  theme(aspect.ratio=1/1)+
  facet_wrap(~circuit, nrow = 3, scales = "free")

fig6

ggsave("Figure6.tiff", fig6, width = 84, height = 200, units = "mm", dpi = 600)
