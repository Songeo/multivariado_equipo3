library(ProjectTemplate)
load.project()

library(FactoMineR)
library(RColorBrewer)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

library(FactoMineR)
library(RColorBrewer)

load("cache/tab.motive.RData")
head(tab.motive)

tab.motive %>% head
tab.motive %>% names




# ......................................... #

# Función de CA
ggCA <- function(ca.fit, var.size = 5, col.size = 3){
  mca1_vars_df <- data.frame(ca.fit$col$coord) %>% 
    rownames_to_column("columna")
  mca1_obs_df <- data.frame(ca.fit$row$coord) %>% 
    rownames_to_column("renglon") %>% 
    mutate(col.color = ifelse(renglon == "mexico", "si", "no"))
  
  ggplot(data = mca1_obs_df, 
         aes(x = Dim.1, y = Dim.2)) + 
    # geom_hline(yintercept = 0, colour = "gray70") + 
    # geom_vline(xintercept = 0, colour = "gray70") + 
    geom_point(colour = "gray50", alpha = 0.7) + 
    # geom_density2d(colour = "gray80") +
    geom_text(aes(label = renglon,
                  color= col.color,
                  size= col.color),
              # size = col.size, 
              nudge_x = .05, nudge_y = .05, 
              # color = "gray50", 
              alpha = .7) + 
    scale_size_manual(values = c(3, 4))+
    geom_text(data = mca1_vars_df, 
              fontface = "bold", size = var.size,
              aes(x = Dim.1, y = Dim.2, label = columna, 
                  colour = columna)) +
    scale_color_manual(values = c(brewer.pal(3, "Set2"), 
                                  "gray50", "gray20")) + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank())
}



# ......................................... #

# Total
tab.motive %>% 
  group_by(type_death) %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes( x = fct_reorder(type_death, n) , y = n)) +
  geom_bar(stat = "identity", alpha = .7) +
  coord_flip() + 
  ylab("número de asesinatos") + 
  xlab(NULL) +
  ggtitle("Principal clasificación de muerte como asesinato")


tab.motive %>% 
  group_by(type_death, year) %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes( x= year, y = n, color = type_death))+
  geom_line(alpha = .5)+
  geom_smooth(se = F, 
              method = "loess", size = 1) + 
  scale_x_continuous(breaks = seq(1992, 2017, by = 2) ) 
  


# ......................................... #

# Asociación de país total 1992 a 2017
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 15)) %>% 
  group_by(type_death, country_killed_c) %>% 
  summarise(n = n_distinct(Name))
tt$country_killed_c %>% table
tab <- tt %>% 
  filter(country_killed_c != "Other",
         type_death != "desconocido") %>% 
  spread(type_death, n, fill = 0) %>% 
  data.frame(check.names = F)
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit) +
  ggtitle("Tipo de Muerte") + 
  theme(legend.position = "none")
ggsave(filename = "graphs/typeofdeath/ca_cuatri_total.png", width = 7,height = 6)



# ......................................... #

# Asociación de pais por cuatrienio
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(type_death, country_killed_c, cuatrienio) %>% 
  summarise(n = n_distinct(Name))

tab <- tt %>% 
  filter(country_killed_c != "Other",
         type_death != "desconocido") %>% 
  spread(type_death, n, fill = 0) %>% 
  data.frame(check.names = F)

ggCA_year <- function(sub){
  # sub <- tab %>% filter(cuatrienio == "[1992,1996]")
  sub <- sub %>% data.frame()
  row.names(sub) <- sub$country_killed_c
  tab.ca <- sub[, c(-1,-2)]
  ca.fit <- CA(tab.ca, graph = F)
  # summary(ca.fit, nb.dec = 2, ncp = 2)
  ggCA(ca.fit) +
    ggtitle(paste("Tipo de Muerte\n", 
                  unique(sub$cuatrienio))) + 
    theme(legend.position = "none") 
}

ggca.tib <- tab %>% 
  group_by(cuatrienio) %>% 
  do(ggca = ggCA_year(.))
sapply(1:nrow(ggca.tib), function(num){
  ggsave(filename = paste0("graphs/typeofdeath/ca_cuatri_", num, ".png"),
         plot = ggca.tib$ggca[[num]], width = 7,height = 6)
  "fin"
})
