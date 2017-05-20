library(ProjectTemplate)
reload.project()

library(FactoMineR)
library(RColorBrewer)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

load("cache/tab.motive.RData")
head(tab.motive)

tab.motive <- tab.motive %>% 
  rename(impunity = `Impunity (for Murder)`,
         type_death = `Type of Death`,
         source_fire = `Source of Fire`) %>% 
  mutate(impunity = fct_explicit_na(factor(impunity), "na"))
tab.motive %>% head
tab.motive %>% names



# ......................................... #

# Función de CA
ggCA <- function(ca.fit, var.size = 5, col.size = 3){
  mca1_vars_df <- data.frame(ca.fit$col$coord) %>% 
    rownames_to_column("columna")
  mca1_obs_df <- data.frame(ca.fit$row$coord) %>% 
    rownames_to_column("renglon") %>% 
    mutate(col.color = ifelse(renglon == "mexico", "1.mex", "2.no mex"))
  
  pal.man <- brewer.pal(4, "Set2")
  
  ggplot(data = mca1_obs_df, 
         aes(x = Dim.1, y = Dim.2)) + 
    geom_hline(yintercept = 0, colour = "gray70") + 
    geom_vline(xintercept = 0, colour = "gray70") + 
    geom_point(colour = "gray50", alpha = 0.7) + 
    # geom_density2d(colour = "gray80") +
    geom_text(aes(label = renglon,
                  color= col.color,
                  size= col.color),
              # size = col.size, 
              nudge_x = .05, nudge_y = .05, 
              # color = "gray50", 
              alpha = .7) + 
    scale_size_manual(values = c(4, 3)) +
    geom_text(data = mca1_vars_df, 
              fontface = "bold", size = var.size,
              aes(x = Dim.1, y = Dim.2, label = columna, 
                  colour = columna)) +
    scale_color_manual(values = c("gray20", "gray50", 
                                  brewer.pal(4, "Set2")))
}


# ......................................... #

# impunidad vs type death
tt <- tab.motive %>% 
  group_by(type_death, impunity) %>% 
  tally %>% 
  complete(nesting(type_death), impunity, fill = list(n = 0) ) %>% 
  spread(impunity, n) %>% 
  data.frame()

row.names(tt) <- tt$type_death
CA(tt[, -1], graph = F)


  
  

# ......................................... #
# Total
tab.motive %>% 
  group_by(impunity) %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes( x = fct_reorder(impunity, n) , y = n)) +
  geom_bar(stat = "identity", alpha = .7) +
  coord_flip() + 
  ylab("número de asesinatos") + 
  xlab("impunidad") +
  ggtitle("La mayoría de los asesinatos quedan impunes")


tab.motive %>% 
  filter(year < 2017) %>% 
  group_by(impunity, year) %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes( x= year, y = n, color = impunity))+
  geom_line(alpha = .5)+
  geom_smooth(se = F, 
              method = "loess", size = 1) + 
  scale_x_continuous(breaks = seq(1992, 2017, by = 2) ) 




# ......................................... #

# Asociación de país total 1992 a 2017
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(impunity, country_killed_c) %>% 
  summarise(n = n_distinct(Name))
tt$country_killed_c %>% table
tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(impunity, n, fill = 0) %>% 
  data.frame()
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit, var.size = 7) +
  ggtitle("Impunidad") + 
  theme(legend.position = "none")
ggsave(filename = "graphs/impunity/imp_ca_cuatri_total.png", width = 7,height = 6)





# ......................................... #

# Asociación de pais por cuatrienio
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 30)) %>% 
  filter(year < 2017) %>% 
  group_by(impunity, country_killed_c, cuatrienio) %>% 
  summarise(n = n_distinct(Name))

tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(impunity, n, fill = 0) %>% 
  data.frame()

ggCA_year <- function(sub){
  # sub <- tab %>% filter(cuatrienio == "[1992,1996]")
  sub <- sub %>% data.frame()
  row.names(sub) <- sub$country_killed_c
  tab.ca <- sub[, c(-1,-2)]
  ca.fit <- CA(tab.ca, graph = F)
  # summary(ca.fit, nb.dec = 2, ncp = 2)
  ggCA(ca.fit) +
    ggtitle(paste("Impunidad\n", 
                  unique(sub$cuatrienio))) + 
    theme(legend.position = "none") 
}

ggca.tib <- tab %>% 
  group_by(cuatrienio) %>% 
  do(ggca = ggCA_year(.))
sapply(1:nrow(ggca.tib), function(num){
  ggsave(filename = paste0("graphs/impunity/imp_ca_cuatri_", num, ".png"),
         plot = ggca.tib$ggca[[num]], width = 7,height = 6)
  "fin"
})
