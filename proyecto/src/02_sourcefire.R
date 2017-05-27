library(ProjectTemplate)
reload.project()

library(FactoMineR)
library(RColorBrewer)

theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

load("cache/tab.motive.RData")
head(tab.motive)
names(tab.motive)

tab.motive %>% head
tab.motive %>% names


# ......................................... #

# Función de CA
ggCA <- function(ca.fit, var.size = 5, col.size = 3, country = F){
  mca1_vars_df <- data.frame(ca.fit$col$coord) %>% 
    rownames_to_column("columna")
  mca1_obs_df <- data.frame(ca.fit$row$coord) %>% 
    rownames_to_column("renglon") 
  if(country == T){
    mca1_obs_df <- mca1_obs_df %>% 
      mutate(col.color = ifelse(renglon == "mexico", "1.mex", "2.no mex"))
  }
  
  pal.man <- c(brewer.pal(7, "Set2"), brewer.pal(3, "Set1")[1:2])
  
  gg.1 <- ggplot(data = mca1_obs_df, 
         aes(x = Dim.1, y = Dim.2)) + 
    # geom_hline(yintercept = 0, colour = "gray70") + 
    # geom_vline(xintercept = 0, colour = "gray70") + 
    geom_point(colour = "gray50", alpha = 0.7) + 
    geom_text(data = mca1_vars_df, 
              fontface = "bold", size = var.size,
              aes(x = Dim.1, y = Dim.2, label = columna, 
                  colour = columna))  + 
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank())
    
  if(country == T){
    gg.f <- gg.1 + 
      geom_text(aes(label = renglon,
                  color= col.color,
                  size= col.color),
              # size = col.size, 
              nudge_x = .05, nudge_y = .05, 
              # color = "gray50", 
              alpha = .7) + 
    scale_size_manual(values = c(4, 3))  +
    scale_color_manual(values = c("gray20", "gray50", pal.man))
  }
  if(country == F){
    gg.f <- gg.1 + 
      geom_text(aes(label = renglon),
                size = col.size,
                nudge_x = .05, nudge_y = .05, 
                color = "gray50",
                alpha = .7)
  }
  gg.f
}



# ......................................... #

# Total
tab.motive %>% 
  group_by(source_fire_c) %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes( x = fct_reorder(source_fire_c, n) , y = n)) +
  geom_bar(stat = "identity", alpha = .7) +
  coord_flip() + 
  ylab("número de asesinatos") + 
  xlab(NULL) +
  ggtitle("Grupos políticos y oficiales principal fuente de fuego")


tab.motive %>% 
  group_by(source_fire_c, year) %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes( x= year, y = n, color = source_fire_c))+
  # geom_line(alpha = .3)+
  geom_smooth(se = F, 
              method = "loess", size = 1) + 
  scale_x_continuous(breaks = seq(1992, 2017, by = 2) ) 


# ......................................... #

# impunidad vs source_fire_c
tt <- tab.motive %>% 
  group_by(source_fire_c, impunity) %>% 
  tally %>% 
  complete(nesting(source_fire_c), impunity, fill = list(n = 0) ) %>% 
  spread(impunity, n) %>% 
  data.frame(check.names = F)

row.names(tt) <- tt$source_fire_c
ca.fit <- CA(tt[, -1], graph = F)
ggCA(ca.fit, col.size = 4) + 
  ggtitle("Impunidad asociado a la fuente de fuego") + 
  theme(legend.position  = "none")
ggsave("graphs/sourcefire/source_impunity_tot.png", width = 6, height = 5)



# type of death vs source_fire_c
tt <- tab.motive %>% 
  filter(type_death != "desconocido") %>% 
  mutate(type_death = factor(as.character(type_death)) ) %>% 
  group_by(source_fire_c, type_death) %>% 
  tally %>% 
  complete(nesting(source_fire_c), type_death, fill = list(n = 0) ) %>% 
  spread(type_death, n, fill = 0) %>% 
  data.frame(check.names = F)

row.names(tt) <- tt$source_fire_c
ca.fit <- CA(tt[, -1], graph = F)
ggCA(ca.fit, col.size = 4) + 
  ggtitle("Tipo de muerte asociado a la fuente de fuego") + 
  theme(legend.position  = "none")
ggsave("graphs/sourcefire/source_typefire_tot.png", width = 6, height = 5)
 


# ......................................... #

# Asociación de país total 1992 a 2017
tt <- tab.motive %>% 
  mutate(country_killed_c = fct_lump(country_killed, n = 20)) %>% 
  group_by(source_fire_c, country_killed_c) %>% 
  summarise(n = n_distinct(Name))
tt$country_killed_c %>% table
tab <- tt %>% 
  filter(country_killed_c != "Other") %>% 
  spread(source_fire_c, n, fill = 0) %>% 
  data.frame(check.names = F)
row.names(tab) <- tab$country_killed_c

ca.fit <- CA(tab[, -1], graph = F)
summary(ca.fit, nb.dec = 2, ncp = 2)

ggCA(ca.fit, var.size = 4.5, country = T) +
  ggtitle("Fuente de Fuego por País") + 
  theme(legend.position = "none")
ggsave(filename = "graphs/sourcefire/source_ca_country_total.png", width = 7,height = 6)


# ......................................... #

# Asociación de pais cada 10 años
tt <- tab.motive %>% 
  mutate(periodo = cut(year, breaks = c(1992, 2004, 2017), include.lowest = T, dig.lab = 5),
         country_killed_c = fct_lump(country_killed, n = 15)) %>% 
  # filter(year < 2017) %>% 
  group_by(source_fire_c, country_killed_c, periodo) %>% 
  summarise(n = n_distinct(Name))
tt
tab <- tt %>% 
  filter(country_killed_c != "Other") %>%
  spread(source_fire_c, n, fill = 0) %>% 
  data.frame(check.names = F)
apply(tab[, -1:-2], 2, sum)

ggCA_year <- function(sub){
  # sub <- tab %>% filter(cuatrienio == "[1992,1996]")
  sub <- sub %>% data.frame(check.names = F)
  row.names(sub) <- sub$country_killed_c
  tab.ca <- sub[, c(-1,-2)]
  ca.fit <- CA(tab.ca, graph = F)
  # summary(ca.fit, nb.dec = 2, ncp = 2)
  ggCA(ca.fit) +
    ggtitle(paste("Fuente de Fuego por País\n",
                  unique(sub$periodo))) +
    theme(legend.position = "none")
 }

ggca.tib <- tab %>%
  # filter( !(cuatrienio %in% c("[1992,1996]", "(1996,2000]")) ) %>% 
  group_by(periodo) %>%
  do(ggca = ggCA_year(.))
sapply(1:nrow(ggca.tib), function(num){
  ggsave(filename = paste0("graphs/sourcefire/source_ca_periodo_", num, ".png"),
         plot = ggca.tib$ggca[[num]], width = 7,height = 6)
  "fin"
})




# ............................................................................ #

# source of fire vs type of death Mexico
tt <- tab.motive %>% 
  filter(country_killed == "mexico") %>% 
  group_by(source_fire_c, impunity) %>% 
  tally %>% 
  complete(nesting(source_fire_c), impunity, fill = list(n = 0) ) %>% 
  spread(impunity, n, fill = 0) %>% 
  data.frame(check.names = F)
tt
row.names(tt) <- tt$source_fire_c
ca.fit <- CA(tt[, -1], graph = F)
ggCA(ca.fit, col.size = 4) + 
  ggtitle("Impunidad asociado a la fuente de fuego\nMéxico") + 
  theme(legend.position  = "none")
ggsave("graphs/sourcefire/source_typefire_mex.png", width = 6, height = 5)

