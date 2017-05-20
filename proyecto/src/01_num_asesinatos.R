
library(ProjectTemplate)
reload.project()

theme_update(plot.title = element_text(hjust = 0.5))

# Función para mapas
world <- map_data(map = "world")
world %>% head
ggmap_freq <- function(sub){
  tt.gg <- world %>% 
    as_tibble() %>% 
    mutate(region = tolower(region)) %>% 
    left_join( sub, 
               by = c("region" = "country_killed"))
  ggplot(tt.gg, aes(x = long, y = lat, 
                    group = group, fill = freq))+ 
    geom_polygon() + 
    theme_bw() + 
    coord_fixed() + 
    theme(rect = element_blank(), 
          line = element_blank(),
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = .5),
          legend.position = "bottom") + 
    xlab(NULL) +
    ylab(NULL)
}



# 0. Recodificación de algunas cosas
names(df.cpj.17)

tab.mot <- df.cpj.17 %>% 
  dplyr::select(rowname:country_killed) %>% 
  filter(motive == "motive confirmed") 
head(tab.mot)

aux <- tab.mot %>% 
  filter(is.na(year)) %>%
  filter(`day-month` != "unknown") %>% 
  mutate(year = parse_number(`day-month`)) %>% 
  mutate(year = parse_number( ifelse(year < 29, 
                       str_sub(`day-month`, -4, -1), 
                       year)) ) %>% 
  data.frame()
aux

tab.motive <- tab.mot %>% 
  filter(!(rowname %in% aux$rowname)) %>% 
  bind_rows(aux) %>% 
  filter(`day-month` != "unknown") %>% 
  mutate(quinquenio = cut(year, breaks = seq(1990, 2020, by = 5), 
                          include.lowest = T),
         cuatrienio = cut(year, breaks = seq(1992, 2020, by = 4), 
                          include.lowest = T))
tab.motive %>% head
cache("tab.motive")

apply(is.na(tab.motive), 2, sum)
filter(tab.motive, is.na(quinquenio))

length(tab.motive$Name)
n_distinct(tab.motive$Name)



# ......................................... #
# Muertes global por año
tab.motive %>% 
  group_by(year) %>% 
  # tally %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes(x = year, y = n)) + 
  geom_bar(stat = "identity", alpha = .7) + 
  geom_smooth(se = F, color = "blue", 
              method = "loess", size = 2) + 
  scale_x_continuous(breaks = seq(1992, 2017, by = 2) ) + 
  ylab("número de asesinatos") + 
  xlab("año") +
  ggtitle("Número de muertes aumenta en 2004") +
  geom_label(x = 1993, y = 74,
             label = paste("Global:", n_distinct(tab.motive$Name)), 
             color = "blue")



tab.motive %>% 
  group_by(cuatrienio) %>% 
  # tally %>% 
  summarise(n = n_distinct(Name)) %>% 
  ggplot(aes(x = cuatrienio, y = n)) + 
  geom_bar(stat = "identity", alpha = .7) + 
  # geom_hline(yintercept = 238, color = "blue", linetype = 2) +
  geom_label(aes(label = n), color = "blue") +
  ylab("número de asesinatos") + 
  xlab("cuatrienio") +
  ggtitle("2008-2016 comportamiento similar a 1992-1996") 





# ......................................... #
# Frecuencia por país total periodo
tt <- tab.motive %>% 
  group_by(country_killed) %>% 
  # tally %>% 
  summarise(n = n_distinct(Name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character)
tt$country_killed %>% n_distinct()

tt %>% 
  rename(freq = n) %>% 
  ggmap_freq(.) + 
  scale_fill_continuous(low = "#ffe5e5", high = "#B30000",
                        na.value = "grey95") +
  guides(fill = guide_legend(title = "# asesinatos")) + 
  ggtitle("Iraq-Syria mayor número de asesinatos 1992-2017")
  
arrange(tt, desc(n)) %>% .[1:15,]
  


# Frecuencia por país por cuatrienio
tab.ggmapy <- tab.motive %>% 
  group_by(country_killed, cuatrienio) %>% 
  # tally %>% 
  summarise(n = n_distinct(Name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character) %>% 
  ungroup() %>% 
  complete(cuatrienio, nesting(country_killed),
           fill = list(n = 0)) %>% 
  mutate(ng = cut(n, breaks = c(0, 1, 5, 10, 20, 50, 100), include.lowest = T, right = F)) %>% 
  mutate_at(.cols = "country_killed", .funs = as.character)
tab.ggmapy$country_killed %>% n_distinct()
tab.ggmapy$ng %>% table

colPalette <- colorRampPalette(colors = c("#ffe5e5",
                                          "#B30000"))(n_distinct(tab.ggmapy$ng))
ggmap_year <- function(tt){
  tt %>% 
    rename(freq = ng) %>% 
    ggmap_freq(.) + 
    guides(fill = guide_legend(title = "# asesinatos")) +
    scale_fill_manual(values = colPalette, drop = F,
                      na.value = "gray90",
                      name = "# nasesinatos")+
    ggtitle(unique(tt$cuatrienio))
}

gg.tib <- tab.ggmapy %>% 
  group_by(cuatrienio) %>% 
  do(ggmap = ggmap_year(.))
lapply(1:nrow(gg.tib), function(num){
  ggsave(filename = paste0("graphs/mapas_eda/mapa_cuatri_", num, ".png"),
         plot = gg.tib$ggmap[[num]], width = 7,height = 6)
  "fin"
})




# Frecuencia por país por año
tab.ggmapy <- tab.motive %>% 
  group_by(country_killed, year) %>% 
  # tally %>% 
  summarise(n = n_distinct(Name)) %>% 
  mutate_at(.cols = c("country_killed"), .funs = as.character) %>% 
  ungroup() %>% 
  complete(year, nesting(country_killed),
           fill = list(n = 0)) %>% 
  mutate(ng = cut(n, breaks = c(0, 1, 5, 10, 20, 35), include.lowest = T, right = F)) %>% 
  mutate_at(.cols = "country_killed", .funs = as.character)
tab.ggmapy$country_killed %>% n_distinct()
tab.ggmapy$ng %>% table

colPalette <- colorRampPalette(colors = c("#b2b2ff",
                                          "#33337f"))(n_distinct(tab.ggmapy$ng))
ggmap_year <- function(tt){
  tt %>% 
    rename(freq = ng) %>% 
    ggmap_freq(.) + 
    guides(fill = guide_legend(title = "# asesinatos")) +
    scale_fill_manual(values = colPalette, drop = F,
                      na.value = "gray90",
                      name = "# nasesinatos")+
    ggtitle(unique(tt$year)) +
    theme(title = element_text(size = 13))
}

gg.tib <- tab.ggmapy %>% 
  group_by(year) %>% 
  do(ggmap = ggmap_year(.))
lapply(1:nrow(gg.tib), function(num){
  ggsave(filename = paste0("graphs/mapas_eda/mapa_year_", num, ".png"),
         plot = gg.tib$ggmap[[num]], width = 7,height = 6)
  "fin"
})


