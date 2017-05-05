library('ProjectTemplate')
load.project()

library(maps)
library(gganimate)

# datos actualizados

str(df.cpj.17)
df.cpj.17 %>% data.frame() %>% head

df.cpj.17 %>% 
  group_by(`Country Killed`) %>% 
  tally %>% 
  arrange(desc(n))

df.cpj.17 %>% 
  group_by(`Country Killed`) %>% 
  tally %>% 
  arrange(desc(n))

df.cpj.17 %>% 
  group_by(Sex) %>% 
  tally %>% 
  arrange(desc(n))

df.cpj.17 %>% 
  group_by(motive) %>% 
  tally %>% 
  arrange(desc(n))

str(df.cpj.17)


df.cpj.17$Nationality %>% table %>% sort
df.cpj.17$`Country Killed` %>% table %>% sort

n_distinct(df.cpj.17$`Country Killed`) # 105 paises


# MAPA
world <- map_data("world")
world %>% head


sort(unique(df.cpj.17$country_killed))
setdiff(unique(df.cpj.17$country_killed), unique(tolower(world$region)))
# setdiff( unique(tolower(world$region)), unique(df.cpj.17$country_killed)) %>% sort

df.cpj.17 %>% country_killed

tab.country <- df.cpj.17 %>% 
  filter(year <= 2017) %>% 
  group_by(year, country_killed) %>% 
  tally 

tab.country %>% 
  filter(country_killed == "colombia") %>% 
  data.frame()


gg_year <- function(sub){
  tt.gg <- world %>% 
    as_tibble() %>% 
    mutate(region = tolower(region)) %>% 
    left_join(
      sub %>% 
        mutate_if(is.factor, as.character), 
      by = c("region" = "country_killed"))
  gg <- ggplot(tt.gg, aes(x = long, y = lat, group = group, fill = n))+ 
    geom_polygon() + 
    scale_fill_continuous(na.value = "gray90") + 
    theme_nothing(legend = F) +
    # guides(fill = guide_legend(title = paste(unique(sub$year), "Freq", sep = "\n"))) + 
    coord_fixed()
  # print(gg)
  ggsave(filename = paste0("graphs/mapas_eda/mapa_", unique(sub$year), ".pdf"), 
         plot = gg, width = 7,height = 6)
  "fin"
}

gg_year(tab.country %>% filter(year == 2013))

tab.country %>% 
  group_by(year) %>% 
  do(año = gg_year(.))




# No funciona tan bien 
tt.gg <- world %>% 
  as_tibble() %>% 
  mutate(region = tolower(region)) %>% 
  full_join(
    tab.country %>% 
      mutate_if(is.factor, as.character), 
    by = c("region" = "country_killed"))
gg <- ggplot(tt.gg, aes(x = long, y = lat, group = group, fill = n, frame = year))+ 
  geom_polygon() + 
  scale_fill_continuous(na.value = "gray90") + 
  theme_nothing(legend = T)  + 
  coord_fixed()

gganimate(gg, "graphs/output.gif")

