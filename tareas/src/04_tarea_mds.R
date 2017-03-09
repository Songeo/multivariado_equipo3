
library(tidyverse)
library(stringr)
library(lubridate)
library(MVA)

theme_set(theme_minimal())



# • ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈ • #
# AUSTRALIA

dist.au <- read.csv("http://rosetta.reltech.org/TC/v15/Mapping/data/dist-Aus.csv")
# dist.au <- read.csv("dist-Aus.csv")
dist.au

rownames(dist.au) <- dist.au[, 1]
  
fit <- cmdscale(dist.au[, -1], eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
city.names <- c("Adelaide", "Alice Springs", "Brisbane", "Darwin", "Hobart", 
                "Melbourne", "Perth", "Sydney")
text(x, y, pos = 4, labels = city.names)


# Euro Distances
euromat <-  as.matrix(eurodist)
euromat[1:5,1:5]

# CMDSCALE
fit <- cmdscale(euromat, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]

plot(x, y, pch = 19, xlim = range(x) + c(0, 600))
text(x, y, pos = 4, labels = labels(eurodist))

# MANITA
dim(euromat)
d <- euromat
n <- nrow(d)
kn <- diag(1, n) - (1/n)*rep(1, n)*rep(1, n)

dim(d)
dim(kn)
b <- (-1/2) *((kn %*% d^2) %*% kn)

eigenval <- eigen(b)
c <-  diag(eigenval$values)
p <-  eigenval$vectors
fit$eig
eigenval$values

aprox <- p %*% sqrt( abs(c) ) %>% 
  as_tibble()

ggplot(aprox, aes(x = V1, y = -1*V2)) + 
  geom_point() + 
  geom_text(label = rownames(d))







# • ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈ • #
# EJEMPLO SENADORES
library(metodosMultivariados2017)

# 0. datos
data("senado_votaciones")
names(senado_votaciones)
dim(senado_votaciones)


aux.senadores <- read.csv("tareas/data/senadores-partidos.csv", 
                          col.names = c("senador", "partido", "estado")) %>% 
  mutate(partido = str_trim(partido))
sort(colnames(senado_votaciones)[-1:-3])
sort(as.character(str_trim(aux.senadores$senador)))

tab.senadores <- data.frame(
  senador = colnames(senado_votaciones)[-1:-3]
  ) %>% 
  left_join(aux.senadores, by = "senador") %>% 
  mutate(senador.id = row.names(.), 
         partido = str_replace_all(partido, "ṔAN", "PAN"))
head(tab.senadores)


# 1. funciones
NAReplace <- function(col){
  col2 <- ifelse(is.na(col), 900, col)
  return(col2)
}


# 2. mds
# PROPUESTA
d <- senado_votaciones %>% 
  dplyr::select(-1:-3) %>% 
  # t() %>% 
  as_tibble() %>% 
  mutate_all(funs(NAReplace(.))) %>% 
  dist(method = "euclidean") %>% 
  as.matrix()

n <- nrow(d)
kn <- diag(1, n) - (1/n)*rep(1, n)*rep(1, n)

dim(d)
dim(kn)
b <- (-1/2) *((kn %*% d^2) %*% kn)

eigenval <- eigen(b)
c <-  diag(eigenval$values)
p <-  eigenval$vectors
eigenval$values

aprox <- p %*% sqrt( abs(c) ) %>% 
  as_tibble()

fechas.vec <- factor(month(senado_votaciones$FECHA)) 
ggplot(aprox, aes(x = V1, y = V2)) + 
  geom_point(aes(color = fechas.vec) )  + 
  theme(legend.position = 'bottom')
  # geom_text(label = cnames(senado_votaciones)[-1:-3], check_overlap = T)

fit <- cmdscale(d, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y, pch = 19)




# SENADORES
d <- senado_votaciones %>% 
  dplyr::select(-1:-3) %>% 
  t() %>%
  as_tibble() %>% 
  mutate_all(funs(NAReplace(.))) %>% 
  dist(method = "euclidean") %>% 
  as.matrix()

n <- nrow(d)
kn <- diag(1, n) - (1/n)*rep(1, n)*rep(1, n)

dim(d)
dim(kn)
b <- (-1/2) *((kn %*% d^2) %*% kn)

eigenval <- eigen(b)
c <-  diag(eigenval$values)
p <-  eigenval$vectors
eigenval$values

aprox <- p %*% sqrt( abs(c) ) 

tab.gg <- aprox %>% 
  as_tibble() %>% 
  mutate(senador.id = row.names(.)) %>% 
  left_join(tab.senadores, by = 'senador.id')


gg <- ggplot(tab.gg, aes(x = V1, y = V2, color = partido)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("blue", "yellow", "red", "black", "green", "gray"))
ggplotly(gg)


# CMDSCALE
fit <- cmdscale(d, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y, pch = 19)



ggplot(tab.gg, aes(x = V1, y = V2, 
                   color = partido,label = tab.gg$senador)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("blue", "gray", "red", "orange", "green", "black"))+
  geom_text(check_overlap = T)

ggplot(tab.gg[tab.gg$V1<0,], aes(x = V1, y = V2,
                                 color = partido,label = tab.gg[tab.gg$V1<0,]$senador)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("blue", "gray", "red", "orange", "green", "black"))+
  geom_text(check_overlap = T)

ggplot(tab.gg[tab.gg$V2>175,], aes(x = V1, y = V2, 
                                   color = partido,label = tab.gg[tab.gg$V2>175,]$senador)) + 
  geom_point(size = 3) + 
  scale_color_manual(values = c("blue", "gray", "red", "orange", "green", "black"))+
  geom_text(check_overlap = F)




# # • ≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈ • #
# # EJEMPLO MARGINACIÓN
# 
# 
# # 0. datos
# df.marginacion <- read_csv("tareas/data/Base_Indice_de_marginacion_municipal_90-15.csv") %>%
#   rename(year = AÑO) %>%
#   filter(year == 2015)
# 
# # 1. mds
# d <- df.marginacion %>% 
#   dplyr::select(ANALF:OVPT) %>% 
#   as_tibble() %>% 
#   mutate_all(funs(NAReplace(.))) %>% 
#   dist(method = "euclidean") %>% 
#   as.matrix()
# 
# n <- nrow(d)
# kn <- diag(1, n) - (1/n)*rep(1, n)*rep(1, n)
# 
# dim(d)
# dim(kn)
# b <- (-1/2) *((kn %*% d^2) %*% kn)
# 
# eigenval <- eigen(b)
# c <-  diag(eigenval$values)
# p <-  eigenval$vectors
# eigenval$values
# 
# aprox <- p %*% sqrt( abs(c) ) 
# 
# tab.gg <- aprox %>% 
#   as_tibble() #%>% 
#   # mutate(senador.id = row.names(.)) %>% 
#   # left_join(tab.senadores, by = 'senador.id')
# 
# 
# ggplot(tab.gg, aes(x = V1, y = V2)) + 
#   geom_point(size = 3) 
# 
# 
# # CMDSCALE
# fit <- cmdscale(d, eig = TRUE, k = 2)
# x <- fit$points[, 1]
# y <- fit$points[, 2]
# plot(x, y, pch = 19)
# 
# 



