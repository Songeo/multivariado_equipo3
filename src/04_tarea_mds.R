
library(tidyverse)
library(stringr)
library(lubridate)
library(MVA)
theme_set(theme_minimal())


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




# EJEMPLO MAU
library(metodosMultivariados2017)
data("senado_votaciones")
names(senado_votaciones)
dim(senado_votaciones)

NAReplace <- function(col){
  col2 <- ifelse(is.na(col), 99, col)
  return(col2)
}


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


tab.senators <- read.delim("data/the_senators_with_info.dat", header = F, sep = "|")
sort(colnames(senado_votaciones)[-1:-3])
sort(as.character(str_trim(tab.senators$V6)))

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

aprox <- p %*% sqrt( abs(c) ) %>% 
  as_tibble()

ggplot(aprox, aes(x = V1, y = V2)) + 
  geom_point() 
# geom_text(label = cnames(senado_votaciones)[-1:-3], check_overlap = T)





# CMDSCALE
fit <- cmdscale(d, eig = TRUE, k = 2)
x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y, pch = 19)


