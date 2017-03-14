
# 0. Librerías
# 1. Datos
# 2. EDA
# 3. librería SEM
# 4. librería LAVAN


# 0. Librerías
library(tidyverse)
library(forcats)
library(foreign)
library(qgraph)
library(sem)
library(lavaan)
theme_set(theme_bw())

# 1. Datos
data <- read.spss("http://www.methodsconsultants.com/data/intelligence.sav", 
                  to.data.frame=TRUE)
names(data)
data.cov <- cov(data)

# 2. EDA
data.cov %>% 
  as_tibble() %>% 
  mutate(nom = rownames(data.cov)) %>% 
  gather(var.lab, var.val, -nom) %>% 
  mutate(var.val2 = ifelse(var.lab == nom, NA, var.val), 
         nom = factor(nom, levels = c('reading', 'writing', 'math', 
                                      'analytic', 'simpsons', 
                                      'familyguy',  'amerdad')),
         var.lab = factor(var.lab, levels = c('reading', 'writing', 'math', 
                                      'analytic', 'simpsons', 
                                      'familyguy',  'amerdad'))) %>% 
  ggplot(aes(x = nom, y = var.lab, fill = var.val2)) + 
  geom_tile() + 
  ylab(NULL) + xlab(NULL) +  
  theme(axis.text.x = element_text(angle = 90)) + 
  guides(fill = guide_legend("covarianza"))

qgraph(cov(data), borders = FALSE, layout = "spring")



# 3. librería SEM
cat(file = "tareas/doc/sem_intellect.txt",
"humor -> simpsons, NA, 1
humor -> familyguy, l2, NA
humor -> amerdad, l3, NA
intell -> reading, l4, NA
intell -> writing, l5, NA
intell -> math, l6, NA
intell -> analytic, l7, NA
intell -> humor, g1, NA
simpsons <-> simpsons, e1, NA
familyguy <-> familyguy, e2, NA
amerdad <-> amerdad, e3, NA
reading <-> reading, d1, NA
writing <-> writing, d2, NA
math <-> math, d3, NA
analytic <-> analytic, d4, NA
reading <-> writing, f1, NA
intell <-> intell, NA, 1
humor <-> humor, z1, NA")

fullsem <- specifyModel(file = "tareas/doc/sem_intellect.txt")
fit.sem <- sem(fullsem, data.cov, N = 100)
summary(fit.sem)
pathDiagram(fit.sem)

pdf("diag_sem.pdf")
pathDiagram(fit.sem)
dev.off()

# 4. librería LAVAN
model <- "
# latent variable definitions (is measured by)
humor =~ l1*simpsons + l2*familyguy + l3*amerdad
intell =~ l4*reading + l5*writing + l6*math + l7*analytic
# regressions (regressed on)
intell ~ g1*humor
# correlations (correlated with)
reading ~~ writing
"

fit.lav <- lavaan::sem(model, data = data)
lavaan::summary(fit.lav, standardized=TRUE)
lavaan::coef(fit.lav)
semPaths(fit.lav, "std", curvePivot = T, layout = "circle2")
lavaan::modindices(fit.lav)
lavaan::fitmeasures(fit.lav)


# estimated values
lavaan::parameterestimates(fit.lav)
summary(fit.sem)

