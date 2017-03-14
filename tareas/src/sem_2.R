library(foreign)
data <- read.spss("http://www.methodsconsultants.com/data/intelligence.sav", to.data.frame=TRUE)

names(data)
dataCov<-cov(data)

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
intell <-> intell, NA, 1
humor <-> humor, z1, NA")

fullsem <- specifyModel("tareas/doc/sem_intellect.txt")

out <- sem(fullsem,dataCov,N=100)
summary(out)
pathDiagram(out)

