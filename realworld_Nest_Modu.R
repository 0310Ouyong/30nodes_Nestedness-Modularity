rm(list = ls())
getwd()
setwd('C:/data/Hsi')
library(vegan)  
library(bipartite)
load('realworld.matrix.RData')
realworld_Nest <- data.frame()
realworld_Modu <- data.frame()

#calculate nestedness
for(iter in 1:74){
  D = realworld.matrix[[iter]]
  realworld_Nest = rbind(realworld_Nest,
               rbind(c('realworld',iter,as.data.frame(unname(nestednodf(D)$statistic)[3]))))
}
colnames(realworld_Nest) = c("web", "iter", "Nest")
save(realworld_Nest,file = 'realworld_Nest.RData')


#calculate modularity
for(iter in 1:74){
  D = realworld.matrix[[iter]]
  realworld_Modu = rbind(realworld_Modu,
                         rbind(c('realworld',iter,as.data.frame(computeModules(web = D)@likelihood))))
}
colnames(realworld_Modu) = c("web", "iter", "Modu")
save(realworld_Modu,file = 'realworld_Modu.RData')
