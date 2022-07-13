rm(list=ls())
setwd('C:/data')
load('30EcoBuilder_Nest.RData')
load('30EcoBuilder_Modu.RData')
setwd('C:/data/Hsi')
load('realworld_Nest.RData')
load('realworld_Modu.RData')
library(ggplot2)
library(reshape2)    
library(ggpubr)      
library(scales)     
library(plyr)        
library(deSolve)     

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)        # for summarySE(), this must be loaded before dplyr
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}

Nest <- rbind(EcoBuilder_Nest,realworld_Nest)
Modu <- rbind(EcoBuilder_Modu,realworld_Modu)
col = c("black", "red3")
# col = c("black", "black", "black", "black", "black", "black") # for black and white plot.

for(topo in c("Nest", "Modu")){
  dat = get(topo)
  # Change columns' type as they're not quite in the correct form.
  dat$web = factor(dat$web, c("realworld", "EcoBuilder"))
  dat$iter = as.factor(unlist(dat$iter))
  dat[, 3] = as.numeric(dat[, 3]); colnames(dat)[3] = "value"
  dat2 = summarySE(data = dat, groupvars = "web", measurevar = "value", na.rm = T)
  if(topo == "Nest") {label = "NODF"; ylim = c(0, 50)}
  if(topo == "Modu") {label = "Modularity index (bipartite)"; ylim = c(0, 1)}
  
  Plot = ggplot(dat2, aes(x = web, y = value, color = web)) +
    geom_point(size = 2) +
    scale_color_manual(values = col) +
    geom_errorbar(aes(ymin =  value - se, ymax = value + se), width=.1, color = "black") +
    xlab("Topological model") + ylab(label) +
    theme_bw() + theme(text = element_text(size=12)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    ylim(ylim)
  
  assign(paste(topo, "plot", sep="."), Plot)
}; rm(Plot, label, dat2, dat, topo, ylim) # end of topo for-loop.

topo.NM = ggarrange(Nest.plot + ylab("Nestedness")  + theme(axis.title.x = element_blank()), Modu.plot + ylab("Modularity") + theme(axis.title.x = element_blank()),
                    nrow = 2, ncol = 1, heights = rep(3, 2), widths = rep(4, 2), common.legend = F)
#####
print(topo.NM)


