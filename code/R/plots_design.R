#################################################
#Plot some of the basic features of the experiment

#Currently used in thesis and also in cogsci paper
#For teh various set-up figures, e.g. the stimuli
#parts of the example patterns, the delay distributions
#################################################

library(acl)
library(igraph)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(tidyr)

#Plot the graphs----
graphs<-list(
  
  matrix(c(0,0,1,0,0,1,0,0,0), ncol=3, byrow=T),
  matrix(c(0,1,0,0,0,1,0,0,0), ncol=3, byrow=T),
  matrix(c(0,1,1,0,0,0,0,0,0), ncol=3, byrow=T),
  
  matrix(c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0), ncol=4, byrow=T),
  
  matrix(c(0,1,0, 0,0,1, 1,0,0), ncol=3, byrow=T),
  matrix(c(0,1,0, 0,0,1, 0,1,0), ncol=3, byrow=T),
  matrix(c(0,1,0, 1,0,1, 0,0,0), ncol=3, byrow=T),
  
  matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1, 1,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,0,0, 0,0,1,0, 0,1,0,1, 0,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,0,0, 0,0,1,0, 1,0,0,1, 0,0,0,0), ncol=4, byrow=T)
  
)

#matrix(c(0,1,0,0,0,0,0,0,0), ncol=3),
#matrix(c(0,1,1,0,0,1,0,0,0), ncol=3),
#matrix(c(0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0), ncol=4),
#matrix(c(0,1,1,1,0,0,1,1,0,0,0,1,0,0,0,0), ncol=4),

nodes<-labels<-c('A','B','C','D')
colours<-c('white', 'gray')



pdf('../../figures/for_thesis/subparts/ti_gen_mods.pdf', width=8, height=3)
par(mar=c(1,1,1,1), mfrow=c(2,6))
for (i in 1:length(graphs))
{
  
  G<-graph.adjacency(graphs[[i]])
  
  
  V(G)$name<-nodes[1:ncol(graphs[[i]])]
  V(G)$label<-labels[1:ncol(graphs[[i]])]
  V(G)$label.font<-2.5
  V(G)$size <- 80
  V(G)$color<-'lightgrey'
  V(G)$label.cex <- 1.5
  V(G)$label.color <- "black"
  V(G)$label.family <- "sans"
  V(G)$width<-2
  E(G)$color <- 'black'
  E(G)$width <- 2
  
  E(G)$curved = 0
  
  locations<-matrix(0,nrow(graphs[[i]]), 2)
  
  if( nrow(graphs[[i]])==3)
  {
    locations[V(G)$name==nodes[1],]<- c(100,186.6)/100
    locations[V(G)$name==nodes[2],]<- c(186.6,50)/100
    locations[V(G)$name==nodes[3],]<- c(13.4,50)/100
  } else
  {
    locations[V(G)$name==nodes[1],]<- c(0,1)
    locations[V(G)$name==nodes[2],]<- c(1,1)
    locations[V(G)$name==nodes[3],]<- c(1,0)
    locations[V(G)$name==nodes[4],]<- c(0,0)
  }
  
  
  plot(G, layout=locations, edge.arrow.size=1)
  
}
dev.off()



#Individual graphs----
i<-9
nodes<-labels<-c('A','B','C','D')
colours<-c('white', 'gray')

pdf('../../figures/for_thesis/subparts/loopout3.pdf', width=2, height=2)
par(mar=c(1,1,1,1))
G<-graph.adjacency(graphs[[i]])
V(G)$name<-nodes[1:ncol(graphs[[i]])]
V(G)$label<-labels[1:ncol(graphs[[i]])]
V(G)$label.font<-2.5
V(G)$size <- 80
V(G)$color<-'lightgrey'
V(G)$label.cex <- 1.5
V(G)$label.color <- "black"
V(G)$label.family <- "sans"
V(G)$width<-2
E(G)$color <- 'black'
E(G)$width <- 2
E(G)$curved = 0
locations<-matrix(0,nrow(graphs[[i]]), 2)
if( nrow(graphs[[i]])==3)
{
  locations[V(G)$name==nodes[1],]<- c(100,186.6)/100
  locations[V(G)$name==nodes[2],]<- c(186.6,50)/100
  locations[V(G)$name==nodes[3],]<- c(13.4,50)/100
} else
{
  locations[V(G)$name==nodes[1],]<- c(0,1)
  locations[V(G)$name==nodes[2],]<- c(1,1)
  locations[V(G)$name==nodes[3],]<- c(1,0)
  locations[V(G)$name==nodes[4],]<- c(0,0)
}
plot(G, layout=locations, edge.arrow.size=1)
dev.off()

#Delay distribution

vec<-seq(0,3, length.out=1000)#seq(1,3000, length.out=1000)
delay_mu = 1.5#1500
delay_alpha1 = 200#200
delay_alpha2 = 5#5

df<-data.frame(Time=vec) %>%
  mutate(Reliable = dgamma(Time, shape=delay_alpha1, rate = delay_alpha1/delay_mu),
         BetweenWithin = dgamma(Time, shape=delay_alpha2, rate = delay_alpha2/delay_mu))

#THESIS:
# df.l<-df %>% gather(Condition, Probability, Reliable:BetweenWithin) %>%
#   mutate(Condition= factor(Condition, levels = c('Reliable', 'BetweenWithin'), labels = c('Reliable', 'Between / Within')))
# 
# ggplot(df.l, aes(x=Time, y=Probability, linetype=Condition)) +
#   geom_line(size=1) +
#   theme_bw() +
#   theme(panel.grid = element_blank(),
#         legend.position =c(.8,.8)) +
#   labs(x='Time (ms)')
# ggsave('../../figures/for_thesis/ti_delay_distributions.pdf', width=5, height=3)

#COGSCI:
df.l<-df %>% gather(Condition, Probability, Reliable:BetweenWithin) %>%
  mutate(Condition= factor(Condition, levels = c('Reliable', 'BetweenWithin'), labels = c('Reliable', 'Unreliable')))

ggplot(df.l, aes(x=Time, y=Probability, color = Condition)) +
  geom_line(size=1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position =c(.8,.8)) +
  labs(x='Delay (s)')
ggsave('../../figures/for_cogsci_talk/delay_distributions.pdf', width=4, height=3)


# pdf('../../figures/delay_distributions.pdf', width=5, height=4)
# plot(vec, dgamma(vec, shape=delay_alpha1, rate = delay_alpha1/delay_mu), type='l', lty=2, xlab='Time',ylab='Probability')
# lines(vec, dgamma(vec, shape=delay_alpha2, rate = delay_alpha2/delay_mu), type='l', lty=1)
# legend('topright', lty=1:3, legend = c('Variable','Reliable'))
# dev.off()






#FOR HEURISTICS PLOT----

graphs<-list(
  #Start
  matrix(c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0), ncol=4, byrow=T),
  #Add 1-2
  matrix(c(0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0), ncol=4, byrow=T),
  #Add 2-3
  matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,0, 0,1,0,0), ncol=4, byrow=T),
  #Add 1-3 as well
  matrix(c(0,1,1,0, 0,0,0,0, 0,0,0,0, 0,1,0,0), ncol=4, byrow=T),
  #Add 1-3 instead
  matrix(c(0,0,1,0, 0,0,0,0, 0,0,0,0, 0,1,0,0), ncol=4, byrow=T),
  #Remove 2-4
  matrix(c(0,0,1,0, 0,0,0,0, 0,0,0,0, 0,0,0,0), ncol=4, byrow=T)
  
)

#Individual graphs

nodes<-labels<-c('A','B','C','D')
colours<-c('white', 'gray')

for (i in 1:length(graphs) )
{
  pdf(paste0('../../figures/for_thesis/subparts/incremental_change', i, '.pdf'), width=2, height=2)
  par(mar=c(1,1,1,1))
  G<-graph.adjacency(graphs[[i]])
  V(G)$name<-nodes[1:ncol(graphs[[i]])]
  V(G)$label<-labels[1:ncol(graphs[[i]])]
  V(G)$label.font<-2.5
  V(G)$size <- 80
  V(G)$color<-'lightgrey'
  V(G)$label.cex <- 1.5
  V(G)$label.color <- "black"
  V(G)$label.family <- "sans"
  V(G)$width<-2
  E(G)$color <- 'black'
  E(G)$width <- 3
  E(G)$curved = 0
  locations<-matrix(0,nrow(graphs[[i]]), 2)
  if( nrow(graphs[[i]])==3)
  {
    locations[V(G)$name==nodes[1],]<- c(100,186.6)/100
    locations[V(G)$name==nodes[2],]<- c(186.6,50)/100
    locations[V(G)$name==nodes[3],]<- c(13.4,50)/100
  } else
  {
    locations[V(G)$name==nodes[1],]<- c(0,1)
    locations[V(G)$name==nodes[2],]<- c(1,1)
    locations[V(G)$name==nodes[3],]<- c(1,0)
    locations[V(G)$name==nodes[4],]<- c(0,0)
  }
  plot(G, layout=locations, edge.arrow.size=2)
  dev.off()
}




#FOR INTRO PLOT----

graphs<-list(
  #Fork
  matrix(c(0,1,1,0,0,0,0,0,0), ncol=3, byrow=T),
  #Cycle
  matrix(c(0,1,0,1,0,1,0,0,0), ncol=3, byrow=T)
)

#Individual graphs

nodes<-labels<-c('A','B','C')
colours<-c('white', 'gray')

for (i in 1:length(graphs) )
{
  pdf(paste0('../../figures/for_thesis/subparts/intro', i, '.pdf'), width=2, height=2)
  par(mar=c(1,1,1,1))
  G<-graph.adjacency(graphs[[i]])
  V(G)$name<-nodes[1:ncol(graphs[[i]])]
  V(G)$label<-labels[1:ncol(graphs[[i]])]
  V(G)$label.font<-2.5
  V(G)$size <- 80
  V(G)$color<-'lightgrey'
  V(G)$label.cex <- 1.5
  V(G)$label.color <- "black"
  V(G)$label.family <- "sans"
  V(G)$width<-2
  E(G)$color <- 'black'
  E(G)$width <- 3
  E(G)$curved = 0
  locations<-matrix(0,nrow(graphs[[i]]), 2)
  if( nrow(graphs[[i]])==3)
  {
    locations[V(G)$name==nodes[1],]<- c(0,1)#c(100,186.6)/100
    locations[V(G)$name==nodes[2],]<- c(1,.5)#c(186.6,50)/100
    locations[V(G)$name==nodes[3],]<- c(0,0)#c(13.4,50)/100
  } else
  {
    locations[V(G)$name==nodes[1],]<- c(0,1)
    locations[V(G)$name==nodes[2],]<- c(1,1)
    locations[V(G)$name==nodes[3],]<- c(1,0)
    locations[V(G)$name==nodes[4],]<- c(0,0)
  }
  plot(G, layout=locations, edge.arrow.size=1.5)
  dev.off()
}
