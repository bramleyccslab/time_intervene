#library(acl)
# library(sna)
# library(network)
library(GGally)
library(gridExtra)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(tidyr)
library(ggplot2)

rm(list=ls())


load('../data/cogsci_data.rdata')#The trialwise, beliefwise, eventwise, and subjectwise data
load('../data/mf_results.rdata')#The results of the belief model simulations given the evidence seen by participants: Rational + heuristics
#TODO check/subset mod_preds?
#load('../data/cogsci_int_data.rdata')#The results of a set of interventions simulations using different rules


# load('../data/pilot_data.rdata')
# df.tw<-filter(df.tw, practice==F, delay_cond!='between')
# df.ev<-filter(df.ev, practice==F, delay_cond!='between')
# not_between_ix <- df.be$delay_cond[df.be$practice==F] !='between'
# df.be<-filter(df.be, practice==F, delay_cond!='between')
# df.sw<-filter(df.sw, delay_cond!='between')
# 
# df.tw$delay_cond<-factor(df.tw$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
# df.ev$delay_cond<-factor(df.ev$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
# df.be$delay_cond<-factor(df.be$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
# df.sw$delay_cond<- factor(df.sw$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
# 
# 
# row.names(df.tw)<-NULL
# row.names(df.ev)<-NULL
# row.names(df.be)<-NULL
# row.names(df.sw)<-NULL


########################
#Performance by condition etc---

#Adding Bayesian predictions
all_mod_preds<-mod_preds #Keep the full length bayesian predictions around just in case
mod_preds<-all_mod_preds[not_between_ix,]
mod_preds$delay_cond<-df.be$delay_cond
mod_preds$cyclic<-as.numeric(mod_preds$trial_type)>6
mod_preds$bayes.acc<-mod_preds$bayes.perf/mod_preds$check
mod_preds$mostrecent.acc<-mod_preds$mostrecent.perf/mod_preds$check
mod_preds$morelikely.acc<-mod_preds$morelikely.perf/mod_preds$check
mod_preds$morelikely.prune.acc<-mod_preds$morelikely.prune.perf/mod_preds$check
no_ans<-is.na(mod_preds$bayes)
unpredictable_trials<- mod_preds %>% filter(final_judgment==T, bayes.exist==T, no_ans)
df.mod<-               mod_preds %>% filter(final_judgment==T, bayes.exist==T, !no_ans)
#42/515 (8.1%) trials end in NA's, almost entirely cyclic trials especially trial type '9'.

df.mod.s<-df.mod %>% group_by(delay_cond) %>%
  summarise(sum_bayes = sum(bayes.perf), sum_check = sum(check)) %>%
  mutate(bayes.acc = sum_bayes/sum_check)

#A play performance by condition plot
# ggplot(df.sw, aes(x=delay_cond, y=final_acc)) +
#   geom_boxplot() +
#   geom_hline(yintercept = 1/4, color='grey', linetype='dashed') +
#   stat_summary(fun.y=mean, geom="point", size=3, show.legend = F) +
#   stat_summary(data = df.mod.s, aes(x=delay_cond, y=bayes.acc), fun.y=mean, geom="point", size=3, color='red', shape=17, show.legend = F) +
#   #    stat_summary(data = df.mod, aes(x=delay_cond, y=mostrecent.acc), fun.y=mean, geom="point", size=3, color='green') +
#   #    stat_summary(data = df.mod, aes(x=delay_cond, y=morelikely.acc), fun.y=mean, geom="point", size=3, color='yellow') +
#   #    stat_summary(data = df.mod, aes(x=delay_cond, y=morelikely.prune.acc), fun.y=mean, geom="point", size=3, color='purple') +
#   labs(x='Condition',y='Accuracy') +
#   theme_bw() +
#   coord_cartesian(y=c(0,1)) +
#   theme(panel.grid = element_blank()) +
#   ggtitle('Accuracy by condition') + 
#   ggsave('../../figures/for_cogsci/acc_cond.pdf', width = 5, height=5)


#Divide it by cyclic/noncyclic
acc_cy_ncy<-df.tw %>% group_by(cyclic, ppt) %>% summarise(sum(score))
df.sw$final_acc.cy<-unname(unlist(filter(acc_cy_ncy, cyclic==T)[,3]))/27
df.sw$final_acc.ncy<-unname(unlist(filter(acc_cy_ncy, cyclic==F)[,3]))/27
df.sw.l<-df.sw %>% gather(cyclic, final_acc_cy_ncy, final_acc.cy:final_acc.ncy)
df.sw.l$cyclic<-factor(df.sw.l$cyclic, levels = c('final_acc.ncy', 'final_acc.cy'))
df.sw.l$cyclic <-recode(df.sw.l$cyclic, 'final_acc.ncy' = 'FALSE', 'final_acc.cy'='TRUE')

#df.sw.l$delay_cond<-factor(df.sw.l$delay_cond, levels=c('reliable','unreliable'), labels = c('reliable', 'variable-within', 'variable-between'))

df.mod.s<-df.mod %>% group_by(delay_cond, cyclic) %>%
  summarise(sum_bayes = sum(bayes.perf), sum_check = sum(check)) %>%
  mutate(bayes.acc = sum_bayes/sum_check)

levels(df.sw.l$cyclic)<-c('Acyclic','Cyclic')
df.mod.s$cyclic<-factor(as.character(df.mod.s$cyclic), labels = c('Acyclic','Cyclic'))

ggplot(df.sw.l, aes(x=cyclic, y=final_acc_cy_ncy, fill=delay_cond)) +
  #geom_boxplot() +
  
  stat_summary(aes(y=final_acc_cy_ncy), fun.y=mean, geom="bar", color = 'black', position = position_dodge()) +
  #stat_summary(aes(y=final_acc), fun.y=mean, geom="point", size=5, shape = 18, show.legend=F) +
  stat_summary(data = df.mod.s, aes(x=cyclic, fill=delay_cond, y=bayes.acc),
               fun.y=mean, geom="point", size=3, color='red',
               shape=17, position = position_dodge(width = .75), show.legend=F) +
  geom_hline(yintercept = 1/4, color='grey', linetype='dashed') +
  #scale_fill_grey(start = 1, end =.7) +
  labs(x='',y='Accuracy', fill='Condition') +
  theme_bw() +
  coord_cartesian(y=c(0,1)) +
  theme(panel.grid = element_blank(),
        legend.position = c(.8,.15)) +
  # guides(shape = guide_legend(override.aes = list(size = 10))) +
  #ggtitle('Accuracy by condition') + 
  ggsave('../../figures/for_cogsci/acc_cond_cyclic.pdf', width = 4.5, height=3.5)

###############################
#Weighted graphs----

#Plot weighted graphs----
# ord_ix<-c(1:6, 8,7,9,11,10,12)
# nodes<-c('A','B','C','D')
# cols<-colorRampPalette( c("red", "grey", "green"), space="rgb")(100)
# intcols<-colorRampPalette( c("white", "black"), space="rgb")(100)
# 
# pdf('../../figures/for_cogsci/subparts/mod_acc.pdf', width=8, height=3)
# par(mar=c(1,1,1,1), mfrow=c(2,6))
# for (i in 1:12)
# {
#   ix<-ord_ix[i]
#   tmp<-df.tw %>% filter(trial_type==ix) %>% select(n1_cor:n6_cor)
#   tmp2<-apply(tmp, 2, mean)
#   weights<-tmp2[!is.na(tmp2)]
#   
#   tmp<-df.tw %>% filter(trial_type==ix) %>% select(n_ac1:n_ac4)
#   tmp2<-apply(tmp, 2, mean)
#   node_weights<-tmp2[!is.na(tmp2)]
#   
#   cat(ix, node_weights, '     ',
#       ceiling(node_weights*100/6), '    ',
#       (ceiling(node_weights* (100 / 6))-13), '   ',
#       round( (ceiling(node_weights* (100 / 6))-13) * (100/21)), '\n')
#   
#   if (df.tw$n_nodes[df.tw$trial_type==ix][1]==3)
#   {
#     graph<-DBN3[,,df.tw$graph[df.tw$trial_type==ix][1]]
#     
#     locations<-matrix(0,nrow(graph), 2)
#     locations[1,]<- c(100,186.6)/100
#     locations[2,]<- c(186.6,50)/100
#     locations[3,]<- c(13.4,50)/100
#   } else {
#     graph<-DBN4[,,df.tw$graph[df.tw$trial_type==ix][1]]
#     
#     locations<-matrix(0,nrow(graph), 2)
#     locations[1,]<- c(0,1)
#     locations[2,]<- c(1,1)
#     locations[3,]<- c(1,0)
#     locations[4,]<- c(0,0)
#   }
#   
#   fullgraph<-matrix(1, nrow(graph), ncol(graph))
#   diag(fullgraph)<-0
#   fullgraph[upper.tri(fullgraph)]<-0
#   Gfull<-graph.adjacency(fullgraph)
#   G<-graph.adjacency(graph)
#   
#   V(Gfull)$name<-V(G)$name<-nodes[1:ncol(graph)]
#   V(Gfull)$label<-V(G)$label<-nodes[1:ncol(graph)]
#   V(Gfull)$label.font<-V(G)$label.font<-2.5
#   V(Gfull)$size<-V(G)$size <- 80
#   V(G)$color<-intcols[ round( (ceiling(node_weights* (100 / 6))-13) * (100/21))]
#   V(Gfull)$color<-'lightgrey'
#   
#   V(Gfull)$label.cex<-V(G)$label.cex <- 1.5
#   V(Gfull)$label.color<-V(G)$label.color <- c('black', 'white')[(round( (ceiling(node_weights* (100 / 6))-13) * (100/21))>50)+1]
#   V(Gfull)$label.family<-V(G)$label.family <- "sans"
#   V(Gfull)$width<-V(G)$width<-2
#   E(G)$color <-'black'  #c('white', 'black')[(ceiling(weights*100)>50)+1]#
#   E(Gfull)$color<-cols[ceiling(weights*100)]#[ceiling(seq(1, 100, length.out = 6))]#ceiling(weights*100)
#   E(G)$width <- 2
#   E(Gfull)$width<-20
#   #E(Gfull)$curved<-E(G)$curved = 0
#   
#   plot(Gfull, layout=locations,  edge.arrow.size=0)
#   plot(G, layout=locations,  edge.arrow.size=1, add=T)
# }
# dev.off()
# pdf('../../figures/for_cogsci/subparts/int_legend.pdf', width=3, height=1)
# par(mar=c(3,1,1,1))
# image(matrix(seq(0,1,length.out=100),100), col=intcols, yaxt='n', xaxt='n')
# axis(1, at = seq(0,1,length.out=5), labels = round(seq(0.8333333, 2.016667, length.out=5), digits=2))
# dev.off()


ord_ix<-c(1:6, 8,7,9,11,10,12)
nodes<-c('A','B','C','D')
cols<-colorRampPalette( c("red", "grey", "green"), space="rgb")(100)
intcols<-colorRampPalette( c("white", "black"), space="rgb")(100)

pdf('../../figures/for_cogsci/subparts/acc_legend.pdf', width=3, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=100), 100), col=cols, yaxt='n', xaxt='n')
axis(1, at = seq(0,1,length.out=5), labels = paste(seq(0, 100, length.out=5), '%', sep=''))
dev.off()

pdf('../../figures/for_cogsci/subparts/acc_legend_v.pdf', width=1, height=3)
par(mar=c(1,3,1,1))
image(matrix(seq(0,1,length.out=100), 1), col=cols, yaxt='n', xaxt='n')
axis(2, at = seq(0,1,length.out=5), labels = paste(seq(0, 100, length.out=5), '%', sep=''), las=1)
dev.off()

nw<-c()

for (cond in c('reliable','unreliable'))
{
  pdf(paste('../../figures/for_cogsci/subparts/mod_acc_', cond, '.pdf', sep=''), width=10, height=3)
  par(mar=c(0.5,1,0.5,3), mfrow=c(2,6))
  for (i in 1:12)
  {
    ix<-ord_ix[i]
    tmp<-df.tw %>% filter(trial_type==ix, delay_cond==cond) %>% select(n1_cor:n6_cor)
    tmp2<-apply(tmp, 2, mean)
    weights<-tmp2[!is.na(tmp2)]
    
    tmp<-df.tw %>% filter(trial_type==ix, delay_cond==cond) %>% select(n_ac1:n_ac4)
    tmp2<-apply(tmp, 2, mean)
    node_weights<-tmp2[!is.na(tmp2)]
    nw<-c(nw, node_weights)
    cat(ix, node_weights, '     ',
        ceiling(node_weights*100/6), '    ',
        (ceiling(node_weights* (100 / 6))-11), '   ',
        round( (ceiling(node_weights* (100 / 6))-11) * (100/25)), '\n')
    
    if (df.tw$n_nodes[df.tw$trial_type==ix][1]==3)
    {
      graph<-DBN3[,,df.tw$graph[df.tw$trial_type==ix][1]]
      
      locations<-matrix(0,nrow(graph), 2)
      locations[1,]<- c(100,186.6)/100
      locations[2,]<- c(186.6,50)/100
      locations[3,]<- c(13.4,50)/100
    } else {
      graph<-DBN4[,,df.tw$graph[df.tw$trial_type==ix][1]]
      
      locations<-matrix(0,nrow(graph), 2)
      locations[1,]<- c(0,1)
      locations[2,]<- c(1,1)
      locations[3,]<- c(1,0)
      locations[4,]<- c(0,0)
    }
    
    fullgraph<-matrix(1, nrow(graph), ncol(graph))
    diag(fullgraph)<-0
    fullgraph[upper.tri(fullgraph)]<-0
    Gfull<-graph.adjacency(fullgraph)
    G<-graph.adjacency(graph)
    
    V(Gfull)$name<-V(G)$name<-nodes[1:ncol(graph)]
    V(Gfull)$label<-V(G)$label<-nodes[1:ncol(graph)]
    V(Gfull)$label.font<-V(G)$label.font<-2.5
    V(Gfull)$size<-V(G)$size <- 80
    V(G)$color<-intcols[ round( (ceiling(node_weights* (100 / 6))-11) * (100/25))]
    V(Gfull)$color<-'lightgrey'
    
    V(Gfull)$label.cex<-V(G)$label.cex <- 1.5
    V(Gfull)$label.color<-V(G)$label.color <- c('black', 'white')[(round( (ceiling(node_weights* (100 / 6))-11) * (100/25))>50)+1]
    V(Gfull)$label.family<-V(G)$label.family <- "sans"
    V(Gfull)$width<-V(G)$width<-2
    E(G)$color <- 'black'
    E(Gfull)$color<-cols[ceiling(weights*100)]#[ceiling(seq(1, 100, length.out = 6))]#ceiling(weights*100)
    E(G)$width <- 2
    E(Gfull)$width<-20
    #E(Gfull)$curved<-E(G)$curved = 0
    
    plot(Gfull, layout=locations,  edge.arrow.size=0)
    plot(G, layout=locations,  edge.arrow.size=1, add=T)
    text(1.95,0,paste('Ints: ', round(sum(node_weights), digits=1), '\nAcc: ', round(100*mean(weights)), '%', sep=''))
  }
  dev.off()
}


pdf('../../figures/for_cogsci/subparts/int_cond_legend.pdf', width=3, height=1)
par(mar=c(3,1,1,1))
image(matrix(seq(0,1,length.out=100),100), col=intcols, yaxt='n', xaxt='n')
axis(1, at = seq(0,1,length.out=5), labels = round(seq(0.7, 2.15, length.out=5), digits=2))
dev.off()

pdf('../../figures/for_cogsci/subparts/int_cond_legend_v.pdf', width=1, height=3)
par(mar=c(1,3,1,1))
image(matrix(seq(0,1,length.out=100),1), col=intcols, yaxt='n', xaxt='n')
axis(2, at = seq(0,1,length.out=5), labels = round(seq(0.7, 2.15, length.out=5), digits=2), las=1)
dev.off()
# 
# df.tw$n_nodes.f<-factor(df.tw$n_nodes)
# ggplot(df.tw, aes(x=trial_type, y=acc, fill=cyclic, color=n_nodes.f)) +
#   stat_summary(fun.y=mean, geom="bar", size=2) +
#   stat_summary(fun.data = mean_cl_normal, aes(y = acc),
#                geom = "errorbar",  fun.args = list(mult = 1)) +
#   stat_summary(data = df.mod, aes(x=trial_type, y=bayes.acc), fun.y=mean, geom="point", size=3, color='red') +
#   scale_y_continuous(limits=c(0,1)) +
#   scale_x_discrete(breaks=1:12) +
#   scale_fill_grey() +
#   labs(x='Device',y='Accuracy',color='Nodes', fill='Cyclic') +
#   geom_hline(yintercept = 1/4, color='blue') +
#   theme_bw() +
#   ggtitle('Accuracy by device') + 
#   ggsave('../../figures/for_cogsci/acc_by_dev.pdf', width = 10, height=5)
# 
# 
# df.tw.l<-gather(df.tw, node, value, n1_cor:n6_cor) %>% mutate(value = as.numeric(value))
# 
# head(df.tw.l)
# 
# ggplot(df.tw.l, aes(x=trial_type, y=value, fill=node)) +
#   stat_summary(fun.y=mean, geom="bar",  color='black', position='dodge', width=.75) +
#   stat_summary(fun.data = mean_cl_normal, aes(y = value),
#                geom = "errorbar",  fun.args = list(mult = 1), position='dodge', width=.75) +
#   scale_y_continuous(limits=c(0,1)) +
#   scale_x_discrete(breaks=1:12) +
#   scale_fill_discrete(name='Edge',labels=c('AB','AC','BC','AD','BD','CD')) +
#   labs(x='Device',y='Accuracy') +
#   facet_wrap(~delay_cond, ncol=1) +
#   theme_bw() +
#   ggtitle('Accuracy by edge device and condition') + 
#   ggsave('../../figures/for_cogsci/acc_by_dev_cond_edge.pdf', width = 10, height=10)


# 
# ggplot(df.tw.l, aes(x=trial_type, y=value, fill=node)) +
#   stat_summary(fun.y=mean, geom="bar",  color='black', position='dodge', width=.75) +
#   stat_summary(fun.data = mean_cl_normal, aes(y = value),
#                geom = "errorbar",  fun.args = list(mult = 1), position='dodge', width=.75) +
#   scale_y_continuous(limits=c(0,1)) +
#   scale_x_discrete(breaks=1:12) +
#   scale_fill_discrete(name='Edge',labels=c('AB','AC','BC','AD','BD','CD')) +
#   labs(x='Device',y='Accuracy') +
#   #facet_wrap(~delay_cond, ncol=1) +
#   theme_bw() +
#   ggtitle('Accuracy by edge device and condition') + 
#   ggsave('../../figures/for_cogsci/acc_by_dev_edge.pdf', width = 10, height=5)




#Example cyclic network reliable/within/between
set.seed(1)
bet<-rgamma(2, shape=5, rate=5/1500)

within_a<-between_a<-reliable_a<-0

within_b<-within_a+rgamma(1, shape=5, rate=5/1500)
between_b<-between_a+bet[1]
reliable_b<-reliable_a+rgamma(1, shape=200, rate=200/1500)

within_c<-within_b+rgamma(1, shape=5, rate=5/1500)
between_c<-between_b+bet[2]
reliable_c<-reliable_b+rgamma(1, shape=200, rate=200/1500)

within_a<-c(within_a, within_b+rgamma(1, shape=5, rate=5/1500))
between_a<-c(between_a, between_b+bet[1])
reliable_a<-c(reliable_a, reliable_b+rgamma(1, shape=200, rate=200/1500))

for (i in 1:10)
{
  within_b<-c(within_b, within_a[length(within_a)]+rgamma(1, shape=5, rate=5/1500))
  between_b<-c(between_b, between_a[length(between_a)]+bet[1])
  reliable_b<-c(reliable_b, reliable_a[length(reliable_a)]+rgamma(1, shape=200, rate=200/1500))
  
  within_c<-c(within_c, within_b[length(within_b)]+rgamma(1, shape=5, rate=5/1500))
  between_c<-c(between_c, between_b[length(between_b)]+bet[2])
  reliable_c<-c(reliable_c, reliable_b[length(reliable_b)]+rgamma(1, shape=200, rate=200/1500))
  
  within_a<-c(within_a, within_b[length(within_b)]+rgamma(1, shape=5, rate=5/1500))
  between_a<-c(between_a, between_b[length(between_b)]+bet[1])
  reliable_a<-c(reliable_a, reliable_b[length(reliable_b)]+rgamma(1, shape=200, rate=200/1500))
}

df<-data.frame(within = c(within_a, within_b, within_c),
               between = c(between_a, between_b, between_c),
               reliable = c(reliable_a, reliable_b, reliable_c),
               node = factor(rep(c('A','B','C'), c(12, 11, 11)), levels=c('C','B','A')))

ggplot(df, aes(x=reliable, y=node)) +
  geom_point(size=7, shape=21, color='black', fill='white')

ggplot(df, aes(x=within, y=node)) +
  geom_point(size=7, shape=21, color='black', fill='white')

ggplot(df, aes(x=between, y=node)) +
  geom_point(size=7, shape=21, color='black', fill='white')



#Create an empty plot
df<-data.frame(Time=0,Component=factor(rev(c('A','B','C')), levels=rev(c('A','B','C'))))

ggplot(df, aes(x=Time, y=Component)) +
  geom_blank() +
  coord_cartesian(x=c(0,45)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank()) +
  ggsave('../../figures/for_cogsci/subparts/blank_plot.pdf', width=6, height=2)

df<-data.frame(Time=0,Component=factor(rev(c('A','B','C','D')), levels=rev(c('A','B','C','D'))))

ggplot(df, aes(x=Time, y=Component)) +
  geom_blank() +
  coord_cartesian(x=c(0,45)) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x=element_blank()) +
  ggsave('../../figures/for_cogsci/subparts/blank_plot4.pdf', width=6, height=3)




#Individual pattern----

i<-48
df<-filter(df.ev, ppt==i, practice==F)
df <- df %>% mutate(to = factor(location, levels = 4:1),
                    from=factor(df$from, levels=4:1),
                    rev_to = 5-as.numeric(as.character(to)),
                    rev_from = 5-as.numeric(as.character(from)),
                    cause_time = time-with_delay)
df<-filter(df, trial_type%in%c(4:6,10:12))

ggplot(df, aes(x=time, y=to)) +
  geom_point( aes(fill=type), shape=21, size=2) +
  #geom_segment(aes(x=cause_time, y=rev_from, xend=time, yend=rev_to), color='grey') +
  labs(y='Component',x='Time') +
  scale_fill_grey(name='Event') +
  scale_y_discrete(breaks=1:4, labels=c('A','B','C','D')) +
  facet_wrap(~trial_type, ncol=1) +
  theme_bw(base_size=15) +
  theme(legend.position = 'bottom') +
  ggtitle(paste('Participant ', i, ' ', df$delay_cond[1], sep='')) +
  ggsave(filename = '../../figures/for_cogsci/example_events_no_line.pdf',
         width=5, height=8)
ggplot(df, aes(x=time, y=to)) +
  geom_point( aes(fill=type), shape=21, size=2) +
  geom_segment(aes(x=cause_time, y=rev_from, xend=time, yend=rev_to), color='grey') +
  labs(y='Component',x='Time') +
  scale_fill_grey(name='Event') +
  scale_y_discrete(breaks=1:4, labels=c('A','B','C','D')) +
  facet_wrap(~trial_type, ncol=1) +
  theme_bw(base_size=15) +
  theme(legend.position = 'bottom') +
  ggtitle(paste('Participant ', i, ' ', df$delay_cond[1], sep='')) +
  ggsave(filename = '../../figures/for_cogsci/example_events_line.pdf',
         width=5, height=8)




df<-filter(df, trial_type%in%c(6))
levels(df$type)<-c('intervention','effect')

ggplot(df, aes(x=time, y=to)) +
  geom_point( aes(fill=type), shape=21, size=6) +
  #geom_segment(aes(x=cause_time, y=rev_from, xend=time, yend=rev_to), color='grey') +
  labs(y='Component',x='Time') +
  scale_fill_manual(name='Event', values = c('black','green')) +
  scale_y_discrete(breaks=1:4, labels=c('A','B','C','D')) +
  #facet_wrap(~trial_type, ncol=1) +
  theme_bw(base_size=10) +
  theme(legend.position = 'bottom') +
  #ggtitle(paste('Participant ', i, ' ', df$delay_cond[1], sep='')) +
  ggsave(filename = '../../figures/for_cogsci/events_for_model_48_6.pdf',
         width=5, height=3)


ggplot(df, aes(x=time, y=to)) +
  geom_point( aes(fill=type), shape=21, size=4) +
  geom_segment(aes(x=cause_time, y=rev_from, xend=time, yend=rev_to), color='grey') +
  labs(y='Component',x='Time') +
  scale_fill_grey(name='Event') +
  scale_y_discrete(breaks=1:4, labels=c('A','B','C','D')) +
  #facet_wrap(~trial_type, ncol=1) +
  theme_bw(base_size=15) +
  theme(legend.position = 'bottom') +
  #ggtitle(paste('Participant ', i, ' ', df$delay_cond[1], sep='')) +
  ggsave(filename = '../../figures/for_cogsci/within_example_line6.pdf',
         width=5, height=3)



df2<-filter(df.be, ppt==i, practice==F) %>%
  gather(node, value, n1_cor:n6_cor) %>%
  mutate(value=as.logical(value),
         to = factor(node, levels = c('n6_cor','n5_cor','n4_cor', 'n3_cor', 'n2_cor', 'n1_cor'),
                     labels=rev(c('AB','AC','BC','AD','BD','CD'))))
tmp<-filter(df.be, ppt==i, practice==F) %>%
  gather(node, value, n1_dir:n6_dir) %>% mutate(value = factor(value))
df2$direction<-tmp$value

df2<-filter(df2, trial_type%in%c(4:6,10:12))

ggplot(df2, aes(x=time, y=to)) +
  geom_point(aes(fill=value, color=value, shape=direction), size=3) +
  scale_shape_manual('Direction', values=c(21,24,25,23), labels = c('none','forward','backward','bidirectional')) +
  #scale_fill_manual('Correct', values = c("pink", "lightGreen")) +
  #scale_color_manual(name='Belief', values=c('pink', 'lightGreen')) +
  facet_wrap(~trial_type, ncol=1) +
  labs(y='Connection', labels=c('AB','AC','BC','AD','BD','CD')) +
  theme_bw() +
  theme(legend.position = 'bottom') +
  ggtitle(paste('Participant ', i, ' ', df2$delay_cond[1], sep=''))     +
  ggsave(filename = '../../figures/for_cogsci/example_beliefs.pdf',
         width=5, height=8)

df2<-filter(df2, trial_type%in%c(6))
ggplot(df2, aes(x=time, y=to)) +
  geom_point(aes(fill=value, color=value, shape=direction), size=3) +
  scale_shape_manual('Direction', values=c(21,24,25,23), labels = c('none','forward','backward','bidirectional'), drop=F) +
  #scale_fill_manual('Correct', values = c("pink", "lightGreen")) +
  #scale_color_manual(name='Belief', values=c('pink', 'lightGreen')) +
  #facet_wrap(~trial_type, ncol=1) +
  labs(y='Connection', x='Time', labels=c('AB','AC','BC','AD','BD','CD')) +
  theme_bw(base_size=10) +
  theme(legend.position = 'bottom') +
  #ggtitle(paste('Participant ', i, ' ', df2$delay_cond[1], sep=''))     +
  ggsave(filename = '../../figures/for_cogsci/belief_for_model_48_6.pdf',
         width=5, height=3)



#Chosen example - ppt48, device 6, within

load('../data/individual_fits/ppt48.rdata')
plot(posts[[6]][[1]], type='l')
plot(posts[[6]][[2]], type='l')
plot(posts[[6]][[3]], type='l')
plot(posts[[6]][[4]], type='l')
plot(posts[[6]][[5]], type='l')
plot(posts[[6]][[6]], type='l')

load('../data/NS_morelikely_prune.rdata')
graphs<-belief_trajectories[[48]][[6]]







nodes<-labels<-c('A','B','C','D')
colours<-c('white', 'gray')

for (i in 1:dim(graphs)[3])
{
  pdf(paste('../../figures/for_cogsci/example_48_6_belief_', i, '.pdf',sep=''), width=5, height=5)
  par(mar=c(1,1,1,1))
  G<-graph.adjacency(graphs[,,i])
  
  
  V(G)$name<-nodes[1:ncol(graphs[,,i])]
  V(G)$label<-labels[1:ncol(graphs[,,i])]
  V(G)$label.font<-2.5
  V(G)$size <- 50
  V(G)$color<-'white'
  V(G)$label.cex <- 1.5
  V(G)$label.color <- "black"
  V(G)$label.family <- "sans"
  V(G)$width<-2
  E(G)$color <- 'black'
  E(G)$width <- 2
  
  E(G)$curved = 0
  
  locations<-matrix(0,nrow(graphs[,,i]), 2)
  
  if( nrow(graphs[,,i])==3)
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
}



########################
#Example patterns----
set.seed(5)
A_time<-round(runif(5) * 43500)
B_time<-A_time + 1500
C_time<-round(runif(5) * 45000)

df<-data.frame(Component = factor(rep(c('A','B','C'), each=5), levels=c('C','B','A')),
               Time=c(A_time, B_time, C_time))

ggplot(df, aes(x=Time, y=Component)) +
  geom_point(size=7, shape=21, color='black', fill='white') +
  scale_fill_discrete(guide=F) +
  theme_bw() +
  theme(axis.text.x=element_blank()) +
  coord_cartesian(xlim=c(0,45000)) +
  ggsave('../../figures/for_cogsci/dependence_example.pdf', width=5, height=3)

ggplot(df, aes(x=Time, y=Component)) +
  geom_vline(xintercept=seq(1, 45000, 2000), col='red') +
  geom_point(size=7, shape=21, color='black', fill='white') +
  scale_fill_discrete(guide=F) +
  theme_bw(base_size=18) +
  theme(axis.text.x=element_blank()) +
  coord_cartesian(xlim=c(0,45000)) +
  ggsave('../../figures/for_cogsci/dependence_example2.pdf', width=5, height=3)

ggplot(df, aes(x=Time, y=Component)) +
  geom_vline(xintercept=seq(1, 45000, 5000), col='red') +
  geom_point(size=7, shape=21, color='black', fill='white') +
  scale_fill_discrete(guide=F) +
  theme_bw(base_size=18) +
  theme(axis.text.x=element_blank()) +
  coord_cartesian(xlim=c(0,45000)) +
  ggsave('../../figures/for_cogsci/dependence_example5.pdf', width=5, height=3)

ggplot(df, aes(x=Time, y=Component)) +
  geom_vline(xintercept=seq(1, 45000, 10000), col='red') +
  geom_point(size=7, shape=21, color='black', fill='white') +
  scale_fill_discrete(guide=F) +
  theme_bw(base_size=18) +
  theme(axis.text.x=element_blank()) +
  coord_cartesian(xlim=c(0,45000)) +
  ggsave('../../figures/for_cogsci/dependence_example10.pdf', width=5, height=3)


