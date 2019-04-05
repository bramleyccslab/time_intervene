#Initial pass plots and including individuals' plots

library(acl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sna)
library(network)
library(GGally)
library(gridExtra)

rm(list=ls())

load('../data/pilot_data.rdata')

df.tw<-filter(df.tw, practice==F)
df.ev<-filter(df.ev, practice==F)
df.be<-filter(df.be, practice==F)

#######################################
#Condition, device and edge differences
#######################################

ggplot(df.sw, aes(x=delay_cond, y=final_acc)) +
  geom_boxplot() +
  labs(x='Condition',y='Accuracy') +
  theme_bw()+
  ggtitle('Accuracy by condition') + 
  ggsave('../../figures/acc_by_cond.pdf', width = 5, height=5)

ggplot(df.sw, aes(x=delay_cond, y=final_acc)) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", size=3) +
  labs(x='Condition',y='Accuracy') +
  theme_bw()+
  ggtitle('Accuracy by condition') + 
  ggsave('../../figures/acc_by_cond_violin.pdf', width = 5, height=5)

ggplot(df.sw, aes(x=delay_cond, y=final_acc)) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", size=3) +
  labs(x='Condition',y='Accuracy') +
  theme_bw() +
  ggtitle('Accuracy by condition') + 
  ggsave('../../figures/acc_by_cond_violin.pdf', width = 5, height=5)

ggplot(df.sw, aes(x=delay_cond, y=final_acc)) +
  stat_summary(fun.y=mean, geom="bar", fill='white', color='black') +
  stat_summary(fun.data = mean_cl_normal, aes(y = final_acc),
               geom = "errorbar",  fun.args = list(mult = 1)) +
  labs(x='Condition',y='Accuracy') +
  theme_bw() +
  ggtitle('Accuracy by condition') + 
  ggsave('../../figures/acc_by_cond_bar.pdf', width = 5, height=5)

ggplot(df.tw, aes(x=delay_cond, y=acc)) +
  stat_summary(fun.y=mean, geom="bar", fill='white', color='black') +
  stat_summary(fun.data = mean_cl_normal, aes(y = acc),
               geom = "errorbar",  fun.args = list(mult = 1)) +
  labs(x='Condition',y='Accuracy') +
  facet_wrap(~cyclic, ncol=2) +
  theme_bw() +
  ggtitle('Accuracy by condition and cyclicity') + 
  ggsave('../../figures/acc_by_cond_cyclic_bar.pdf', width = 5, height=5)

  
  
  ggplot(df.tw, aes(x=trial_type, y=acc)) +
    stat_summary(fun.y=mean, geom="bar", fill='white', color='black') +
    stat_summary(fun.data = mean_cl_normal, aes(y = acc),
                 geom = "errorbar",  fun.args = list(mult = 1)) +
    scale_y_continuous(limits=c(0,1)) +
  scale_x_discrete(breaks=1:12) +
    labs(x='Device',y='Accuracy') +
    geom_hline(yintercept = 1/4, color='blue') +
    theme_bw() +
    ggtitle('Accuracy by device') + 
    ggsave('../../figures/acc_by_dev.pdf', width = 5, height=5)
  
  
  ggplot(df.tw, aes(x=trial_type, y=acc, fill=delay_cond)) +
    stat_summary(fun.y=mean, geom="bar",  color='black', position='dodge', width=.75) +
    stat_summary(fun.data = mean_cl_normal, aes(y = acc),
                 geom = "errorbar",  fun.args = list(mult = 1), position='dodge', width=.75) +
    scale_y_continuous(limits=c(0,1)) +
    scale_x_continuous(breaks=1:12) +
    labs(x='Device',y='Accuracy') +
    geom_hline(yintercept = 1/4, color='blue') +
    theme_bw()+
    ggtitle('Accuracy by device and condition') + 
    ggsave('../../figures/acc_by_dev_cond.pdf', width = 10, height=5)
  
  
  df.tw.l<-gather(df.tw, node, value, n1_cor:n6_cor) %>% mutate(value = as.numeric(value))
    
head(df.tw.l)

ggplot(df.tw.l, aes(x=trial_type, y=value, fill=node)) +
  stat_summary(fun.y=mean, geom="bar",  color='black', position='dodge', width=.75) +
  stat_summary(fun.data = mean_cl_normal, aes(y = value),
               geom = "errorbar",  fun.args = list(mult = 1), position='dodge', width=.75) +
  scale_y_continuous(limits=c(0,1)) +
  scale_x_continuous(breaks=1:12) +
  scale_fill_discrete(name='Edge',labels=c('AB','AC','BC','AD','BD','CD')) +
  labs(x='Device',y='Accuracy') +
  facet_wrap(~delay_cond, ncol=1) +
  theme_bw() +
    ggtitle('Accuracy by edge device and condition') + 
    ggsave('../../figures/acc_by_dev_cond_edge.pdf', width = 10, height=10)

#############################
#Individual plots
##############################
for (i in 1:nrow(df.sw))
{
  
  df<-filter(df.ev, ppt==i, practice==F)
  df <- df %>% mutate(to = factor(location, levels = 4:1),
                      from=factor(df$from, levels=4:1),
                      rev_to = 5-as.numeric(as.character(to)),
                      rev_from = 5-as.numeric(as.character(from)),
                      cause_time = time-with_delay)
  
    
  ggplot(df, aes(x=time, y=to)) +
    geom_point( aes(fill=type), shape=21, size=2) +
    geom_segment(aes(x=cause_time, y=rev_from, xend=time, yend=rev_to), color='grey') +
    labs(y='Component',x='Time') +
    scale_fill_grey(name='Event') +
    facet_wrap(~trial_type, ncol=3) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    ggtitle(paste('Participant ', i, ' ', df$delay_cond[1], sep='')) +
    ggsave(filename = paste('../../figures/individual/Participant_', i, '_',
                            df$delay_cond[1], '_score_',
                            df.sw$final_score[i],'_events.pdf', sep=''),
           width=8, height=8)
  
  df2<-filter(df.be, ppt==i, practice==F) %>%
    gather(node, value, n1_cor:n6_cor) %>%
    mutate(value=as.logical(value),
           to = factor(node, levels = c('n6_cor','n5_cor','n4_cor', 'n3_cor', 'n2_cor', 'n1_cor'),
                       labels=rev(c('AB','AC','BC','AD','BD','CD'))))
  tmp<-filter(df.be, ppt==i, practice==F) %>%
    gather(node, value, n1_dir:n6_dir) %>% mutate(value = factor(value))
  df2$direction<-tmp$value
  
  ggplot(df2, aes(x=time, y=to)) +
    geom_point(aes(fill=value, fill=value, shape=direction), size=5) +
    scale_fill_manual('Correct', values = c("pink", "lightGreen"), guide=F) +
    scale_shape_manual('Direction', values=c(21,24,25,23), labels = c('none','forward','backward','bidirectional')) +
    #scale_color_manual(name='Belief', values=c('pink', 'lightGreen')) +
    facet_wrap(~trial_type, ncol=3) +
    labs(y='Connection', labels=c('AB','AC','BC','AD','BD','CD')) +
    theme_bw() +
    theme(legend.position = 'bottom') +
    ggtitle(paste('Participant ', i, ' ', df2$delay_cond[1], sep=''))     +
    ggsave(filename=paste('../../figures/individual/Participant_', i, '_',
                  df2$delay_cond[1], '_score_',
                  df.sw$final_score[i],'_beliefs.pdf', sep=''), width=8, height=8)
  #TODO add point type upward downward triangle or diamon or empty circle
  cat(i, 'done\n')
}



# df<-filter(df.be, ppt==1, practice==F)
# #graphs<-list()
# 
# for (i in 1:nrow(df))
# {
#   if (df$n_nodes[i]==3)
#   {
#     g<-DBN3[,,df$belief[i]]
#   } else {
#     g<-DBN4[,,df$belief[i]]
#   }
#   
#   net<-network(g, directed=T)
#   
#   # x = gplot.layout.circle(net, NULL)
#   # net %v% "x" = x[, 1]
#   # net %v% "y" = x[, 2]
#   # # common plotting parameters
#   # b = theme(panel.background = element_rect(color = "grey50"))
#   # z = guides(color = FALSE)
#   # y = scale_y_continuous(limits = range(x[, 2] * 1.1), breaks = NULL)
#   # x = scale_x_continuous(limits = range(x[, 1] * 1.1), breaks = NULL)
#   
#   network.vertex.names(net) = LETTERS[1:df$n_nodes[i]]
#   
#   graphs[[i]]<-ggnet2(net, mode='circle', label=T, arrow.size=12, arrow.gap=.04)  + 
#     ggtitle(paste('t=',df$time[i], sep='')) #+b
#   #x + y + z +
# }


for (i in 1:11)
{
  tmp<-filter(df, trial_type==i)
  tmp2<-graphs[df$trial_type==i]
  
  p<-grid.arrange(tmp2[[1]], tmp2[[2]],tmp2[[3]],
                  nrow = 1)
  ggsave(paste('test', i, '.pdf', sep=''), width=10, height=15, p)
  
}



# show each temporal network

