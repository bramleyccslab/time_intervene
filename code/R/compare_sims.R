#Compare models to data
library(dplyr)
library(tidyr)
library(acl)
library(xtable)
library(ggplot2)

rm(list=ls())


load('../data/cogsci_data.rdata')
load('../data/simulated_participants.rdata')



upis<-as.character(df.sw$upi)
gix<- (head(df.tw, 12) %>% arrange(trial_type))$graph

#Note that ppt 1 t 11 is missing (I think because they performed no interventions)
df.eff<-filter(df.sim, type=='effect')
nev<-df.eff %>% group_by(learn_cond, ppt, trial_type) %>% summarise(nev = length(done)) %>% arrange(learn_cond, ppt, trial_type)

file_list<-list.files('../data/individual_fits_cogsci/sims_5sec')

df.sim.res<-data.frame(ppt=rep(1:40, each=12),
                       upi=rep(df.sw$upi, each=12),
                       trial=1:12,
                       n_nodes = c(3,3,3,4,4,4,3,3,3,4,4,4),
                       cyclic = c(F,F,F,F,F,F,T,T,T,T,T,T),
                       sim_cond = rep(1:9, each=12*40),
                       t_spacing = rep(c(0,1,2,0,1,2,0,1,2), each=12*40),
                       l_spacing = rep(c(0,0,0,1,1,1,2,2,2), each=12*40),
                       entropy=NA, max_p=NA, p_truth=NA, nev=0)
for (i in 1:9)
{
  
  
  for (j in 1:40)
  {
    #cf<-file_list[grep(paste('sims', i, 'ppt', j, upis[j], sep=''), file_list)]
    file<-paste('../data/individual_fits_cogsci/sims_5sec/sims', i, 'ppt', j, 'upi', upis[j], '.rdata', sep='')
    load(file)
   
    df.sim.res$entropy[df.sim.res$ppt==j & df.sim.res$sim_cond==i] <-sapply(posts, shannon_entropy, log_type='bits')
    df.sim.res$max_p[df.sim.res$ppt==j & df.sim.res$sim_cond==i] <-sapply(posts, max)
    df.sim.res$p_truth[df.sim.res$ppt==j & df.sim.res$sim_cond==i] <-diag(sapply(posts, '[', gix))
    
    for (k in 1:12)
    {
      if (any(nev$learn_cond==i & nev$ppt==j & nev$trial_type==k))
      df.sim.res$nev[df.sim.res$ppt==j & df.sim.res$sim_cond==i & df.sim.res$trial==k]<-
          nev$nev[nev$learn_cond==i & nev$ppt==j & nev$trial_type==k]
    }
    
  }
}

df.ppt.res<-data.frame(ppt=rep(1:40, each=12),
                       upi=rep(df.sw$upi, each=12),
                       trial=1:12,
                       n_nodes = c(3,3,3,4,4,4,3,3,3,4,4,4),
                       cyclic = c(F,F,F,F,F,F,T,T,T,T,T,T),
                       sim_cond = 10,
                       t_spacing = 3,
                       l_spacing = 3,
                       entropy=NA, max_p=NA, p_truth=NA, nev=0)

for (j in 1:40)
{
  #cf<-file_list[grep(paste('sims', i, 'ppt', j, upis[j], sep=''), file_list)]
  file<-paste('../data/individual_fits_cogsci/ppt', j, 'upi', upis[j], '.rdata', sep='')
  load(file)
  final_posts<-diag(sapply(posts, '[', sapply(posts, length)))
  
  df.ppt.res$entropy[df.ppt.res$ppt==j] <-sapply(final_posts, shannon_entropy, log_type='bits')
  df.ppt.res$max_p[df.ppt.res$ppt==j] <-sapply(final_posts, max)
  df.ppt.res$p_truth[df.ppt.res$ppt==j] <-diag(sapply(final_posts, '[', gix))
  
  for (k in 1:12)
  {
      df.ppt.res$nev[df.ppt.res$ppt==j & df.ppt.res$trial==k]<-df.tw$n_ev[df.tw$upi==upis[j] & df.tw$trial_type==k]
  }
}

df.co<-rbind(df.sim.res, df.ppt.res)

#TODO ensure no NaNs are returned by likelihood function (true structure should never be ruled out)
#TODO add participants choices as strategy "0" and remake plots

#We have 3% NANs right now....  Flagging them
#Re-ran with wider range for max/min, now more failures 9%, but deadlock broken...?
mean(is.nan(df.co$max_p)) #There is now only 1


# fill them with uniforms
df.co$entropy[is.nan(df.co$max_p) & df.co$n_nodes==3]<-shannon_entropy(1/(dim(DBN3)[3]), log_type='bits')
df.co$entropy[is.nan(df.co$max_p) & df.co$n_nodes==4]<-shannon_entropy(1/(dim(DBN4)[3]), log_type='bits')
df.co$p_truth[is.nan(df.co$p_truth) & df.co$n_nodes==3]<-1/(dim(DBN3)[3])
df.co$p_truth[is.nan(df.co$p_truth) & df.co$n_nodes==4]<-1/(dim(DBN4)[3])
df.co$max_p[is.nan(df.co$max_p) & df.co$n_nodes==3]<-1/(dim(DBN3)[3])
df.co$max_p[is.nan(df.co$max_p) & df.co$n_nodes==4]<-1/(dim(DBN4)[3])

#(Or exclude them)
#df.co.filtered<-filter(df.co, fail_flag==F) %>% select(-upi, -fail_flag)

df.co<-df.co %>% mutate(n_nodes=factor(n_nodes, levels=3:4),
                                  sim_cond=factor(sim_cond, levels=1:9),
                                  cyclic = factor(cyclic, levels=c(F,T)))


df.co<-df.co %>% mutate(location = factor(l_spacing, levels=0:3, labels=c('random','even','positive','human')),
                        timing = factor(t_spacing, levels=0:3, labels=c('random','even','reactive','human')))

save(file='../data/cogsci_int_data.rdata', df.co, df.sim.res, df.ppt.res)


###############################################################################


df.co %>% group_by(trial) %>% summarise_all(.funs='mean')
df.co %>% group_by(sim_cond) %>% summarise_all(.funs='mean')
df.co %>% group_by(t_spacing) %>% summarise_all(.funs='mean')
df.co %>% group_by(l_spacing) %>% summarise_all(.funs='mean')
data.frame(df.co %>% group_by(sim_cond, trial) %>% summarise_all(.funs='mean'))


ggplot(data=df.co, aes(y=max_p, x=location, color=timing)) +
  #geom_boxplot(size=1) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .75), size=5) + 
  stat_summary(fun.data = mean_cl_normal,geom = "errorbar", fun.args = list(mult = 1), position = position_dodge(width = .75)) +
  scale_fill_grey(start=.4, end=1) +
  labs(y='max P(M|d)') +
  theme_bw() +
  ggtitle('Intervention simulations humans')
  

ggplot(data=df.co) +
  geom_boxplot(aes(y=entropy, x=factor(l_spacing), fill=factor(t_spacing)), size=1) +
  scale_fill_grey(start=0, end=1) +
  theme_bw()

ggplot(data=df.co) +
  geom_boxplot(aes(y=p_truth, x=factor(l_spacing), fill=factor(t_spacing)), size=1) +
  scale_fill_grey(start=0, end=1)+
  theme_bw()

ggplot(data=df.sim.res) +
  geom_boxplot(aes(y=max_p, x=t_spacing, fill=cyclic))

ggplot(data=sim.res) +
  geom_boxplot(aes(y=max_p, x=l_spacing, fill=cyclic))

ggplot(data=sim.res) +
  geom_boxplot(aes(y=p_truth, x=sim_cond, fill=cyclic))

ggplot(data=sim.res) +
  geom_boxplot(aes(y=p_truth, x=t_spacing, fill=cyclic)) 

ggplot(data=sim.res) +
  geom_boxplot(aes(y=p_truth, x=l_spacing, fill=cyclic))

ggplot(sim.res, aes(y=entropy, x=sim_cond.f, color=trial.f)) +
  geom_point()




#########################################################
#Check the mean wait in the randomly spaced interventions:
df<-filter(df.ev, type=='action') %>% arrange(ppt, trial_type, time)
df$first <- (c(df$trial_type, 1)!=c(1, df$trial_type))[-nrow(df)]
df$first[1]<-T
df$first[nrow(df)]<-F

df$interval <- c(0,(df$time[-1] - df$time[-nrow(df)]))
df$interval[df$first==T]<-NA
head(df)
mean(df$interval, na.rm=T)
