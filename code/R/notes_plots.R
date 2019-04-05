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
#load('../data/cogsci_int_data_5sec.rdata')#The results of a set of interventions simulations using different rules


##############
#Judgments---

#Relationship between judgment accuracy and interventions:
ggplot(df.tw, aes(y=jitter(acc), x=max_p)) +
  geom_point() +
  geom_smooth() +
  facet_grid(n_nodes ~ cyclic) +
  labs(y='Jittered accuracy',
       x='Accuracy by Max(P|d;c) given interventions') +
  theme_bw() + 
  ggtitle('Faceted by number of nodes and cyclicity') +
ggsave('../../write_up/notes/figures/information_accuracy_relationship.pdf',width=5, height=5)

#Relationship between judgment accuracy and effects:
ggplot(df.tw, aes(y=jitter(acc), x=n_ef)) +
  geom_point() +
  geom_smooth() +
  facet_grid(n_nodes ~ cyclic) +
  labs(y='Jittered accuracy',
       x='Accuracy by number of effects experienced given interventions') +
  theme_bw() + 
  ggtitle('Faceted by number of nodes and cyclicity') +
  ggsave('../../write_up/notes/figures/n_effects_accuracy_relationship.pdf',width=5, height=5)

############################
#Normative information stuff
str(df.sw)
str(df.tw)
str(mod_preds)
df.sw$upi<-factor(as.character(df.sw$upi), levels=as.character(df.sw$upi))
df.tw$upi<-factor(as.character(df.tw$upi), levels=as.character(df.sw$upi))
mod_preds$upi<-factor(as.character(mod_preds$upi), levels=as.character(df.sw$upi))
df.tw<-arrange(df.tw, upi, trial_type)
mod_preds<-arrange(mod_preds, upi, trial_type)
mod_preds<-mutate(mod_preds, bayes.acc = bayes.perf / check)

df.tw$ideal_acc <- unlist(filter(mod_preds, final_judgment==T) %>% select(bayes.acc))
tmp<-df.tw %>% filter(!is.na(ideal_acc)) %>% group_by(upi) %>% summarise(m_igain = mean(igain, na.rm=T),
                                   m_igain.norm = mean(igain.norm, na.rm=T),
                                   m_ideal_acc = mean(ideal_acc, na.rm=T))
df.sw$m_igain<-tmp$m_igain
df.sw$m_igain.norm<-tmp$m_igain.norm
df.sw$m_ideal_acc<-tmp$m_ideal_acc

#Delay condition
ggplot(df.sw, aes(y=m_ent, x=delay_cond)) +
  geom_boxplot()
ggplot(df.sw, aes(y=m_igain, x=delay_cond)) +
  geom_boxplot()
ggplot(df.sw, aes(y=m_igain.norm, x=delay_cond)) +
  geom_boxplot()
ggplot(df.sw, aes(y=m_ideal_acc, x=delay_cond)) +
  geom_boxplot()

#Cyclicity
df.sw.c<-gather(df.sw, cyclic, m_ent_ncy_cy, m_ent_ncy:m_ent_cy) %>% mutate(cyclic.f = factor(recode(cyclic, "m_ent_ncy"=0,
                                                                                               "m_ent_cy"=1), levels=0:1),
                                                                            cyclic.n = as.numeric(cyclic.f))
ggplot(df.sw.c, aes(x=cyclic.f, y=m_ent_ncy_cy)) +
  geom_boxplot() +
  geom_point(aes(x=jitter(cyclic.n), colour=upi)) +
  labs(x='Cyclic?', y='Posterior entropy', colour = 'Participant') +
theme_bw() +
  theme(legend.position = 'none')

df.sw.c<-gather(df.sw, cyclic, final_acc_cy, m_ent_ncy:m_ent_cy) %>% mutate(cyclic.f = factor(recode(cyclic, "m_ent_ncy"=0,
                                                                                                     "m_ent_cy"=1), levels=0:1),
                                                                            cyclic.n = as.numeric(cyclic.f))
ggplot(df.sw.c, aes(x=cyclic.f, y=m_ent_ncy_cy)) +
  geom_boxplot() +
  geom_point(aes(x=jitter(cyclic.n), colour=upi)) +
  labs(x='Cyclic?', y='Posterior entropy', colour = 'Participant') +
  theme_bw() +
  theme(legend.position = 'none')


###############################################################
#Compare participant and heurisitc/bayesian accuracy by problem
str(mod_preds)

chosen_mod_names<- c('ppt','mostrecent', 'mostlikely', 'morelikely', 'morelikely.prune','bayes')
#chosen_mod_names<-c('ppt','mostrecent.boost', 'mostlikely.boost', 'morelikely.boost', 'morelikely.boost.prune','bayes')#mod_names

df.full<- mod_preds %>% filter(final_judgment==T) %>%
  mutate(trial.f<-factor(trial_type, levels=1:12),
         bayes.exist = NULL)
df<-df.full %>% select(contains('perf'))
names(df)<-substr(names(df), 1, nchar(names(df))-5)#Drop the .pref

tmp<-df.full$n_nodes
tmp[tmp==4]<-6
df<-sweep(df, 1, tmp,  '/')

df<-df %>% select(which(names(df)%in%chosen_mod_names))

df$trial<-df.full$trial.f
df$upi<-df.full$upi
df$delay_cond<-df.full$delay_cond
df.l<-df %>% gather(model, acc, mostrecent:bayes) %>% #mostrecent
mutate(model = factor(model),
       human = model=='ppt',
       normative = model=='bayes',
       mod_kind = factor(human - normative, levels=-1:1, labels=c('Rational','Incremental','Participants')),
       size = (human + 1) * 3)

#Based on accuracy per edge
ggplot(df.l, aes(x=trial, y=acc, shape=model, colour=mod_kind)) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .75), size=4) + 
  stat_summary(fun.data = mean_cl_normal,geom = "errorbar", fun.args = list(mult = 1),
               position = position_dodge(width = .75)) +
  theme_bw() +
  labs(x='Problem', y='Accuracy') + 
  scale_colour_manual(values=c('red','grey','black')) +
  ggsave('../../write_up/notes/figures/ppt_mod_performance_trial.pdf', width=12, height=4)

#Based on getting it completely correct
ggplot(df.l, aes(x=trial, y=as.numeric(acc==1), shape=model, colour=mod_kind)) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .75), size=4) + 
  stat_summary(fun.data = mean_cl_normal,geom = "errorbar", fun.args = list(mult = 1),
               position = position_dodge(width = .75)) +
  theme_bw() +
  labs(x='Problem', y='Accuracy') + 
  scale_colour_manual(values=c('red','grey','black')) +
  ggsave('../../write_up/notes/figures/ppt_mod_pcorrect_trial.pdf', width=12, height=4)




#####################
#Intervention timings:

#distribution of inter-intervention delays
df1<-df.ev %>% arrange(ppt, trial_type, time) %>% select(ppt, upi, learn_cond, trial_type, time, type, n_nodes) %>%
  mutate(learn_cond = as.character(learn_cond),
         upi = as.character(upi),
         time_cond = 'Participants')
df2<-df.sim %>% arrange(learn_cond, ppt, trial_type, time) %>% select(ppt, upi, learn_cond, trial_type, time, type, n_nodes) %>%
  mutate(time_cond = as.character(factor(learn_cond%%3, levels=c(1,2,0), labels=c('Rand','Even','Reactive'))),
         learn_cond = as.character(learn_cond),
         upi = as.character(upi)
  )

df.ev.co<-rbind(df1, df2)
df.ev.co<-mutate(df.ev.co, time_cond.f = factor(time_cond, levels = c('Participants', 'Rand','Even','Reactive')))

df<-filter(df.ev.co, type=='action')

df$first <- (c(df$trial_type, 1)!=c(1, df$trial_type))[-nrow(df)]
df$first[1]<-T
df$first[nrow(df)]<-F

df$interval <- c(0,(df$time[-1] - df$time[-nrow(df)]))
df$interval[df$first==T]<-NA

ggplot(df[!is.na(df$interval),], aes(x=interval)) +
  geom_histogram(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), bins=10, color='black',fill='grey') +
  labs(y='Proportion', x='Interval between interventions') +
  theme_bw() +
  facet_wrap(~ time_cond.f, ncol=4) +
  ggsave('../../write_up/notes/figures/interintervention_intervals.pdf', width=10, height=4)
#Now do the same for gap between latest intervention and next action

df.ev.co$first <- (c(df.ev.co$trial_type, 1)!=c(1, df.ev.co$trial_type))[-nrow(df)]
df.ev.co$first[1]<-T
df.ev.co$first[nrow(df.ev.co)]<-F
head(df.ev.co)

intervals_nonfirst<-df.ev.co$time[which(df.ev.co$type=='action' & df.ev.co$first==F)] - df.ev.co$time[which(df.ev.co$type=='action' & df.ev.co$first==F)-1]
intervals_nonfirst[intervals_nonfirst<0]<-NA
df.ev.co$interval_prev_event<- NA
df.ev.co$interval_prev_event[df.ev.co$type=='action' & df.ev.co$first==F]<-intervals_nonfirst
tail(df.ev.co, 50)

ggplot(df.ev.co[!is.na(df.ev.co$interval_prev_event),], aes(x=interval_prev_event)) +
  geom_histogram(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]), bins=10, color='black',fill='grey') +
  labs(y='Proportion', x='Intervals between intervention and preceeding event') +
  theme_bw() +
  facet_wrap(~ time_cond.f, ncol=4) +
  ggsave('../../write_up/notes/figures/reaction_intervals.pdf', width=10, height=4)

# upis<-df.sw$upi
# for( i in length(upis))
# {
#   for (j in 1:12)
#   {
#     tmp<-unlist(filter(df, upi==upis[i], trial_type==j) %>% arrange(time) %>% select(time))
#     df.tw$int_interval<-tmp[-1] - tmp[-length(tmp)]
#   }
# 
# }
#distribution of waits from previous event to next intervention

########################
#Intervention locations:


####################
#Intervention strategies----
start_ent3<-shannon_entropy(rep(1/dim(DBN3)[3], dim(DBN3)[3]), log_type='bits')
start_ent4<-shannon_entropy(rep(1/dim(DBN4)[3], dim(DBN4)[3]), log_type='bits')
df.co$start_ent<-start_ent3
df.co$start_ent[df.co$n_nodes==4]<-start_ent4
df.co<-mutate(df.co, igain = start_ent - entropy,
              igain.norm = igain/start_ent)


max(df.co$entropy)
#Comparison of performance with humans
ggplot(data=df.co, aes(y=igain.norm, x=location, color=timing)) +
  #geom_boxplot(size=1) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .75), size=5) + 
  stat_summary(fun.data = mean_cl_normal,geom = "errorbar", fun.args = list(mult = 1), position = position_dodge(width = .75)) +
  scale_fill_grey(start=.4, end=1) +
  labs(y='Normalised information gain', x='Location', colour='Timing') +
  theme_bw() +
  #ggtitle('Intervention simulations') +
  ggsave('../../write_up/notes/figures/int_sim_performance.pdf', width=6, height=4)

#Broken down by problem
df.co<-df.co %>% mutate(trial.f = factor(trial))

ggplot(data=df.co, aes(y=igain.norm, shape=location, color=timing, x=trial.f)) +
  #geom_boxplot(size=1) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .75), size=5) + 
  stat_summary(fun.data = mean_cl_normal,geom = "errorbar", fun.args = list(mult = 1), position = position_dodge(width = .75)) +
  scale_fill_grey(start=.4, end=1) +
  labs(y='Normalised information gain', x='Problem') +
  theme_bw() +
  #ggtitle('Intervention simulation by trial') +
  ggsave('../../write_up/notes/figures/int_sim_performance_trial.pdf', width=12, height=4)

ggplot(data=df.co, aes(y=igain.norm, shape=location, color=timing, x=cyclic)) +
  #geom_boxplot(size=1) +
  stat_summary(fun.y = mean, geom = "point", position = position_dodge(width = .75), size=5) + 
  stat_summary(fun.data = mean_cl_normal,geom = "errorbar", fun.args = list(mult = 1), position = position_dodge(width = .75)) +
  scale_fill_grey(start=.4, end=1) +
  labs(y='Normalised information gain', x='Cyclic?') +
  theme_bw() +
  #ggtitle('Intervention simulation by trial') +
  ggsave('../../write_up/notes/figures/int_sim_performance_cyclic.pdf', width=12, height=4)

#Correlation between number of events and information

ggplot(df.co, aes(y=igain.norm, x=nev, colour=cyclic)) +
  geom_point(alpha=.2) +
  geom_smooth(method='glm',formula=y~x, method.args = list(family = "binomial"), se = TRUE) +
  #geom_smooth(method='lm',formula=y~x, se = TRUE) +
  theme_bw() +
  theme(legend.position = 'none') + 
  ggsave('../../write_up/notes/figures/nev_igain_correlation.pdf', width=5, height=5)

