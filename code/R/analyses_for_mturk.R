library(acl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(heplots)
library(ez)

rm(list=ls())


#####################################
#Read in data----

load('../data/cogsci_data.rdata')#The trialwise, beliefwise, eventwise, and subjectwise data
load('../data/mf_results.rdata')#The results of the belief model simulations given the evidence seen by participants: Rational + heuristics
#TODO check/subset mod_preds?
#load('../data/cogsci_int_data_5sec.rdata')#The results of a set of interventions simulations using different rules


#Make sure all upi variables have the same levels in the same order (dropping the ones that are no longer involved)
df.tw$upi<-factor(as.character(df.tw$upi), levels=as.character(df.sw$upi))
df.be$upi<-factor(as.character(df.be$upi), levels=as.character(df.sw$upi))
df.ev$upi<-factor(as.character(df.ev$upi), levels=as.character(df.sw$upi))
mod_preds$upi<-factor(as.character(mod_preds$upi), levels=as.character(df.sw$upi))
df.sw$upi<-factor(as.character(df.sw$upi), levels=as.character(df.sw$upi))#(Has to go last)

#Arrange long dfs by participant then trial
df.tw<-arrange(df.tw, upi, trial_type)
mod_preds<-arrange(mod_preds, upi, trial_type)
#Set a dataframe for final model predictions only (more useful)
df.mod<-filter(mod_preds, final_judgment==T)

#A function to print the mean and standard deviation of a vector
msd<- function(vec, digits=1)
{
  cat(paste('$', round(mean(vec), digits=digits),"\\pm", round(sd(vec), digits=digits), '$', sep=''))
}


##################
#Subject level----

summary(df.sw)
sd(df.sw$age)
summary(lm(final_acc ~ delay_cond, data=df.sw))
t.test(df.sw$final_acc[df.sw$delay_cond=='between'], df.sw$final_acc[df.sw$delay_cond=='within'], var.equal=T)

df.sw %>% group_by(delay_cond) %>% summarise(mean(final_score))


#How many belief reports?----

df.be<-df.be %>% mutate(initial_judgment = time==0,
                        final_judgment = 1:nrow(df.be) %in% c((which(time==0)-1)[-1], nrow(df.be)))

df.be$judgment_number<-0
for (i in 1:(nrow(df.be)-1))
{
  if (df.be$initial_judgment[i+1]!=T)
  {
    df.be$judgment_number[i+1] <-df.be$judgment_number[i] + 1
  }
}

df.be<-mutate(df.be, n_links =  apply(cbind((n1_dir!=0), (n2_dir!=0), (n3_dir!=0), (n4_dir!=0), (n5_dir!=0), (n6_dir!=0)), 1, sum, na.rm=T))
df.be$change =  (c(df.be$n_links, 0) - c(0, df.be$n_links))[c(1:nrow(df.be))]
df.be$change[df.be$initial_judgment==T | df.be$final_judgment==T]<-NA

df<-filter(df.be, final_judgment==F & initial_judgment==F) %>% group_by(upi) %>%
  summarise(mean = mean(sum(initial_judgment==F & final_judgment==F)))
msd(df$mean/12)

#Does accuracy improve over the trial?
m0<-lmer(acc ~ 1 + (1|ppt) + (1|trial_type), REML = F, data = df.be[df.be$final_judgment==F & df.be$initial_judgment==F,])
m1<-lmer(acc ~ judgment_number + (1|ppt) + (1|trial_type), REML = F, data = df.be[df.be$final_judgment==F & df.be$initial_judgment==F,])
summary(m1)
anova(m0,m1) #No difference in accuracy by judgment number other things being equal

#Is there a difference from initial to final trial?
t.test(df.be$acc[df.be$judgment_number==1], df.be$acc[df.be$final_judgment==T], paired=T) #Yes
msd(df.be$acc[df.be$judgment_number==1], digits=2)
msd(df.be$acc[df.be$final_judgment==T], digits=2)

#Number of additions and subtractions per update?
summary(as.factor(df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F])) / 
  length(df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F])#Proportion of judgments for N additions/subtractions

sum((df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F])>0) / 
  length(df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F])

n_j_per_t<-(filter(df.be, final_judgment==F & initial_judgment==F) %>% group_by(upi, trial_type) %>% summarise(length(time)))[[3]]

seq(1, n_j_per_t)

#Do beliefs improve with time?----
m0<-lmer(acc ~ 1 + (1|upi), REML=F, data=df.be[df.be$initial_judgment==F & df.be$final_judgment==F,])
m1<-lmer(acc ~ time + (1|upi), REML=F, data=df.be[df.be$initial_judgment==F & df.be$final_judgment==F,])
anova(m0,m1)


#How many effects are experienced per trial on average? comparing across cyclic and acyclic----
#1.2914      1     0.2558
t.test(df.sw$n_effects.cy/6, df.sw$n_effects.ncy/6, paired=T)
msd(df.sw$n_effects.ncy/6)
msd(df.sw$n_effects.cy/6)





#############################################
#Comparisons between between and within subs conditions----

accs<-df.tw %>% group_by(delay_cond, cyclic) %>% summarise(m_acc = mean(acc), sd_acc =  sd(acc))
accs

ents<-df.tw %>% group_by(delay_cond, cyclic) %>%
  summarise(m_ent = mean(final_entropy, na.rm=T), sd_acc =  sd(final_entropy, na.rm=T))
ents
ents<-df.tw %>% group_by(cyclic) %>%
  summarise(m_ent = mean(final_entropy, na.rm=T), sd_acc =  sd(final_entropy, na.rm=T))
ents

#Add ideal accuracy to the trialwise dataframe
df.mod<-mutate(df.mod, bayes.acc = bayes.perf / check)
df.tw$ideal_acc <- NA
df.tw$ideal_acc<-df.mod$bayes.acc

df.tw$info_per_effect<-df.tw$igain.norm/df.tw$n_ef
df.tw$info_per_effect[is.nan(df.tw$info_per_effect)]<-0

#Add information gain and ideal accuracy to the subjectwise dataframe
tmp<-df.tw %>% filter(!is.na(ideal_acc)) %>% group_by(upi) %>%
  summarise(m_igain = mean(igain, na.rm=T),
            m_igain.norm = mean(igain.norm, na.rm=T),
            m_ideal_acc = mean(ideal_acc, na.rm=T),
            m_info_per_effect = mean(info_per_effect, na.rm=T))
df.sw$m_igain<-tmp$m_igain
df.sw$m_igain.norm<-tmp$m_igain.norm
df.sw$m_ideal_acc<-tmp$m_ideal_acc
df.sw$m_info_per_effect<-tmp$m_info_per_effect
names(df.sw)

#Create a dataframe with two rows per participant for noncyclic/cyclic devices for repeated measures comparisons and analyses
df.swc<-df.sw %>% gather(cyclic, entropy, m_ent_ncy:m_ent_cy) %>%
  mutate(cyclic = factor(cyclic, levels = c('m_ent_ncy','m_ent_cy'), labels = c(F,T))) %>% 
  select(upi, delay_cond, cyclic, entropy) %>% arrange(upi, cyclic)
#Extract various measures from trialwise dataframe for this new dataframe
tmp<-df.tw %>% group_by(upi, cyclic) %>% summarise_each(funs='mean')
tmp2<-df.tw %>% group_by(upi, cyclic) %>% summarise(score = sum(score))
df.swc$score <- tmp2$score
df.swc$accuracy <- tmp2$score/27
df.swc$igain<-tmp$igain
df.swc$igain.norm<-tmp$igain.norm
df.swc$ideal_accuracy = tmp$ideal_acc
df.swc$n_ints = tmp$n_ints
df.swc$n_ef = tmp$n_ef
df.swc$info_per_effect = tmp$info_per_effect

head(df.swc)

#Effects of delay condition and cyclicity on accuracy
m1 = ezANOVA(
  data = df.swc
  , dv = accuracy
  , wid = upi
  , within = .(cyclic)
  , between = delay_cond
)
print(m1)


#Effects of delay condition and cyclicity on ideal accuracy
df.swc$ideal_accuracy[is.na(df.swc$ideal_accuracy)]<-mean(df.swc$ideal_accuracy, na.rm=T)
m2 = ezANOVA(
  data = df.swc
  , dv = ideal_accuracy
  , wid = upi
  , within = .(cyclic)
  , between = delay_cond
)
print(m2)
#...and on entropy
df.swc$entropy[is.na(df.swc$entropy)]<-mean(df.swc$entropy, na.rm=T)
m3 = ezANOVA(
  data = df.swc
  , dv = entropy
  , wid = upi
  , within = .(cyclic)
  , between = delay_cond
)
print(m3)
summary(lm(accuracy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==F,]))
summary(lm(entropy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==F,]))

#Relationships between accuracy and number of effects
m0<-lm(accuracy ~ delay_cond, data=df.swc[df.swc$cyclic==T,])
m1<-lm(accuracy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,])
anova(m0,m1)
etasq(lm(accuracy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,]))


summary(lm(entropy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,]))

summary(lmer(accuracy ~ entropy + cyclic + n_ef + (1|delay_cond) + (1|upi), data=df.swc, REML=F))

summary(lm(accuracy ~ entropy + delay_cond, data=df.swc[df.swc$cyclic==T,]))
summary(lm(accuracy ~ entropy + n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,]))
summary(lm(accuracy ~ entropy + delay_cond, data=df.swc[df.swc$cyclic==F,]))
summary(lm(accuracy ~ entropy + n_ef + delay_cond, data=df.swc[df.swc$cyclic==F,]))

#Info per effect
summary(lm(final_acc ~ m_info_per_effect + delay_cond, data=df.sw))

#Correlate info and accuracy across trials
tmp<-df.tw %>% group_by(trial_type) %>% summarise(m_acc = mean(acc),
                                             m_ideal_acc = mean(ideal_acc, na.rm=T))
cor(tmp$m_acc[1:6], tmp$m_ideal_acc[1:6])
cor(tmp$m_acc[7:12], tmp$m_ideal_acc[7:12])
# mod_preds$delay_cond<-df.be$delay_cond
# mod_preds$cyclic<-as.numeric(mod_preds$trial_type)>6
# mod_preds$bayes.acc<-mod_preds$bayes.perf/mod_preds$check
# tmp<-mod_preds %>% 
#   filter(final_judgment==T) %>%
#   group_by(upi) %>% summarise(mba=mean(bayes.acc))
# df.sw$bayes.acc<-tmp$mba
# tmp<-mod_preds %>% 
#   filter(final_judgment==T) %>%
#   group_by(upi, cyclic) %>% summarise(mba=mean(bayes.acc))
# df.sw$bayes_acc.ncy<-tmp$mba[tmp$cyclic==F]
# df.sw$bayes_acc.cy<-tmp$mba[tmp$cyclic==T]
# df.sw$ppt<-1:40
# 
# summary(lm(bayes.acc ~ delay_cond, data=df.sw))#Yes
# 
# t.test(df.sw$bayes_acc.cy, df.sw$bayes_acc.ncy, paired=T)#No
# summary(lm(bayes_acc.ncy ~ delay_cond, data=df.sw))#Yes
# summary(lm(bayes_acc.cy ~ delay_cond, data=df.sw))#Yes

# df.rm<-df.sw %>% filter(ppt!=33) %>% gather(cyclic, bayes_acc, c(bayes_acc.ncy, bayes_acc.cy))
# 
# ezANOVA(data=df.rm, dv=.(ppt), within=.(bayes_acc), wid=.(cyclic), between = (delay_cond), detailed=TRUE)


################
#Trial level----

#correlation between conditions?----

df<-df.tw %>% group_by(delay_cond, trial_type) %>% summarise(acc = mean(acc))
cor(df$acc[df$delay_cond=='reliable'], df$acc[df$delay_cond=='unreliable'])


#Cyclic vs non cyclic within conditions----
df.cy<-df.tw %>% filter(delay_cond=='reliable', cyclic==T) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
df.ncy<-df.tw %>% filter(delay_cond=='reliable', cyclic==F) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
t.test(df.cy$accuracy, df.ncy$accuracy, paired=T)


df.cy<-df.tw %>% filter(delay_cond=='unreliable', cyclic==T) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
df.ncy<-df.tw %>% filter(delay_cond=='unreliable', cyclic==F) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
t.test(df.cy$accuracy, df.ncy$accuracy, paired=T)

summary(lm(acc ~ trial_type, data = df.tw)) #Main effect of trial type

m0<-lm(acc ~ delay_cond + cyclic, data = df.tw)
m1<-lm(acc ~ delay_cond * cyclic, data = df.tw)
anova(m0,m1)#NS
summary(m1)


m0<-lm(acc ~ delay_cond + trial_type, data = df.tw)
m1<-lm(acc ~ delay_cond * trial_type, data = df.tw)
anova(m0,m1)
summary(m1)#NS

m0<-lm(acc ~ delay_cond + trial_type, data = df.tw[df.tw$delay_cond!='reliable',])
m1<-lm(acc ~ delay_cond * trial_type, data = df.tw[df.tw$delay_cond!='reliable',])
anova(m0,m1)
summary(m1)#NS


#Look closer at loop-out devices----
df<-df.tw  %>% filter(trial_type==9, delay_cond=='reliable')%>% group_by(delay_cond) %>% select(delay_cond, n1_dir:n3_dir) 
summary(factor(df$n1_dir))
summary(factor(df$n2_dir))
summary(factor(df$n3_dir))
df<-df.tw  %>% filter(trial_type==9, delay_cond=='within')%>% group_by(delay_cond) %>% select(delay_cond, n1_dir:n3_dir) 
rbind(summary(factor(df$n1_dir)),
      summary(factor(df$n2_dir)),
      summary(factor(df$n3_dir)))
df<-df.tw  %>% filter(trial_type==9, delay_cond=='between')%>% group_by(delay_cond) %>% select(delay_cond, n1_dir:n3_dir) 
rbind(summary(factor(df$n1_dir)),
      summary(factor(df$n2_dir)),
      summary(factor(df$n3_dir)))#Nothing interesting

# tw_by_cond<-df.tw %>% group_by(delay_cond)
# summarise(tw_by_cond, mean(acc))
# summarise(tw_by_cond, sd(acc))

# summary(lm(acc ~ cyclic, data=df.tw))
# summary(lm(acc ~ delay_cond:cyclic, data=df.tw))
# summary(lm(acc ~ delay_cond*cyclic, data=df.tw[df.tw$delay_cond!='reliable',])) #Nope
# summary(lm(acc ~ trial_type*delay_cond, data=df.tw[df.tw$delay_cond!='reliable',])) #Nope

mod_preds$delay_cond<-df.be$delay_cond
mod_preds$cyclic<-as.numeric(mod_preds$trial_type)>6

mod_preds_by_cond<-mod_preds %>% filter(final_judgment==T, bayes.exist==T, !is.na(bayes)) %>% group_by(delay_cond)
summarise(mod_preds_by_cond, mean(bayes.perf)/mean(check))
mod_preds_by_cond_cyclic<-mod_preds %>% filter(final_judgment==T, bayes.exist==T, !is.na(bayes)) %>% group_by(delay_cond, cyclic)
summarise(mod_preds_by_cond_cyclic, mean(bayes.perf)/mean(check))

mod_preds$bayes.cor
mod_preds$bayes.perf


summarise(mod_preds_by_cond, mean(mostrecent.perf)/mean(check))
summarise(mod_preds_by_cond_cyclic, mean(mostrecent.perf)/mean(check))
summarise(mod_preds_by_cond, mean(mostlikely.perf)/mean(check))
summarise(mod_preds_by_cond_cyclic, mean(mostlikely.perf)/mean(check))
summarise(mod_preds_by_cond, mean(morelikely.perf)/mean(check))
summarise(mod_preds_by_cond_cyclic, mean(morelikely.perf)/mean(check))

tmp<-mod_preds %>% mutate(mostrecent.perf = mostrecent.perf/check,
                          mostlikely.perf=mostlikely.perf/check,
                          morelikely.perf = morelikely.perf/check) %>%
  group_by(ppt) %>% summarise_each(funs='mean')
tmp$delay_cond<-df.sw$delay_cond
t.test(tmp$mostrecent.perf[tmp$delay_cond=='reliable'],
       tmp$mostrecent.perf[tmp$delay_cond=='unreliable'], var.equal=T)
t.test(tmp$mostlikely.perf[tmp$delay_cond=='reliable'],
       tmp$mostlikely.perf[tmp$delay_cond=='unreliable'], var.equal=T)
t.test(tmp$morelikely.perf[tmp$delay_cond=='reliable'],
       tmp$morelikely.perf[tmp$delay_cond=='unreliable'], var.equal=T)

tmp<-mod_preds  %>% mutate(bayes.perf = bayes.perf/check,
                           mostrecent.perf = mostrecent.perf/check,
                           mostlikely.perf=mostlikely.perf/check,
                           morelikely.perf = morelikely.perf/check) %>%
  group_by(ppt, cyclic) %>% summarise_each(funs='mean')

t.test(tmp$mostrecent.perf[tmp$cyclic==F],
       tmp$mostrecent.perf[tmp$cyclic==T], paired=T)
t.test(tmp$mostlikely.perf[tmp$cyclic==F],
       tmp$mostlikely.perf[tmp$cyclic==T], paired=T)
t.test(tmp$morelikely.perf[tmp$cyclic==F],
       tmp$morelikely.perf[tmp$cyclic==T], paired=T)
t.test(tmp$bayes.perf[tmp$cyclic==F],
       tmp$bayes.perf[tmp$cyclic==T], paired=T)

##########################
#Interventions----

#As predictors of judgement accuracy?----
names(df.tw)

summary(df.sw)

summary(lm(final_acc ~ n_ints, data = df.sw))#Fewer ints -> higher acc

summary(lm(acc ~ n_ints*cyclic, data = df.tw))#No interaction with cycles

t.test(df.sw$n_ints.cy/6, df.sw$n_ints.ncy/6, paired=T)
msd(df.sw$n_ints.ncy/6)
msd(df.sw$n_ints.cy/6)

t.test(df.sw$n_effects.cy/6, df.sw$n_effects.ncy/6, paired=T)
msd(df.sw$n_effects.ncy/6)
msd(df.sw$n_effects.cy/6)

#############################
#Preference for parents----
df.sw$int_pref_parent[1]<-mean(df.sw$int_pref_parent, na.rm=T)
m0<-lm(final_acc ~ delay_cond, data = df.sw)
m1<-lm(final_acc ~ delay_cond + int_pref_parent,data = df.sw)#Yes
m2<-lm(final_acc ~ delay_cond * int_pref_parent,data = df.sw)#No interaction with condition
anova(m0,m1)
summary(m1)
etasq(m1)
summary(m2)

#Affecting entropy?
summary(lm(m_ent ~ delay_cond + int_pref_parent,data = df.sw))#No

msd(df.sw$int_pref_parent.ncy)
t.test(df.sw$int_pref_parent.ncy, mu=1)

apply(df.tw %>% filter(trial_type==6, delay_cond=='within') %>% select(n_ac1:n_ac4), 2, msd)
msd(unlist(df.tw %>% filter(trial_type==6, delay_cond=='within') %>% select(n_ac2:n_ac4)))

summary(lm(final_acc ~ int_pref_parent, data = df.sw)) #Intervene on parents -> much higher acc
summary(lm(final_acc ~ n_effects, data = df.sw)) #More effects > worse accuracy

#################################
#Time spacing and variability----

#Intervention spacing
msd(df.sw$int_space.mean/1000)
msd(df.sw$int_space.mean[df.sw$delay_cond=='reliable']/1000)
msd(df.sw$int_space.mean[df.sw$delay_cond=='unreliable']/1000)

df.sw$int_space.cv<-(df.sw$int_space.sd/df.sw$int_space.mean) 

m0<-lm(final_acc ~ delay_cond + n_ints, data = df.sw)
m1<-lm(final_acc ~ delay_cond + n_ints + int_space.mean, data = df.sw)
summary(m1) #More spaced > more accuracy STRONG
etasq(m1)
anova(m0,m1)

summary(lm(final_acc ~ n_ints + delay_cond * int_space.mean, data = df.sw)) #No interaction

#summary(lm(final_acc ~ delay_cond + int_space.cv, data = df.sw)) #Less variable > more accurate
#etasq(lm(final_acc ~ delay_cond + int_space.cv,data = df.sw))
m0<-lm(final_acc ~ delay_cond + n_ints, data = df.sw)
m1<-lm(final_acc ~ delay_cond + n_ints + int_space.cv, data = df.sw)
summary(m1) #less variable > more accurate
etasq(m1)
anova(m0,m1)

m2 <-lm(final_acc ~ n_ints + delay_cond * int_space.cv, data = df.sw) #Small interaction
summary(m2)
etasq(m2)
anova(m1,m2)

#Previous event to intervention spacing
msd(df.sw$int_space_event.mean/1000)
msd(df.sw$int_space_event.mean[df.sw$delay_cond=='reliable']/1000)
msd(df.sw$int_space_event.mean[df.sw$delay_cond=='unreliable']/1000)
t.test(df.sw$int_space_event.mean[df.sw$delay_cond=='reliable']/1000,
       df.sw$int_space_event.mean[df.sw$delay_cond=='unreliable']/1000)#Wrong direction and luckily nonsignificant

df.sw$int_space_event.cv<-(df.sw$int_space_event.sd/df.sw$int_space_event.mean) 

m0<-lm(final_acc ~ delay_cond + n_ints, data = df.sw)
m1<-lm(final_acc ~ delay_cond + n_ints + int_space_event.mean, data = df.sw)
summary(m1) #More spaced > more accurate
etasq(m1)
anova(m0,m1)

summary(lm(final_acc ~ n_ints + delay_cond * int_space_event.mean, data = df.sw)) #No interaction

m0<-lm(final_acc ~ delay_cond + n_ints, data = df.sw)
m1<-lm(final_acc ~ delay_cond + n_ints + int_space_event.cv, data = df.sw)
summary(m1) #less variable > more accurate
etasq(m1)
anova(m0,m1)

m2 <-lm(final_acc ~ n_ints + delay_cond * int_space_event.cv, data = df.sw) #No interaction
summary(m2)

#And with posterior information/entropy
m0<-lm(m_ent ~ delay_cond + n_ints, data = df.sw)
m1<-lm(m_ent ~ delay_cond + n_ints + int_space.mean, data = df.sw)
summary(m1)
etasq(m1)
anova(m0,m1)
m1<-lm(m_ent ~ delay_cond + n_ints + int_space_event.mean, data = df.sw)
summary(m1)
etasq(m1)
anova(m0,m1)

summary(lm(m_ent ~ delay_cond + n_ints + int_space.cv, data = df.sw))
summary(lm(m_ent ~ delay_cond + n_ints + int_space_event.cv, data = df.sw))

# summary(lm(final_acc ~ delay_cond + int_space.mean + df.sw$int_space.cv, data = df.sw))
# 
# summary(lm(final_acc ~ delay_cond + int_space.mean * df.sw$int_space.cv, data = df.sw)) #Combining variability with spacing
# 
# #Check for individual significance
# m1<-lm(final_acc ~ delay_cond + int_space.mean , data = df.sw)
# m2<-lm(final_acc ~ delay_cond + df.sw$int_space.cv, data = df.sw)
# m3<-lm(final_acc ~ delay_cond + int_space.mean + df.sw$int_space.cv, data = df.sw)
# anova(m1, m3)
# anova(m2, m3)
# etasq(lm(final_acc ~ delay_cond + int_space.mean + df.sw$int_space.cv, data = df.sw)) #Getting the partial regression coefficients
# 
# summary(lm(final_acc ~ prop_nodes_tested, data = df.sw)) #No relationship
# 
# summary(lm(final_acc ~ int_space.mean * int_pref_parent, data = df.sw)) #A combination model
# 
# summary(lm(n_ints ~ delay_cond, data=df.sw[df.sw$delay_cond!='reliable',]))#NS
# 
# summary(lm(int_space.mean ~ delay_cond, data=df.sw))#NS
# summary(lm(int_space_event.mean ~ delay_cond, data=df.sw))#NS
# summary(lm(prop_nodes_tested ~ delay_cond, data=df.sw))#NS
# summary(lm(n_ints ~ delay_cond, data=df.sw))#NS
# 
# head(df.tw)
# df_sw_by_cond<-group_by(df.sw, delay_cond)
# tmp<-summarise_each(df_sw_by_cond, funs(mean))



#As related to posterior entropy----

head(df.tw)
summary(lm(acc ~ upi + igain.norm*cyclic, data=df.tw))#No

head(df.sw)
summary(lm(final_acc ~  m_ent, data=df.sw))#No
summary(lm(final_acc ~ n_effects + m_ent + delay_cond, data=df.sw)) #No
summary(lm(final_acc ~ n_effects * m_ent * delay_cond, data=df.sw)) #No

df.sw$int_space.cv<-(df.sw$int_space.sd/df.sw$int_space.mean)

summary(lm(m_ent ~ delay_cond, data = df.sw)) #No
summary(lm(m_ent ~ delay_cond + int_space.mean, data = df.sw)) #No
summary(lm(m_ent ~ delay_cond * int_space.mean, data = df.sw)) #No interaction
summary(lm(m_ent ~ delay_cond + int_space.cv, data = df.sw)) #No
summary(lm(m_ent ~ delay_cond + int_space.mean + df.sw$int_space.cv, data = df.sw)) #No

summary(lm(m_ent_ncy ~ int_space.mean + delay_cond, data = df.sw)) #Marginal positive
summary(lm(m_ent_cy ~ int_space.mean + delay_cond, data = df.sw)) #Yes negative!

summary(lm(m_ent_ncy ~ int_space.cv + delay_cond, data = df.sw)) #No
summary(lm(m_ent_cy ~ df.sw$int_space.cv + delay_cond, data = df.sw)) #Yes
#Larger spacing (and less variance??) improve entropy for cyclic structures

summary(lm(m_ent3 ~ int_space.mean, data = df.sw)) #Yes
summary(lm(m_ent3 ~ df.sw$int_space.cv , data = df.sw)) #No
summary(lm(m_ent4 ~ int_space.mean, data = df.sw)) #Yes
summary(lm(m_ent4 ~ df.sw$int_space.cv , data = df.sw)) #No

summary(lm(m_ent ~ delay_cond + int_pref_parent, data=df.sw))

df.cy<-df.tw %>% filter(cyclic==T) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
df.ncy<-df.tw %>% filter(cyclic==F) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)

summary(lm(final_acc ~ m_ent, data = df.sw))
summary(lm(final_acc ~ delay_cond + m_ent, data = df.sw))
summary(lm(final_acc ~ delay_cond * m_ent, data = df.sw))
etasq(lm(final_acc ~ delay_cond + m_ent, data = df.sw))
