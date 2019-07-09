library(acl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(heplots)
library(ez)

rm(list=ls())


#####################################
# Read in data

data_file = '../data/prolific_data.rdata' #'../data/cogsic_data.rdata'
load(data_file)

# Basic preps
# Make sure all upi variables have the same levels in the same order (dropping the ones that are no longer involved)
df.tw$upi<-factor(as.character(df.tw$upi), levels=as.character(df.sw$upi))
df.be$upi<-factor(as.character(df.be$upi), levels=as.character(df.sw$upi))
df.ev$upi<-factor(as.character(df.ev$upi), levels=as.character(df.sw$upi))
df.sw$upi<-factor(as.character(df.sw$upi), levels=as.character(df.sw$upi))

df.tw<-arrange(df.tw, upi, trial_type)

# Helper function to print the mean and standard deviation of a vector
msd <- function(vec, digits=1) {
  cat(paste('$', round(mean(vec), digits=digits),"\\pm", round(sd(vec), digits=digits), '$', sep=''))
}


##################
# Subject level
summary(df.sw)
sd(df.sw$age)
summary(lm(final_acc ~ delay_cond, data=df.sw)) # no significance
t.test(df.sw$final_acc[df.sw$delay_cond=='reliable'], df.sw$final_acc[df.sw$delay_cond=='unreliable'], var.equal=T) # very similar
df.sw %>% group_by(delay_cond) %>% summarise(mean(final_acc)) # 0.599, 0.549
df.tw %>% group_by(delay_cond, cyclic) %>% summarise(acc_final = mean(score)*12/54, acc_bonus = mean(bonus)*12/54) # cyclicity matters


# Calculate belief updates
df.be <- df.be %>% mutate(
  initial_judgment = (time == 0),
  final_judgment = 1:nrow(df.be) %in% c((which(time==0)-1)[-1], nrow(df.be)),
  judgment_number = 0
)

for (i in 2 : (nrow(df.be))) {
  if (df.be$initial_judgment[i] != T) {
    df.be$judgment_number[i] <- df.be$judgment_number[i - 1] + 1
  }
}

df.be <- mutate(df.be, n_links = apply(cbind((n1_dir!=0), (n2_dir!=0), (n3_dir!=0), (n4_dir!=0), (n5_dir!=0), (n6_dir!=0)), 1, sum, na.rm=T))
df.be$change = (c(df.be$n_links, 0) - c(0, df.be$n_links))[c(1:nrow(df.be))]
df.be$change[df.be$initial_judgment==T | df.be$final_judgment==T]<-NA

df<-filter(df.be, final_judgment==F & initial_judgment==F) %>% group_by(upi) %>%
  summarise(mean = mean(sum(initial_judgment==F & final_judgment==F)))
msd(df$mean/12) #$1.4\pm0.9$

# Proportion of judgments for N additions/subtractions
summary(as.factor(df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F])) / 
  length(df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F]) # -1: 4%

sum((df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F])>0) / 
  length(df.be$change[df.be$judgment_number!=1 & df.be$initial_judgment==F & df.be$final_judgment==F]) # 70.88%


# Does accuracy improve over the trial?
m0<-lmer(acc ~ 1 + (1|ppt) + (1|trial_type), REML = F, data = df.be[df.be$final_judgment==F & df.be$initial_judgment==F, ])
m1<-lmer(acc ~ judgment_number + (1|ppt) + (1|trial_type), REML = F, data = df.be[df.be$final_judgment==F & df.be$initial_judgment==F, ])
summary(m1)
anova(m0,m1) # Same as cogsci data, no difference


# Is there a difference from initial to final trial?
t.test(df.be$acc[df.be$judgment_number==1], df.be$acc[df.be$final_judgment==T], paired=T) # Same as cogsci_data, yes
msd(df.be$acc[df.be$judgment_number==1], digits=2) # $0.55\pm0.29$
msd(df.be$acc[df.be$final_judgment==T], digits=2) # $0.57\pm0.32$

# Do beliefs improve with time?
m0<-lmer(acc ~ 1 + (1|upi), REML=F, data=df.be[df.be$initial_judgment==F & df.be$final_judgment==F,])
m1<-lmer(acc ~ time + (1|upi), REML=F, data=df.be[df.be$initial_judgment==F & df.be$final_judgment==F,])
anova(m0,m1) # No

# Cyclic vs. acyclic - how many effects are experienced per trial on average? 
t.test(df.sw$n_effects.cy/6, df.sw$n_effects.ncy/6, paired=T) # Significantly differ
msd(df.sw$n_effects.ncy/6) # $4.4\pm0.9$
msd(df.sw$n_effects.cy/6) # $32.1\pm10.5$


#####################################

# Comparisons between between and within subs conditions-
accs <- df.tw %>% group_by(delay_cond, cyclic) %>% summarise(m_acc = mean(acc), sd_acc =  sd(acc))
accs # delay_cond does not really matter, cyclicity does.

ents <- df.tw %>% group_by(delay_cond, cyclic) %>% summarise(m_ent = mean(final_entropy, na.rm=T), sd_acc =  sd(final_entropy, na.rm=T))
ents # both matter



# Create a dataframe with two rows per participant for noncyclic/cyclic devices for repeated measures comparisons and analyses
df.swc <-df.sw %>% gather(cyclic, entropy, m_ent_ncy:m_ent_cy) %>%
  mutate(cyclic = factor(cyclic, levels = c('m_ent_ncy','m_ent_cy'), labels = c(F,T))) %>% 
  select(upi, delay_cond, cyclic, entropy) %>% arrange(upi, cyclic)
# Extract various measures from trialwise dataframe for this new dataframe
tmp <- df.tw %>% group_by(upi, cyclic) %>% summarise_each(funs='mean')
tmp2 <- df.tw %>% group_by(upi, cyclic) %>% summarise(score = sum(score))
df.swc$score <- tmp2$score
df.swc$accuracy <- tmp2$score/27
df.swc$igain<-tmp$igain
df.swc$igain.norm<-tmp$igain.norm
df.swc$n_ints = tmp$n_ints
df.swc$n_ef = tmp$n_ef


# Effects of delay condition and cyclicity on accuracy
m1 = ezANOVA(
  data = df.swc,
  dv = accuracy,
  wid = upi,
  within = .(cyclic),
  between = delay_cond
)
print(m1) # cyclic has a p<.05 star

#...and on entropy
df.swc$entropy[is.na(df.swc$entropy)]<-mean(df.swc$entropy, na.rm=T)
m3 = ezANOVA(
  data = df.swc
  , dv = entropy
  , wid = upi
  , within = .(cyclic)
  , between = delay_cond
)
print(m3) # All significant
summary(lm(accuracy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==F,])) # no significance
summary(lm(entropy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==F,])) # significant

#Relationships between accuracy and number of effects
m0<-lm(accuracy ~ delay_cond, data=df.swc[df.swc$cyclic==T,])
m1<-lm(accuracy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,])
anova(m0,m1) # ***
etasq(lm(accuracy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,]))

summary(lm(entropy ~ n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,])) # ***
summary(lmer(accuracy ~ entropy + cyclic + n_ef + (1|delay_cond) + (1|upi), data=df.swc, REML=F))
summary(lm(accuracy ~ entropy + delay_cond, data=df.swc[df.swc$cyclic==T,])) # *
summary(lm(accuracy ~ entropy + n_ef + delay_cond, data=df.swc[df.swc$cyclic==T,])) # n_ef***, deay_cond**
summary(lm(accuracy ~ entropy + delay_cond, data=df.swc[df.swc$cyclic==F,]))
summary(lm(accuracy ~ entropy + n_ef + delay_cond, data=df.swc[df.swc$cyclic==F,]))



#####################################
# Trial level

# correlation between conditions?
df<-df.tw %>% group_by(delay_cond, trial_type) %>% summarise(acc = mean(acc))
cor(df$acc[df$delay_cond=='reliable'], df$acc[df$delay_cond=='unreliable']) # 0.85


# Cyclic vs non cyclic within conditions
df.cy<-df.tw %>% filter(delay_cond=='reliable', cyclic==T) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
df.ncy<-df.tw %>% filter(delay_cond=='reliable', cyclic==F) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
t.test(df.cy$accuracy, df.ncy$accuracy, paired=T) # Significant

df.cy<-df.tw %>% filter(delay_cond=='unreliable', cyclic==T) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
df.ncy<-df.tw %>% filter(delay_cond=='unreliable', cyclic==F) %>% group_by(upi) %>% summarise(accuracy=sum(score)/27)
t.test(df.cy$accuracy, df.ncy$accuracy, paired=T) # Significant

summary(lm(acc ~ trial_type, data = df.tw)) #Main effect of trial type

##########################
# Interventions

# As predictors of judgement accuracy?
summary(df.sw)
summary(lm(final_acc ~ n_ints, data = df.sw))# Fewer ints -> higher acc, ***
summary(lm(acc ~ n_ints * cyclic, data = df.tw)) # No interaction with cycles

t.test(df.sw$n_ints.cy/6, df.sw$n_ints.ncy/6, paired=T) # 95 percent confident
msd(df.sw$n_ints.ncy/6) # $5.2\pm0.8$
msd(df.sw$n_ints.cy/6) # $4.1\pm1$

t.test(df.sw$n_effects.cy/6, df.sw$n_effects.ncy/6, paired=T) # 95 percent confident
msd(df.sw$n_effects.ncy/6) # $4.4\pm0.9$
msd(df.sw$n_effects.cy/6) # $32.1\pm10.5$


#############################
# Preference for parents

df.sw$int_pref_parent[1]<-mean(df.sw$int_pref_parent, na.rm=T)
m0<-lm(final_acc ~ delay_cond, data = df.sw)
m1<-lm(final_acc ~ delay_cond + int_pref_parent,data = df.sw) # Same as cogsci data - yes (**)
m2<-lm(final_acc ~ delay_cond * int_pref_parent,data = df.sw) # Different from cogsci, YES (**)
summary(m1)
summary(m2)

summary(lm(final_acc ~ int_pref_parent, data = df.sw)) # **: Intervene on parents -> much higher acc
summary(lm(final_acc ~ n_effects, data = df.sw)) # ***: More effects > worse accuracy




#############################
# Save up
save(file=data_file, df.be, df.ev, df.sw, df.tw, DBN3, DBN4, tgixs, get_ix)





