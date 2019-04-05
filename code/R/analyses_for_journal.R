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

load('../data/prolific_data.rdata')



#Arrange long dfs by participant then trial
df.tw<-arrange(df.tw, upi, trial_type)
# mod_preds<-arrange(mod_preds, upi, trial_type)
#Set a dataframe for final model predictions only (more useful)
# df.mod<-filter(mod_preds, final_judgment==T)

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
summary(lm(final_bonus ~ delay_cond, data=df.sw))
t.test(df.sw$final_acc[df.sw$delay_cond=='reliable'], df.sw$final_acc[df.sw$delay_cond=='unreliable'], var.equal=T)


df.sw %>% group_by(delay_cond) %>% summarise(mean(final_acc))

df.tw %>% group_by(delay_cond, cyclic) %>% summarise(acc_final = mean(score)*12/54, acc_bonus = mean(bonus)*12/54)
