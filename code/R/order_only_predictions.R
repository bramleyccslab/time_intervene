#Order only predictions - Not very effective in current implementation
#as does not assign one-to-one causation or prefer more recent events as causes

library(acl)
library(dplyr)
library(tidyr)
library(foreach)
library(doMC)


rm(list=ls())



load('../data/pilot_data.rdata')

df.tw<-filter(df.tw, practice==F)
df.ev<-filter(df.ev, practice==F)
df.be<-filter(df.be, practice==F)

order_likelihoods<-order_posts<-list()
for(pptix in 1:60)
{
  
  library(dplyr)
  library(tidyr)
  
  order_likelihood<- function(events, n_nodes)
  {
    if (n_nodes==3)
    {
      DBN<-DBN3 
    } else {
      DBN<-DBN4
    }
    
    #prior
    prior<-rep(1/dim(DBN)[3], dim(DBN)[3])
    li<-rep(1, dim(DBN)[3])
    
    effects<-filter(events, type=='effect')
    
    possibilities<-matrix(1, dim(DBN)[1], dim(DBN)[2]) - diag(dim(DBN)[1])
    
    if (nrow(effects)>0)
    {
      for (i in 1:nrow(effects))
      {
        exclude<-(1:dim(DBN)[1])[-unique(events$location[events$time<effects$time[i]])]
       possibilities[exclude, effects$location[i]]<-0
      }
      li[colSums(matrix(DBN[possibilities==0], sum(possibilities==0)))!=0]<-0
    }
    li
  }
  
  
  
  likelihoods<-posts<-list()
  
  for (ttix in 1:12)
  {
    likelihoods[[ttix]]<-posts[[ttix]]<-list()
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    n_nodes<-truth$n_nodes

    for (jix in 1:nrow(judgments))
    {
      events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
        select(time, location, type) %>% arrange(time)
      
      events<-filter(events, time<=judgments$time[jix])


        likelihoods[[ttix]][[jix]]<-order_likelihood(events=events,
                                                      n_nodes=n_nodes)
      
    
      posts[[ttix]][[jix]]<-likelihoods[[ttix]][[jix]]/sum(likelihoods[[ttix]][[jix]])
      
    }#Judgment
    
    
  }#Trial
  
  order_likelihoods[[pptix]]<-likelihoods
  order_posts[[pptix]]<-posts
}

save(file='../data/order_only_posteriors.rdata', order_likelihoods, order_posts)


#TODO: The issue is when a judgment is made after an action has occurred but before any effects...



#TODO do likelihood of edge vs no edge
#Think about working forward vs working backwards from effects
#Consider cut off at the longest extant delay for the condition, (overall max is 5512)
#Consider enumerating all the actual causations first
#Then calculating their likelihoods under all models then marginalising over them...
#If the number of possible causal pathways becomes too large then perhaps a greedy sample, or a random sample?
