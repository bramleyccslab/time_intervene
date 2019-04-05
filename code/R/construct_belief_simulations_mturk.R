##############################################
#Simulated learners using participants' data
#############################################


library(acl)
library(dplyr)
library(tidyr)


rm(list=ls())


load('../data/pilot_data.rdata')
df.tw<-filter(df.tw, practice==F)
df.ev<-filter(df.ev, practice==F)
df.be<-filter(df.be, practice==F)

df.sw$alpha<-5
df.sw$beta<-0.003333333
df.sw$alpha[df.sw$delay_cond=='reliable']<-200
df.sw$beta[df.sw$delay_cond=='reliable']<-0.1333333

omega<-1

belief_trajectories<-delay_stores<-belief_reports<-list()



#########################################################################
#NS_most_recent:----
#Steps through events adding links from the most recent other event

for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    
    if (nrow(effects)>0)
    {
      for (i in 1:nrow(effects))
      {

        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        best_expl<-cc_tmp$location[nrow(cc_tmp)]
  
        
        b_traj[,,i+1]<-b_traj[,,i]
        b_traj[best_expl,effects$location[i],i+1]<-1

      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_mostrecent.rdata', belief_trajectories, belief_reports, delay_stores)


#########################################################################

#NS_most_recent - Boost----
#Steps through events adding links from the most recent
#other event (adapts to latest belief when reported)

for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    if (truth$n_nodes==3)
    {
      DBN<-DBN3
    } else {
      DBN<-DBN4
    }
    
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    
    if (nrow(effects)>0)
    {
      for (i in 1:nrow(effects))
      {
        
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        best_expl<-cc_tmp$location[nrow(cc_tmp)]
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        
        #BOOSTING
        #If there's been a judgment between this and the previous effect, set that as the current explanation
        if (i>1)
        {
          jix<-which(effects$time[i]>judgments$time & effects$time[i-1]<judgments$time)
          jix<-jix[length(jix)]
        } else {
          jix<-which(effects$time[i]>judgments$time)
          jix<-jix[length(jix)]
        }
        
        if (length(jix)>0)
        {
          cat(pptix, ttix, i,'boosted\n')
          b_traj[,,i+1] <- DBN[,,judgments$belief[jix]]
        }
        
        
        b_traj[best_expl,effects$location[i],i+1]<-1
        
      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_mostrecent.boost.rdata', belief_trajectories, belief_reports, delay_stores)



#########################################################################

#Most likely:----
#steps through events adding the link most likely to explain each effect


for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    
    if (nrow(effects)>0)
    {
      for (i in 1:nrow(effects))
      {
        
        
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta))
        
        
        
        if (nrow(candidate_causes)>1)
        {
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          
        } else {
          choice_row<-1
          best_expl<-candidate_causes$location
        }
        
        b_traj[,,i+1]<-b_traj[,,i]
        b_traj[best_expl,effects$location[i],i+1]<-1
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_mostlikely.rdata', belief_trajectories, belief_reports, delay_stores)

#########################################################################

#Most likely - Boost:----
#steps through events adding the link most likely to explain each effect


for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    if (truth$n_nodes==3)
    {
      DBN<-DBN3
    } else {
      DBN<-DBN4
    }
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    
    if (nrow(effects)>0)
    {
      for (i in 1:nrow(effects))
      {
        
        
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta))
        
        
        
        if (nrow(candidate_causes)>1)
        {
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          
        } else {
          choice_row<-1
          best_expl<-candidate_causes$location
        }
        
        b_traj[,,i+1]<-b_traj[,,i]
        
        
        #BOOSTING
        #If there's been a judgment between this and the previous effect, set that as the current explanation
        if (i>1)
        {
          jix<-which(effects$time[i]>judgments$time & effects$time[i-1]<judgments$time)
          jix<-jix[length(jix)]
        } else {
          jix<-which(effects$time[i]>judgments$time)
          jix<-jix[length(jix)]
        }
        
        if (length(jix)>0)
        {
          cat(pptix, ttix, i,'boosted\n')
          b_traj[,,i+1] <- DBN[,,judgments$belief[jix]]
        }
        
        
        b_traj[best_expl,effects$location[i],i+1]<-1
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_mostlikely.boost.rdata', belief_trajectories, belief_reports, delay_stores)

############################################################################################

#More likely:----
#adds new link only if its 10* more likely than any existing link


threshold<-20 #How many times better does a new link have to be to supplant an old one?

for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    #If there is any evidence at all
    if (nrow(effects)>0)
    {
      #...Step through all the effects
      for (i in 1:nrow(effects))
      {
        
        #Establish the candidate causes of the current effect
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta),
                                  already_a_cause = b_traj[location,effects$location[i],i])
        
        
        #If there are more than one
        if (nrow(candidate_causes)>1)
        {
          #Establish which is the best explanation
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          best_expl_lik<-candidate_causes$delay_lik[choice_row]
          
          
          #Establish the best existing explanation and how likely it is
          existing_causes<-filter(candidate_causes, already_a_cause==1)
          
          
          if (nrow(existing_causes)>1)
          {
            exi_row<-which.max(existing_causes$delay_lik)
            best_exist_expl_ix<-existing_causes$uid[exi_row]
            best_exist_expl_loc<-existing_causes$location[exi_row]
            best_exist_expl_lik<-max(existing_causes$delay_lik)
            
          } else if (nrow(existing_causes)==1) {
            best_exist_expl_ix<-existing_causes$uid
            best_exist_expl_loc<-existing_causes$location
            best_exist_expl_lik<-existing_causes$delay_lik
            
          } else {
            best_exist_expl_ix<-0
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
          
          already_added<-candidate_causes$already_a_cause[choice_row]
          
        } else {
          
          choice_row<-1
          
          best_expl<-candidate_causes$location
          best_expl_lik<-candidate_causes$delay_lik
          already_added<-candidate_causes$already_a_cause
          
          if (candidate_causes$already_a_cause==T)
          {
            best_exist_expl_loc<-best_expl
            best_exist_expl_lik<-best_expl_lik
          } else {
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
        }
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        if (already_added==0)
        {
          if (best_expl_lik/best_exist_expl_lik > threshold)
          {
            
            b_traj[best_expl,effects$location[i],i+1]<-1
            
            cat(pptix, ttix, i, best_expl, effects$location[i], sum(b_traj[,,i]),
                sum(b_traj[,,i+1]),
                'passed threshold', best_expl_lik, best_exist_expl_lik, '\n')
            
          }else {
            cat('threshold not passed', best_expl_lik, best_exist_expl_lik, '\n')
          }
        }
        
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_morelikely.rdata', belief_trajectories, belief_reports, delay_stores)



#########################################################################

#More likely - Boost:----
#adds new link only if its X* more likely than any existing link


threshold<-20 #How many times better does a new link have to be to supplant an old one?

for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    
    if (truth$n_nodes==3)
    {
      DBN<-DBN3
    } else {
      DBN<-DBN4
    }
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    #If there is any evidence at all
    if (nrow(effects)>0)
    {
      #...Step through all the effects
      for (i in 1:nrow(effects))
      {
        
        #Establish the candidate causes of the current effect
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta),
                                  already_a_cause = b_traj[location,effects$location[i],i])
        
        
        #If there are more than one
        if (nrow(candidate_causes)>1)
        {
          #Establish which is the best explanation
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          best_expl_lik<-candidate_causes$delay_lik[choice_row]
          
          
          #Establish the best existing explanation and how likely it is
          existing_causes<-filter(candidate_causes, already_a_cause==1)
          
          
          if (nrow(existing_causes)>1)
          {
            exi_row<-which.max(existing_causes$delay_lik)
            best_exist_expl_ix<-existing_causes$uid[exi_row]
            best_exist_expl_loc<-existing_causes$location[exi_row]
            best_exist_expl_lik<-max(existing_causes$delay_lik)
            
          } else if (nrow(existing_causes)==1) {
            best_exist_expl_ix<-existing_causes$uid
            best_exist_expl_loc<-existing_causes$location
            best_exist_expl_lik<-existing_causes$delay_lik
            
          } else {
            best_exist_expl_ix<-0
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
          
          already_added<-candidate_causes$already_a_cause[choice_row]
          
        } else {
          
          choice_row<-1
          
          best_expl<-candidate_causes$location
          best_expl_lik<-candidate_causes$delay_lik
          already_added<-candidate_causes$already_a_cause
          
          if (candidate_causes$already_a_cause==T)
          {
            best_exist_expl_loc<-best_expl
            best_exist_expl_lik<-best_expl_lik
          } else {
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
        }
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        
        #BOOSTING
        #If there's been a judgment between this and the previous effect,
        #set that as the current explanation
        if (i>1)
        {
          jix<-which(effects$time[i]>judgments$time & effects$time[i-1]<judgments$time)
          jix<-jix[length(jix)]
        } else {
          jix<-which(effects$time[i]>judgments$time)
          jix<-jix[length(jix)]
        }
        
        if (length(jix)>0)
        {
          cat(pptix, ttix, i,'boosted\n')
          b_traj[,,i+1] <- DBN[,,judgments$belief[jix]]
        }
        
        
        
        if (already_added==0)
        {
          if (best_expl_lik/best_exist_expl_lik > threshold)
          {
            
            b_traj[best_expl,effects$location[i],i+1]<-1
            
            cat(pptix, ttix, i, best_expl, effects$location[i], sum(b_traj[,,i]),
                sum(b_traj[,,i+1]),
                'passed threshold', best_expl_lik, best_exist_expl_lik, '\n')
            
          }else {
            cat('threshold not passed', best_expl_lik, best_exist_expl_lik, '\n')
          }
        }
        
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_morelikely.boost.rdata', belief_trajectories, belief_reports, delay_stores)



#########################################################################

#More likely - Adapt between:----
#adds  for "between" participants, tracking of mean for each edge
#E.g. the mean of the previous causings is used to estimate likelihood of existing link explaining the latest event

threshold<-20 #How many times better does a new link have to be to supplant an old one?

for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    #If there is any evidence at all
    if (nrow(effects)>0)
    {
      #...Step through all the effects
      for (i in 1:nrow(effects))
      {
        
        #Establish the candidate causes of the current effect
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta),
                                  already_a_cause = b_traj[location,effects$location[i],i])
        
        
        #If there are more than one
        if (nrow(candidate_causes)>1)
        {
          #Establish which is the best explanation
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          best_expl_lik<-candidate_causes$delay_lik[choice_row]
          
          
          #Establish the best existing explanation and how likely it is
          existing_causes<-filter(candidate_causes, already_a_cause==1)
          
          #Adaptive behaviour for between condition
          if (truth$delay_cond=='between')
          {
            if (nrow(existing_causes)>0)
            {
              for (j in 1:nrow(existing_causes))
              {
                tmp<- existing_causes$delay_lik[j]
                
                existing_causes$delay_lik[j]<-dgamma(existing_causes$delay[j], shape=200, rate=200/mean(delay_store[existing_causes$location[j], effects$location[i]][[1]]))
                
                cat(pptix, ttix, i, j, 'existing cause likelihoods', mean(delay_store[existing_causes$location[j], effects$location[i]][[1]]),
                    'old', tmp, 'new', existing_causes$delay_lik[j], '\n')
              }
            }
            
          }
          
          if (nrow(existing_causes)>1)
          {
            exi_row<-which.max(existing_causes$delay_lik)
            best_exist_expl_ix<-existing_causes$uid[exi_row]
            best_exist_expl_loc<-existing_causes$location[exi_row]
            best_exist_expl_lik<-max(existing_causes$delay_lik)
            
          } else if (nrow(existing_causes)==1) {
            best_exist_expl_ix<-existing_causes$uid
            best_exist_expl_loc<-existing_causes$location
            best_exist_expl_lik<-existing_causes$delay_lik
            
          } else {
            best_exist_expl_ix<-0
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
          
          already_added<-candidate_causes$already_a_cause[choice_row]
          
        } else {
          
          choice_row<-1
          
          best_expl<-candidate_causes$location
          best_expl_lik<-candidate_causes$delay_lik
          already_added<-candidate_causes$already_a_cause
          
          if (candidate_causes$already_a_cause==T)
          {
            #Adaptive behaviour for between condition
            if (truth$delay_cond=='between')
            {
              tmp<- existing_causes$delay_lik
              
              existing_causes$delay_lik<-dgamma(existing_causes$delay, shape=200, rate=200/mean(delay_store[existing_causes$location, effects$location[i]][[1]]))
              
              cat(pptix, ttix, i, 'existing cause likelihood', mean(delay_store[existing_causes$location, effects$location[i]][[1]]),
                  'old', tmp, 'new', existing_causes$delay_lik, '\n')
              
            }
            
            best_exist_expl_loc<-best_expl
            best_exist_expl_lik<-best_expl_lik
          } else {
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
        }
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        if (already_added==0)
        {
          if (best_expl_lik/best_exist_expl_lik > threshold)
          {
            
            b_traj[best_expl,effects$location[i],i+1]<-1
            
            cat(pptix, ttix, i, best_expl, effects$location[i], sum(b_traj[,,i]),
                sum(b_traj[,,i+1]),
                'passed threshold', best_expl_lik, best_exist_expl_lik, '\n')
            
          }else {
            cat('threshold not passed', best_expl_lik, best_exist_expl_lik, '\n')
          }
        }
        
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
      }
      
    }
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_morelikely_adaptbetween.rdata', belief_trajectories, belief_reports, delay_stores)



#########################################################################################

#More likely - Adapt between -  Boost:----
#adds boosting (overwrites online belief with participants latest online judgment where available)

threshold<-20 #How many times better does a new link have to be to supplant an old one?

for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    if (truth$n_nodes==3)
    {
      DBN<-DBN3
    } else {
      DBN<-DBN4
    }
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    
    
    
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    #If there is any evidence at all
    if (nrow(effects)>0)
    {
      #...Step through all the effects
      for (i in 1:nrow(effects))
      {
        
        #Establish the candidate causes of the current effect
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta),
                                  already_a_cause = b_traj[location,effects$location[i],i])
        
        
        #If there are more than one
        if (nrow(candidate_causes)>1)
        {
          #Establish which is the best explanation
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          
          # p_choose<-candidate_causes$delay_lik/sum(candidate_causes$delay_lik)
          # p_choose<-p_choose^omega/sum(p_choose^omega)
          # choice_row<-sample(x=1:length(p_choose), size=1, prob=p_choose)
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          best_expl_lik<-candidate_causes$delay_lik[choice_row]
          
          
          #Establish the best existing explanation and how likely it is
          existing_causes<-filter(candidate_causes, already_a_cause==1)
          
          #Adaptive behaviour for between condition
          if (truth$delay_cond=='between')
          {
            if (nrow(existing_causes)>0)
            {
              for (j in 1:nrow(existing_causes))
              {
                tmp<- existing_causes$delay_lik[j]
                
                d_mean<-mean(delay_store[existing_causes$location[j], effects$location[i]][[1]])
                if (!is.na(d_mean))
                {
                  existing_causes$delay_lik[j]<-dgamma(existing_causes$delay[j], shape=5, rate=5/
                                                         d_mean)
                }
                
                
                cat(pptix, ttix, i, j, 'existing cause likelihoods', mean(delay_store[existing_causes$location[j], effects$location[i]][[1]]),
                    'old', tmp, 'new', existing_causes$delay_lik[j], '\n')
              }
            }
            
          }
          
          if (nrow(existing_causes)>1)
          {
            exi_row<-which.max(existing_causes$delay_lik)
            best_exist_expl_ix<-existing_causes$uid[exi_row]
            best_exist_expl_loc<-existing_causes$location[exi_row]
            best_exist_expl_lik<-max(existing_causes$delay_lik)
            
          } else if (nrow(existing_causes)==1) {
            best_exist_expl_ix<-existing_causes$uid
            best_exist_expl_loc<-existing_causes$location
            best_exist_expl_lik<-existing_causes$delay_lik
            
          } else {
            best_exist_expl_ix<-0
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
          
          already_added<-candidate_causes$already_a_cause[choice_row]
          
        } else {
          choice_row<-1
          best_expl<-candidate_causes$location
          best_expl_lik<-candidate_causes$delay_lik
          already_added<-candidate_causes$already_a_cause
          
          if (candidate_causes$already_a_cause==T)
          {
            #Adaptive behaviour for between condition
            if (truth$delay_cond=='between')
            {
              tmp<- existing_causes$delay_lik
              
              d_mean<-mean(delay_store[existing_causes$location, effects$location[i]][[1]])
              if (!is.na(d_mean))
              {
                existing_causes$delay_lik<-dgamma(existing_causes$delay, shape=5, rate=5/d_mean)
              }
              
              
              cat(pptix, ttix, i, 'existing cause likelihood', mean(delay_store[existing_causes$location, effects$location[i]][[1]]),
                  'old', tmp, 'new', existing_causes$delay_lik, '\n')
              
            }
            
            best_exist_expl_loc<-best_expl
            best_exist_expl_lik<-best_expl_lik
          } else {
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
        }
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        
        #If there's been a judgment between this and the previous effect, set that as the current explanation
        if (i>1)
        {
          jix<-which(effects$time[i]>judgments$time & effects$time[i-1]<judgments$time)
          jix<-jix[length(jix)]
        } else {
          jix<-which(effects$time[i]>judgments$time)
          jix<-jix[length(jix)]
        }
        
        if (length(jix)>0)
        {
          cat(pptix, ttix, i,'boosted\n')
          b_traj[,,i+1] <- DBN[,,judgments$belief[jix]]
        }
        
        if (already_added==0)
        {
          if (best_expl_lik/best_exist_expl_lik > threshold)
          {
            
            b_traj[best_expl,effects$location[i],i+1]<-1
            
            cat(pptix, ttix, i, best_expl, effects$location[i], sum(b_traj[,,i]),
                sum(b_traj[,,i+1]),
                'passed threshold', best_expl_lik, best_exist_expl_lik, '\n')
            
          } else {
            cat('threshold not passed', best_expl_lik, best_exist_expl_lik, '\n')
          }
        }
        
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
      }
      
    }
    
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_morelikely_adaptbetween_boost.rdata', belief_trajectories, belief_reports, delay_stores)



#########################################################################################

#More likely - Pruned:----
#(use the previous judgment where available and to ns style updating to that)

threshold<-20 #How many times better does a new link have to be to supplant an old one?

n_prunes<-0
for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    if (truth$n_nodes==3)
    {
      DBN<-DBN3
    } else {
      DBN<-DBN4
    }
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    #If there is any evidence at all
    if (nrow(effects)>0)
    {
      #...Step through all the effects
      for (i in 1:nrow(effects))
      {
        
        #Establish the candidate causes of the current effect
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta),
                                  already_a_cause = b_traj[location,effects$location[i],i])
        
        
        #If there are more than one
        if (nrow(candidate_causes)>1)
        {
          #Establish which is the best explanation
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          best_expl_lik<-candidate_causes$delay_lik[choice_row]
          
          
          #Establish the best existing explanation and how likely it is
          existing_causes<-filter(candidate_causes, already_a_cause==1)
          
          
          if (nrow(existing_causes)>1)
          {
            exi_row<-which.max(existing_causes$delay_lik)
            best_exist_expl_ix<-existing_causes$uid[exi_row]
            best_exist_expl_loc<-existing_causes$location[exi_row]
            best_exist_expl_lik<-max(existing_causes$delay_lik)
            
          } else if (nrow(existing_causes)==1) {
            best_exist_expl_ix<-existing_causes$uid
            best_exist_expl_loc<-existing_causes$location
            best_exist_expl_lik<-existing_causes$delay_lik
            
          } else {
            best_exist_expl_ix<-0
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
          
          already_added<-candidate_causes$already_a_cause[choice_row]
          
        } else {
          choice_row<-1
          best_expl<-candidate_causes$location
          best_expl_lik<-candidate_causes$delay_lik
          already_added<-candidate_causes$already_a_cause
          
          if (candidate_causes$already_a_cause==T)
          {
            best_exist_expl_loc<-best_expl
            best_exist_expl_lik<-best_expl_lik
          } else {
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
        }
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        
        #If there's been a judgment between this and the previous effect, set that as the current explanation
        # if (i>1)
        # {
        #   jix<-which(effects$time[i]>judgments$time & effects$time[i-1]<judgments$time)
        #   jix<-jix[length(jix)]
        # } else {
        #   jix<-which(effects$time[i]>judgments$time)
        #   jix<-jix[length(jix)]
        # }
        
        
        # if (length(jix)>0)
        # {
        #   cat(pptix, ttix, i,'boosted\n')
        #   b_traj[,,i+1] <- DBN[,,judgments$belief[jix]]
        # }
        
        if (already_added==0)
        {
          if (best_expl_lik/best_exist_expl_lik > threshold)
          {
            
            b_traj[best_expl,effects$location[i],i+1]<-1
            
            cat(pptix, ttix, i, best_expl, effects$location[i], sum(b_traj[,,i]),
                sum(b_traj[,,i+1]),
                'passed threshold', best_expl_lik, best_exist_expl_lik, '\n')
            
          } else {
            cat('threshold not passed', best_expl_lik, best_exist_expl_lik, '\n')
          }
        }
        
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
        
        #Prune here
        for (j in 1:dim(b_traj)[1])
        {
          for (k in 1:dim(b_traj)[2])
          {
            if (b_traj[j,k, i+1]==1)
            {
              n_c<-nrow(filter(events, location==j, time<=effects$time[i]))
              n_e<-nrow(filter(events, location==k, time<=effects$time[i]))
              
              if (pbinom(n_e, n_c, prob=.9)<.05)
              {
                cat('pruned', i, j, k, n_c, n_e, pbinom(n_e, n_c, prob=.9), '\n')
                b_traj[j,k,i+1]<-0
                n_prunes<-n_prunes+1
                
              }
            }
          }
        }
        
      }#\i 
    }#\if there are any effects
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_morelikely_prune.rdata', belief_trajectories, belief_reports, delay_stores)



#########################################################################################

#More likely - Boost - Pruned:----
#(use the previous judgment where available and to ns style updating to that)
threshold<-20 #How many times better does a new link have to be to supplant an old one?

n_prunes<-0
for (pptix in 1:60)
{
  belief_trajectories[[pptix]]<-delay_stores[[pptix]]<-belief_reports[[pptix]]<-list()
  
  for (ttix in 1:12)
  {
    judgments<-filter(df.be, ppt==pptix, trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, ppt==pptix, trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    between<-truth$delay_cond=='between'
    if (truth$n_nodes==3)
    {
      DBN<-DBN3
    } else {
      DBN<-DBN4
    }
    
    events<-filter(df.ev, ppt==pptix, trial_type==ttix) %>%
      select(time, location, type) %>% arrange(time)
    
    if (nrow(events)>0)
    {
      events$uid<-1:nrow(events)
    }
    effects<-filter(events, type=='effect')
    
    b_traj<-array(0,dim=c(n_nodes, n_nodes, nrow(effects)+1))
    
    if (n_nodes==3)
    {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL),
                                  c(NULL), c(NULL), c(NULL)), 3,3)
    } else {
      delay_store<-  matrix( list(c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL),
                                  c(NULL), c(NULL), c(NULL),c(NULL)), 4,4)
    }
    
    #If there is any evidence at all
    if (nrow(effects)>0)
    {
      #...Step through all the effects
      for (i in 1:nrow(effects))
      {
        
        #Establish the candidate causes of the current effect
        ev_time<-effects$time[i]
        cc_tmp<-filter(events, time<ev_time, location!=effects$location[i]) 
        
        candidate_causes<- mutate(cc_tmp, effect = effects$location[i],
                                  effectix = effects$uid[i],
                                  delay = effects$time[i] - time,
                                  delay_lik = dgamma(delay, shape=alpha, rate=beta),
                                  already_a_cause = b_traj[location,effects$location[i],i])
        
        
        #If there are more than one
        if (nrow(candidate_causes)>1)
        {
          #Establish which is the best explanation
          choice_row<-which.max(candidate_causes$delay_lik)
          if (length(choice_row)>1)
          {
            choice_row<-sample(1, choice_row)
          }
          
          best_expl<-candidate_causes$location[choice_row]
          best_expl_lik<-candidate_causes$delay_lik[choice_row]
          
          
          #Establish the best existing explanation and how likely it is
          existing_causes<-filter(candidate_causes, already_a_cause==1)
          
          
          if (nrow(existing_causes)>1)
          {
            exi_row<-which.max(existing_causes$delay_lik)
            best_exist_expl_ix<-existing_causes$uid[exi_row]
            best_exist_expl_loc<-existing_causes$location[exi_row]
            best_exist_expl_lik<-max(existing_causes$delay_lik)
            
          } else if (nrow(existing_causes)==1) {
            best_exist_expl_ix<-existing_causes$uid
            best_exist_expl_loc<-existing_causes$location
            best_exist_expl_lik<-existing_causes$delay_lik
            
          } else {
            best_exist_expl_ix<-0
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
          
          already_added<-candidate_causes$already_a_cause[choice_row]
          
        } else {
          choice_row<-1
          best_expl<-candidate_causes$location
          best_expl_lik<-candidate_causes$delay_lik
          already_added<-candidate_causes$already_a_cause
          
          if (candidate_causes$already_a_cause==T)
          {
            best_exist_expl_loc<-best_expl
            best_exist_expl_lik<-best_expl_lik
          } else {
            best_exist_expl_loc<-0
            best_exist_expl_lik<-0
          }
        }
        
        
        b_traj[,,i+1]<-b_traj[,,i]
        
        #If there's been a judgment between this and the previous effect, set that as the current explanation
        if (i>1)
        {
          jix<-which(effects$time[i]>judgments$time & effects$time[i-1]<judgments$time)
          jix<-jix[length(jix)]
        } else {
          jix<-which(effects$time[i]>judgments$time)
          jix<-jix[length(jix)]
        }
        
        
        if (length(jix)>0)
        {
          cat(pptix, ttix, i,'boosted\n')
          b_traj[,,i+1] <- DBN[,,judgments$belief[jix]]
        }
        
        if (already_added==0)
        {
          if (best_expl_lik/best_exist_expl_lik > threshold)
          {
            
            b_traj[best_expl,effects$location[i],i+1]<-1
            
            cat(pptix, ttix, i, best_expl, effects$location[i], sum(b_traj[,,i]),
                sum(b_traj[,,i+1]),
                'passed threshold', best_expl_lik, best_exist_expl_lik, '\n')
            
          } else {
            cat('threshold not passed', best_expl_lik, best_exist_expl_lik, '\n')
          }
        }
        
        
        delay_store[best_expl,effects$location[i]][[1]]<-c(delay_store[best_expl,effects$location[i]][[1]], candidate_causes$delay[choice_row])
        
        #Prune here
        for (j in 1:dim(b_traj)[1])
        {
          for (k in 1:dim(b_traj)[2])
          {
            if (b_traj[j,k, i+1]==1)
            {
              n_c<-nrow(filter(events, location==j, time<=effects$time[i]))
              n_e<-nrow(filter(events, location==k, time<=effects$time[i]))
              
              if (pbinom(n_e, n_c, prob=.9)<.05)
              {
                cat('pruned', i, j, k, n_c, n_e, pbinom(n_e, n_c, prob=.9), '\n')
                b_traj[j,k,i+1]<-0
                n_prunes<-n_prunes+1
                
              }
            }
          }
        }
        
      }#\i 
    }#\if there are any effects
    
    belief_reports[[pptix]][[ttix]]<-list()
    for (jix in 1:nrow(judgments))
    {
      belief_reports[[pptix]][[ttix]][[jix]]<-b_traj[,,1+sum(effects$time<judgments$time[jix])]
    }
    
    
    
    
    
    belief_trajectories[[pptix]][[ttix]]<-b_traj
    delay_stores[[pptix]][[ttix]]<-delay_store
    
    cat('ppt', pptix, 'trial', ttix, '\n\n')
  }#trial
}#participant

save(file='../data/NS_morelikely_boost_prune.rdata', belief_trajectories, belief_reports, delay_stores)
