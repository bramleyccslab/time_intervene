#####################
#Bayesian predictions
#####################


#Libraries
library(acl)
library(dplyr)
library(tidyr)
library(foreach)
library(doMC)

rm(list=ls())

source('likelihood_functions.R')

load('../data/prolific_data.rdata')
# load('../data/simulated_participants.rdata')

# df.sw$alpha<-5
# df.sw$beta<-0.003333333
# df.sw$alpha[df.sw$delay_cond=='reliable']<-200
# df.sw$beta[df.sw$delay_cond=='reliable']<-0.1333333

upis<-as.character(df.sw$upi)


registerDoMC(7)
getDoParWorkers()

#####################
#Do real participants
#####################

set.seed(0)
# for (pptix in 1:1)#length(upis))
# {
test<-foreach(pptix=1:83) %dopar% #1:length(upis)) %dopar%
{
  source('likelihood_functions.R')
  
  likelihoods<-posts<-list()
  
  for (ttix in 1:12)
  {
    likelihoods[[ttix]]<-posts[[ttix]]<-list()
    
    judgments<-filter(df.be, upi==upis[pptix], trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
    
    truth<-filter(df.tw, upi==upis[pptix], trial_type==ttix)
    n_nodes<-truth$n_nodes
    alpha<-df.sw$alpha[pptix]
    beta<-df.sw$beta[pptix]
    strength<-.9
    verbose=T
    
    if (truth$delay_cond=='reliable')
    {
      min_delay<-800
      max_delay<-3000
    } else {
      min_delay<-0
      max_delay<-5600
    }
    max_pat1 <- 50000
    max_pat2 <- 1000
    
    #n_samples<-500
    
    for (jix in 1:nrow(judgments))
    {
      events<-filter(df.ev, upi==upis[pptix], trial_type==ttix, time>0) %>%
        select(time, location, type, from, with_delay) %>% arrange(time)
      
      if (nrow(events)>0)
      {
        events$uid<-1:nrow(events)
      }
      
      
      events<-filter(events, time<=judgments$time[jix])
      end_at<-judgments$time[jix]
      
      
      likelihoods[[ttix]][[jix]]<-within_likelihood(events=events,
                                                    n_nodes=n_nodes, alpha=alpha, beta=beta, strength=strength,
                                                    min_delay=min_delay, max_delay=max_delay,
                                                    max_pat1 = max_pat1, max_pat2 = max_pat2,
                                                    end_at = end_at, truth, verbose=F)
      
      posts[[ttix]][[jix]]<-likelihoods[[ttix]][[jix]]/sum(likelihoods[[ttix]][[jix]])
      
      cat('trial', ttix, 'judgment', jix, 'n_nodes', n_nodes,
          'entropy', shannon_entropy(posts[[ttix]][[jix]]), '\n\n')
      
      #       if (!any(is.nan(posts[[ttix]][[jix]])))
      #       {
      #         plot(posts[[ttix]][[jix]], xlab='Graph', ylab='Probability', main=paste('trial_type', ttix, 'judgment', jix, sep=''))
      #         abline(v=truth$graph, col='red')
      #       }
      
    }#Judgment
    
    
  }#Trial
  save(file=paste('../data/individual_fits_prolific/ppt', pptix, 'upi', upis[pptix],  '.rdata',sep=''), likelihoods, posts)
  
  1
}


par(mfrow=c(4,3), mar=c(2,2,2,1))
for (ttix in 1:12)
{
  
  plot(posts[[ttix]][[length(posts[[ttix]])]], xlab='Graph', ylab='Probability', main=ttix)
  abline(v=tgixs[ttix], col='red')
}




##########################
#Do simulated participants
##########################

# rm(list=ls())
# 
# #source('likelihood_functions.R')
# 
# 
# # load('../data/simulated_participants.rdata')
# 
# load('../data/cogsci_data.rdata')
# upis<-as.character(df.sw$upi)
# 
# 
# registerDoMC(7)
# getDoParWorkers()
# 
# set.seed(0)
# test<-foreach(pptix = 1:length(upis)) %dopar%
# {
#   load('../data/cogsci_data.rdata')
#   load('../data/simulated_participants.rdata')
#   #A few events occur slightly after 45,000; for the reactive condition a number of actions are taken after 45,000.
#   #This excludes both ~(2000/89000)
#   df.sim<-df.sim[df.sim$overflow==F,]
#   
#   upis<-as.character(df.sw$upi)
#   
#   source('likelihood_functions.R')
#   
#   for (sc in 1:9)
#   {
#     #   for (pptix in 1:1)#length(upis))
#     #   {
#     #     df.sw$alpha<-5
#     #     df.sw$beta<-0.003333333
#     #     df.sw$alpha[df.sw$delay_cond=='reliable']<-200
#     #     df.sw$beta[df.sw$delay_cond=='reliable']<-0.1333333
#     
#     likelihoods<-posts<-list()
#     
#     for (ttix in 1:12)
#     {
#       likelihoods[[ttix]]<-posts[[ttix]]<-list()
#       
#       judgments<-filter(df.be, upi==upis[pptix], trial_type==ttix) %>% select(time, belief, n_nodes) %>% arrange(time)
#       judgment<-judgments[nrow(judgments),]
#       
#       truth<-filter(df.tw, upi==upis[pptix], trial_type==ttix)
#       #filter(df.sim, upi==upis[pptix], trial_type==ttix, learn_cond==sc)
#       n_nodes<-truth$n_nodes
#       alpha<-df.sw$alpha[pptix]
#       beta<-df.sw$beta[pptix]
#       strength<-.9
#       verbose=T
#       
#       if (truth$delay_cond=='reliable')
#       {
#         min_delay<-800
#         max_delay<-3000
#       } else {
#         min_delay<-0
#         max_delay<-5600
#       }
#       max_pat1 <- 50000
#       max_pat2 <- 1000
#       
#       n_samples<-500
#       
#       events<-filter(df.sim, upi==upis[pptix], learn_cond==sc, trial_type==ttix) %>%
#         select(time, location, type, from, with_delay) %>% arrange(time) %>% mutate(type = as.character(type))
#       
#       if (nrow(events)>0)
#       {
#         events$uid<-1:nrow(events)
#       }
#       
#       
#       events<-filter(events, time<=judgment$time)
#       end_at<-judgment$time
#       
#       
#       likelihoods[[ttix]]<-within_likelihood(events=events,
#                                              n_nodes=n_nodes, alpha=alpha, beta=beta, strength=strength,
#                                              min_delay=min_delay, max_delay=max_delay,
#                                              max_pat1 = max_pat1, max_pat2 = max_pat2,
#                                              end_at = end_at, truth, verbose=F)
#       
#       
#       posts[[ttix]]<-likelihoods[[ttix]]/sum(likelihoods[[ttix]])
#       
#       #       cat('trial', ttix, 'n_nodes', n_nodes,
#       #           'entropy', shannon_entropy(posts[[ttix]]), '\n\n')
#       # 
#       #       if (!any(is.nan(posts[[ttix]])))
#       #       {
#       #         plot(posts[[ttix]], xlab='Graph', ylab='Probability', main=paste('trial_type', ttix, sep=''))
#       #         abline(v=truth$graph, col='red')
#       #       }
#       
#       
#     }#Trial
#     
#     save(file=paste('../data/individual_fits_cogsci/sims/sims', sc, 'ppt', pptix, 'upi', upis[pptix], '.rdata',sep=''), likelihoods, posts)
#     
# 
#   }
#   pptix
# }
# 
# par(mfrow=c(4,3), mar=c(2,2,2,1))
# for (ttix in 1:length(posts))
# {
#   
#   plot(posts[[ttix]], xlab='Graph', ylab='Probability', main=ttix)
#   abline(v=tgixs[ttix], col='red')
# }
