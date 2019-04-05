#For the bayesian model these assign a likelihood to data given a model by averaging over many possible paths


within_likelihood<- function(events, n_nodes, alpha, beta, strength, min_delay, max_delay, max_pat1, max_pat2, end_at, truth, verbose=F)
{
  #Events data frame detailing all the events in the data
  #n_nodes how many nodes are there?
  #alpha, beta parameters of the known common delay distribution
  #strength the probability that a cause causes its effect at all
  #min delay - don't consider events that occur closer than this as potential causes (reduces the number of patterns)
  #max delay - ditto
  #max_pat1 - what is the maximum number of patterns you are willing to express at all
  #(this cap is met by removing the least likely candidate causes before creating the matrix,
  #it must create a little bias because it greedily keeps more plausible patterns in the mix)
  #max_pat2 - what is the maximum number of patterns you are willing to evaluate
  #(this cap is met by randomly subsampling from the expressed patterns and using the evaluated patterns to estimate the total)
  #hopefullly this one isn't biased
  #end_at, when does the observational window end?
  #verbose - whether or not to print details of the patterns evaluated
  
  if (n_nodes==3)
  {
    DBN<-DBN3 
  } else {
    DBN<-DBN4
  }
  
  #prior
  prior<-rep(1/dim(DBN)[3], dim(DBN)[3])
  li<-rep(0, dim(DBN)[3])
  
  effects<-filter(events, type=='effect')
  
  if (nrow(effects)>0)
  {
    #Loop over graphs checking if they're possible
    graph_possible<- rep(T, dim(DBN)[3])
    for (i in 1:dim(DBN)[3])
    {
      g<-DBN[,,i]
      
      #Loop through events to see if pattern is possible
      pending<-rep(0, n_nodes)
      
      for (j in 1:nrow(events))
      {
        #If its effect, subtract one from effects pending for this node
        if (events$type[j]=='effect')
        {
          pending[events$location[j]]<-pending[events$location[j]]-1
        }
        #cat('j',j, 'pending', pending, '\n')
        
        if (min(pending)<0)
        {
          #Exclude the graph if there's an effect with no possible cause
          graph_possible[i]<-F  
        }
        
        #Add new expected effects to 'pending'
        pending<-pending + g[events$location[j],]
      }
      #Any remaining numbers in pending correspond to effects that failed to occur at that node
      #(but whose actual causes are still potentially ambiguous)
      causes<-events[rep(1:nrow(events), apply(g[events$location,], 1, sum)),]
      fail_li<-dbinom(nrow(effects), nrow(causes), .9)
      
      #If its sufficiently improbable enough to see this few activations given the structure, rule it out
      if (pbinom(nrow(effects), nrow(causes), .9)<0.0001)
      {
        graph_possible[i]<-F
      }
    }
    
    #Now just loop over the graphs that are actually possible
    for (i in (1:dim(DBN)[3])[graph_possible])
    {
      g<-DBN[,,i]
      
      
      #causes$target<-unlist(apply(g[events$location,]==1, 1, which))  
      
      
      #Create a list for the possible causes of each effect
      pc<-vector(mode="list", length=nrow(effects))
      
      #Loop over the list adding all the possible causes
      for (j in 1:length(pc)) {
        
        pc[[j]]<-filter(events, 
                        time>(effects$time[j]-max_delay),
                        time<(effects$time[j]-min_delay),
                        location!=effects$location[j],
                        g[location, effects$location[j]]==1) %>% mutate(implied_delay = effects$time[j] - time,
                                                                        implied_delay_lik = dgamma(implied_delay, shape=alpha, rate=beta))
      }
      
      orig_n_pat<-prod(sapply(pc, nrow))
      
      #Reduce the number of combinations by removing the least likely cause of any event with more than one
      #from the list until the product is acceptable
      while(prod(sapply(pc, nrow))>max_pat1)
      {
        ix<-which(sapply(pc, nrow)>1)[which.min(sapply(sapply(pc, '[[', 'implied_delay_lik', simplify=F), min)[sapply(pc, nrow)>1])]
        
        pc[[ix]]<-pc[[ix]][-which.min(pc[[ix]]$implied_delay_lik),]
      }
      
      
      #If there are no impossible events then this could be the correct graph
      if (min(sapply(pc, nrow))>0)
      {
        
        #If so there could be a number of patterns of actual causation that may have occurred
        #This is given by the number of combinations of possible cause and effect mapping
        #where each event causes an event at each directly-downstream location at most once
        #cat(i, sapply(pc, nrow), '\n')
        
        #Get all the possible patterns of actual causation
        uev<-sapply(pc, '[[', 'uid', simplify=F)
        tmp<-expand.grid(uev) #Each row gives one set up of the cause of effects 1:ncol
        #Same matrix but with the locations...
        #uev.l<-sapply(pc, '[[', 'location', simplify=F)
        #tmp.l<-expand.grid(uev.l)
        #uev
        
        #Check if any causing-event has too many effects in the actual causation pattern
        #if so kick this pattern out
        #Slow if many paths
        ix<-rep(T, nrow(tmp))
        for (j in 1:max(tmp))
        {
          ix[(sum(g[events$location[j],]) - rowSums(tmp==j))<0]<-F
        }

        
        if (any(ix) | truth$graph==i)
        {
          
          #Limit the number of patterns you evaulate (Note they're now in a random order this
          #hopefully shouldn't matter)
          if (sum(ix)>max_pat2)
          {
            approx <- T
            n_ix<-rep(F, length(ix))
            n_ix[sample(x = which(ix==1), size=max_pat2, replace=F)]<-T
            
            #cat('hyp', i, ' subsampling ', max_pat2, ' from', sum(ix), '\n')
            
          } else {
            approx <- F
            n_ix <- ix
            
          }
          
          #cat('graph',i , orig_n_pat, approx,  '\n')
          
          #Now focus on actual causation network j
          #calculating likelihood of each transition and nontransition
          if (any(ix))
          {
            acs<-tmp[n_ix,, drop=F]
            
            li_ac<-rep(0, nrow(acs))
            
            #For a given actual cause pattern
            for (j in 1:nrow(acs))
            {
              ac<-cbind(unlist(acs[j,]), effects$uid)
              ac.l<-cbind(events$location[ac[,1]], events$location[ac[,2]])
              
              delay_lis<-dgamma(events$time[ac[,2]] - events$time[ac[,1]], shape=alpha, rate=beta)
              delay_li<-prod(delay_lis) #* dbinom(nrow(effects), nrow(causes), .9)
              
              li_ac[j]<-delay_li #* fail_li
              
              #cat('actual cause pattern number',j, delay_li, suc_li, '\n')
              
            } #j actual causation paths
          } else if (truth$graph==i) {
            delay_li<-prod(dgamma(effects$with_delay, shape=alpha, rate=beta))
            
            #Occasionally this product is smaller than the precision available, in which case give it a very small value
            if (delay_li==0)
            {
              cat('Precision exceeded for true path', i, '\n')
              delay_li<-1e-306
            }

            li_ac<-delay_li
          } else {
            li_ac<-0
          }
          
          
          
          causes<-events[rep(1:nrow(events), apply(g[events$location,], 1, sum)),]
          fail_li<-dbinom(nrow(effects), nrow(causes), .9)
          
          
          #Note: currently rarely using this approx i think
          if (approx == F)
          {
            li[i]<-sum(li_ac) * fail_li
          } else {
            li[i]<-sum(li_ac) * (sum(ix)/sum(n_ix)) * fail_li
          }
          
          
          if (verbose==T)
          {
            if (i==truth$graph)
            {
              cat(ttix, 'hypothesis',i, 'orig_n_pat', orig_n_pat, 'n pat expressed', length(ix), 'n pat pruned', sum(ix), 'n pat eval', sum(n_ix),
                  'fail_li', fail_li, 'delay_li', delay_li, 'total li:', li[i], '\n')#jix, 
            }
            
          }
          
          
          
        } else {
          #If no paths remain after pruning
          
          #This should almost never happen if the precheck is working
          delay_li<-0
          fail_li<-0
          li[i]<-0
        }#
        
        
      } else {
        #If no paths possible in the first place
        
        #This should also almost never happen since the precheck should catch these,
        #perhaps if one of the true delays is sufficiently surprising to not be considered as a possibility
        ix<-c()
        delay_li<-0
        fail_li<-0
        li[i]<-0
      }
      
      
    } #Graphs
    
    
  } else {
    #If there are no events at all
    if (verbose==T)
    {
      cat(ttix, 'no events!\n')
    }
    
    li<-rep(1, length(prior))
  }
  
  
  li
}







# within_likelihood_old<- function(events, n_nodes, alpha, beta, strength, min_delay, max_delay, max_pat1, max_pat2, end_at, verbose=F)
# {
#   #Events data frame detailing all the events in the data
#   #n_nodes how many nodes are there?
#   #alpha, beta parameters of the known common delay distribution
#   #strength the probability that a cause causes its effect at all
#   #min delay - don't consider events that occur closer than this as potential causes (reduces the number of patterns)
#   #max delay - ditto
#   #max_pat1 - what is the maximum number of patterns you are willing to express at all
#   #(this cap is met by removing the least likely candidate causes before creating the matrix,
#   #it must create a little bias because it greedily keeps more plausible patterns in the mix)
#   #max_pat2 - what is the maximum number of patterns you are willing to evaluate
#   #(this cap is met by randomly subsampling from the expressed patterns and using the evaluated patterns to estimate the total)
#   #hopefullly this one isn't biased
#   #end_at, when does the observational window end?
#   #verbose - whether or not to print details of the patterns evaluated
#   
#   if (n_nodes==3)
#   {
#     DBN<-DBN3 
#   } else {
#     DBN<-DBN4
#   }
#   
#   #prior
#   prior<-rep(1/dim(DBN)[3], dim(DBN)[3])
#   li<-rep(0, dim(DBN)[3])
#   
#   effects<-filter(events, type=='effect')
#   
#   if (nrow(effects)>0)
#   {
#     #Loop over graphs checking if they're possible
#     graph_possible<- rep(T, dim(DBN)[3])
#     for (i in 1:dim(DBN)[3])
#     {
#       g<-DBN[,,i]
#       
#       #Loop through events to see if pattern is possible
#       pending<-rep(0, n_nodes)
#       
#       for (j in 1:nrow(events))
#       {
#         #If its effect, subtract one from those pending for this node
#         if (events$type[j]=='effect')
#         {
#           pending[events$location[j]]<-pending[events$location[j]]-1
#         }
#         #cat('j',j, 'pending', pending, '\n')
#         
#         if (min(pending)<0)
#         {
#           graph_possible[i]<-F  
#         }
#         
#         #Add expected effects to 'pending'
#         pending<-pending + g[events$location[j],]
#       }
#       #Any remaining numbers in pending correspond to effects that failed to occur at that node
#       #(but where the cause is potentially ambiguous)
#     }
#     
#     #Now just loop over the graphs that are actually possible
#     for (i in (1:dim(DBN)[3])[graph_possible])
#     {
#       g<-DBN[,,i]
#       
#       #Create a list for the possible causes of each effect
#       pc<-vector(mode="list", length=nrow(effects))
#       
#       #Loop over the list adding all the possible causes
#       for (j in 1:length(pc)) {
#         
#         pc[[j]]<-filter(events, 
#                         time>(effects$time[j]-max_delay),
#                         time<(effects$time[j]-min_delay),
#                         location!=effects$location[j],
#                         g[location, effects$location[j]]==1) %>% mutate(implied_delay = effects$time[j] - time,
#                                                                         implied_delay_lik = dgamma(implied_delay, shape=alpha, rate=beta))
#       }
#       
#       orig_n_pat<-prod(sapply(pc, nrow))
#       
#       #Reduce the number of combinations by removing the least likely cause of the most ambiguous event
#       #from the list until the product is acceptable
#       while(prod(sapply(pc, nrow))>max_pat1)
#       {
#         idls<-pc[[which.max(sapply(pc, nrow) )]]$implied_delay_lik
#         pc[[which.max(sapply(pc, nrow) )]]<-pc[[which.max(sapply(pc, nrow) )]][-which.min(idls),]
#       }
#       
#       
#       #If there are no impossible events then this could be the correct graph
#       if (min(sapply(pc, nrow))>0)
#       {
#         
#         #If so there could be a number of patterns of actual causation that may have occurred
#         #This is given by the number of combinations of possible cause and effect mapping where each
#         #where each event causes an event at each directly downstream location at most once
#         #cat(i, sapply(pc, nrow), '\n')
#         
#         #Get all the possible patterns of actual causation
#         uev<-sapply(pc, '[[', 'uid', simplify=F)
#         tmp<-expand.grid(uev) #Each row gives one set up of the cause of effects 1:ncol
#         #Same matrix but with the locations...
#         #uev.l<-sapply(pc, '[[', 'location', simplify=F)
#         #tmp.l<-expand.grid(uev.l)
#         
#         
#         #Check if any causing-event has too many effects in the actual causation pattern
#         #if so kick this pattern out
#         ix<-rep(T, nrow(tmp))
#         for (j in 1:max(tmp))
#         {
#           ix[(sum(g[events$location[j],]) - rowSums(tmp==j))<0]<-F
#         }
#         
#         
#         if (any(ix))
#         {
#           
#           #Limit the number of patterns you evaulate (Note they're now in a random order this shouldn't matter)
#           if (sum(ix)>max_pat2)
#           {
#             approx <- T
#             n_ix<-rep(F, length(ix))
#             n_ix[sample(x = which(ix==1), size=max_pat2, replace=F)]<-T
#             
#             #cat('hyp', i, ' subsampling ', max_pat2, ' from', sum(ix), '\n')
#             
#           } else {
#             approx <- F
#             n_ix <- ix
#           }
#           
#           #Now focus on actual causation network j
#           #calculating likelihood of each transition and nontransition
#           acs<-tmp[n_ix,, drop=F]
#           
#           li_ac<-rep(0, nrow(acs))
#           
#           #For a given actual cause pattern
#           for (j in 1:nrow(acs))
#           {
#             ac<-cbind(unlist(acs[j,]), effects$uid)
#             ac.l<-cbind(events$location[ac[,1]], events$location[ac[,2]])
#             
#             #Given each of these, there may be failed or "time-outed" causation
#             #(where the effect hadn't occurred by the end of the clip
#             
#             n_fai<-n_suc<-p_fails<-rep(0, nrow(events))
#             for (k in 1:nrow(events))
#             {
#               #How many effects do we expect this event to have
#               n_exp<-sum(g[events$location[k],]==1)
#               #How many does it have according to the timeline
#               n_suc[k]<-sum(ac[,1]==k)
#               #How many failed
#               n_fai[k]<- n_exp - n_suc[k]
#               
#               p_before_end<-pgamma(end_at - events$time[k], shape=alpha, rate=beta)#Probability of a failure
#               p_after_end<-1-p_before_end
#               
#               p_fails[k]<-((1-strength)*p_before_end + strength*p_after_end)^n_fai[k]
#             }
#             
#             
#             fail_li<-1*prod(p_fails)
#             #Binomial likelihood for the successes and failures here?
#             # kk<-sum(n_suc)
#             # nn<-sum(n_fai) + sum(n_suc)
#             # suc_li <- choose(nn, kk) * .9^kk * (1-.9)^(nn-kk) 
#             
#             
#             delay_lis<-dgamma(events$time[ac[,2]] - events$time[ac[,1]], shape=alpha, rate=beta)
#             delay_li<-prod(delay_lis) * .9^length(delay_lis)
#             
#             li_ac[j]<-delay_li * fail_li
#             
#             #cat('actual cause pattern number',j, delay_li, suc_li, '\n')
#           } #j actual causation paths
#           
#           if (approx == F)
#           {
#             li[i]<-sum(li_ac)
#           } else {
#             li[i]<-sum(li_ac) * (sum(ix)/sum(n_ix))
#           }
#           
#           
#           if (verbose==T)
#           {
#             if (i==truth$graph)
#             {
#               cat(ttix, 'hypothesis',i, 'orig_n_pat', orig_n_pat, 'n pat expressed', length(ix), 'n pat pruned', sum(ix), 'n pat eval', sum(n_ix),
#                   'fail_li', fail_li, 'delay_li', delay_li, 'total li:', li[i], '\n')#jix, 
#             }
#             
#           }
#           
#           
#           
#         } else {
#           #If no paths remain after pruning
#           delay_li<-0
#           fail_li<-0
#           li[i]<-0
#         }#
#         
#         
#       } else {
#         #If no paths possible in the first place
#         ix<-c()
#         delay_li<-0
#         fail_li<-0
#         li[i]<-0
#       }
#       
#       
#     } #Graphs
#   } else {
#     #If there are no events at all
#     if (verbose==T)
#     {
#       cat(ttix, 'no events!\n')
#     }
#     
#     li<-rep(1, length(prior))
#   }
#   
#   
#   li
# }
