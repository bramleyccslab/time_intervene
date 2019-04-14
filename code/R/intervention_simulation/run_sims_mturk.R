#Load packages----
library(LaplacesDemon)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(js)
#library(V8)
#library(doMC)
#library(doParallel)
#library(foreach)

load('../data/cogsci_data.rdata')

#Set up the parameters----

#Intervention rules----

#Random * even * systematic * time &| location
sim_cond <- data.frame(expand.grid(t_spacing = (0 : 2), l_spacing = (0 : 2)))

#Problems (i.e. the generating graphs)----
networks = list(
  list(c(0,0,1),c(0,0,1),c(0,0,0)),
  list(c(0,1,0),c(0,0,1),c(0,0,0)),
  list(c(0,1,1),c(0,0,0),c(0,0,0)),
  
  list(c(0,0,0,1),c(0,0,0,1),c(0,0,0,1),c(0,0,0,0)),
  list(c(0,1,0,0),c(0,0,1,0),c(0,0,0,1),c(0,0,0,0)),
  list(c(0,1,1,1),c(0,0,0,0),c(0,0,0,0),c(0,0,0,0)),
  
  list(c(0,1,0), c(0,0,1), c(0,1,0)),
  list(c(0,1,0), c(0,0,1), c(1,0,0)),
  list(c(0,1,0), c(1,0,1), c(0,0,0)),
  
  list(c(0,1,0,0), c(0,0,1,0), c(0,1,0,1), c(0,0,0,0)),
  list(c(0,1,0,0), c(0,0,1,0), c(0,0,0,1), c(1,0,0,0)),
  list(c(0,1,0,0), c(0,0,1,0), c(1,0,0,1), c(0,0,0,0))
) #By row


#Parameters for each simulation----
params<-list(N = NA,
             prob_N = NA,
             network = NA,
             learn_cond = 'simulation',
             delay_cond = NA,
             timeout = 45000,
             background = 0,
             contingency = 0.9,
             which_subject_background = 'fixed',
             delay_mu = 1500, 
             delay_alpha = 5,
             n_ints = NA)

#Dataframe for storing the data created in each simulation----
df.sim <- data.frame(ppt = c(),
                     upi= c(),
                     learn_cond = c(),
                     delay_cond = c(),
                     trial_type = c(), 
                     n_nodes = c(),
                     n_ints = c(),
                     location = c(),
                     time=c(),
                     type = c(),
                     from = c(),
                     with_delay = c(),
                     done= c())

#Get the unique personal identifiers from the real participants----
upis <- df.sw$upi

#FOR DEVELOPMENT:
# sc<-ppt<-problem<-1
# params$delay_cond <- "var_both"
# params$delay_alpha = 200

set.seed(0)

#Loop over simulation conditions
for (sc in 1 : nrow(sim_cond))
{
  #Loop over participants
  for (ppt in 1 : 40) {
    
    #res[[sc]][[ppt]]<-list()
    
    #Set the same delay condition as they had
    params$delay_cond <- df.sw$delay_cond[df.sw$upi == upis[ppt]] #c('rel_both', 'var_both')[cond]
    
    if (params$delay_cond == 'unreliable') {
      params$delay_alpha <- 5
    } else if (params$delay_cond=='reliable') {
      params$delay_alpha <- 200
    }
    
    #Loop over problems
    for (problem in 1 : 12) {
      
      if (df.tw$n_ints[df.tw$upi==upis[ppt] & df.tw$trial_type == problem] > 0) {
        #Set the current network
        params$network <- networks[[problem]]
        #Set the number of nodes
        params$N <- length(params$network[[1]])
        #Set the number of interventions actually performed by this original participant
        #I.e.: use the same number of interventions as the true participant (where possible)
        params$n_ints <- df.tw$n_ints[df.tw$upi == upis[ppt] & df.tw$trial_type == problem]
        
        nodes <- 1 : params$N #LETTERS[1:params$N]
        
        cat('sc:', sc, 'ppt:', ppt, 'problem:', problem, '\nparams:', unlist(params), '\n\n')
        
        #Prechoose timings (conditions 1 and 2)
        if (sim_cond$t_spacing[sc] == 0) {
          #Prechosen randomly timed interventions
          t_tmp <- sort(ceiling(runif(params$n_ints) * params$timeout))
          #NOTE: Could force the first to be at the first frame?
          
        } else if (sim_cond$t_spacing[sc] == 1) {
          #Prechosen evenly timed interventions
          t_tmp <- ceiling(seq(1, params$timeout, length.out = (params$n_ints + 1)))[-(params$n_ints + 1)]
        } else if (sim_cond$t_spacing[sc] == 2) {
          t_tmp <- 1
        }
        
        if (sim_cond$l_spacing[sc] == 0)
        {
          #Random locations
          l_tmp <- sample(nodes, params$n_ints, replace = T)
          
        } else if (sim_cond$l_spacing[sc] == 1) {
          #Even locations
          tmp <- c(rep(1 : params$N, floor(params$n_ints / params$N)),
                     sample(1 : params$N, params$n_ints %% params$N, replace = F))
          
          if (length(tmp) > 1) {
            l_tmp <- sample(tmp)
          } else {
            l_tmp <- tmp
          }
          
        } # Bonan: 2 ??
        
        #Set up dataframe with all intervention times (conditions 0,1) or just the first (condition 2)
        df.tl <- data.frame(ppt = ppt,
                            upi = as.character(upis[ppt]),
                            learn_cond = sc,
                            delay_cond = params$delay_cond,
                            trial_type = problem,
                            n_nodes = params$N,
                            n_ints = params$n_ints,
                            location = NA,
                            time = t_tmp,
                            type = 'action',
                            from = NA,
                            with_delay = NA,
                            done = F) #Timeline
        
        if (sim_cond$t_spacing[sc] != 2 & sim_cond$l_spacing[sc] != 2) {
          #####################################################################
          #GO THROUGH TRIAL IF INTERVENTIONS' AND TIMINGS CAN BE SET IN ADVANCE
          #####################################################################
          
          # Random locations
          df.tl$location <- l_tmp
          
          timer <- 0
          
          # Walk through the trial, determining the effects and propagating them----
          while(T) {
            #Establish the row of the next event
            tmp <- which(df.tl$time >= timer & df.tl$done == F)
            
            if (length(tmp) > 0) {
              next_event <- tmp[1] # ???
            } else {
              break
            }
            
            
            timer <- df.tl$time[next_event]
            
            df.tl$done[next_event] <- T
            
            #Only keep processing effects while the cause is within the observation window
            if (timer < params$timeout)
            {
              #Establish any effects
              effects <- params$network[[df.tl$location[next_event]]] == 1
              
              #Roll dice to see if they happen
              dice <- runif(length(effects)) < params$contingency
              
              #Remove any unsuccessful effects
              effects <- effects & dice
              
              #Draw delays
              delay_draws <- round(rgamma(params$N, shape = params$delay_alpha, rate =  params$delay_alpha / params$delay_mu))
              
              #Loop through effects
              for (j in 1 : params$N) {
                if (effects[j] == T) {
                  df.tl <- rbind(df.tl,
                                 data.frame(ppt = ppt,
                                            upi =  as.character(upis[ppt]),
                                            learn_cond = sc,
                                            delay_cond = params$delay_cond,
                                            trial_type = problem, 
                                            n_nodes = params$N,
                                            n_ints = params$n_ints,
                                            location = j,
                                            time=timer+delay_draws[j],
                                            type = 'effect',
                                            from = df.tl$location[next_event],
                                            with_delay = delay_draws[j],
                                            done = F))
                }
                
              }
              
              df.tl<-arrange(df.tl, time)
            } #Within timing
            
            #cat('timer', timer, 'number events', nrow(df.tl), '\n')
          } #End trial
          
          
        } else if (sim_cond$t_spacing[sc] != 2 & sim_cond$l_spacing[sc] == 2){
          
          ###############################################################
          #GO THROUGH TRIAL DYNAMICALLY SETTING LOCATIONS BUT NOT TIMINGS
          ###############################################################
          
          #Flat initial choice preferences
          loc_pref <- rep(1, params$N)
          #Random initial location
          df.tl$location[1] <- which(rmultinom(1, 1, loc_pref) == 1)
          
          timer <- 0
          
          
          #Walk through the trial, determining the effects and propagating them----
          while(T) {
            #Establish the row of the next event in the current timeline
            tmp <- which(df.tl$time >= timer & df.tl$done == F)
            
            #End the run through once we've exhausted all the events
            if (length(tmp) > 0) {
              next_event <- tmp[1]
            } else {
              break
            }
            
            #Check the time
            timer <- df.tl$time[next_event]
            
            #If its an intervention give it a location
            if(df.tl$type[next_event] == 'action') {
              df.tl$location[next_event] <- which(rmultinom(1, 1, loc_pref) == 1)
            }
            
            
            #Tick it off
            df.tl$done[next_event] <- T
            
            #Only keep processing effects while the cause is within the observation window
            if (timer < params$timeout)
            {
              #Establish any effects
              effects <- params$network[[df.tl$location[next_event]]] == 1
              
              #Roll dice to see if they happen
              dice <- runif(length(effects)) < params$contingency
              
              #Remove any unsuccessful effects
              effects <- effects & dice
              
              #Draw delays
              delay_draws <- round(rgamma(params$N, shape = params$delay_alpha, rate =  params$delay_alpha / params$delay_mu))
              
              #Loop through effects adding them to the timeline
              for (j in 1 : params$N) {
                if (effects[j] == T) {
                  df.tl <- rbind(df.tl,
                                 data.frame(ppt = ppt,
                                            upi =  as.character(upis[ppt]),
                                            learn_cond = sc,
                                            delay_cond = params$delay_cond,
                                            trial_type = problem, 
                                            n_nodes = params$N,
                                            n_ints = params$n_ints,
                                            location = j,
                                            time=timer+delay_draws[j],
                                            type = 'effect',
                                            from = df.tl$location[next_event],
                                            with_delay = delay_draws[j],
                                            done = F))
                  
                  #Otherwise increase the loc pref for the most recent intervention
                  tmp <- which(df.tl$type == 'action' & df.tl$time <= timer)
                  cat('timer', timer, tmp)
                  mri <- tmp[length(tmp)]
                  
                  loc_pref[df.tl$location[mri]] <- loc_pref[df.tl$location[mri]] + 1
                }
                
              }
              #Sort the timeline so they are all in time order
              df.tl <- arrange(df.tl, time)
              
            }#Within timing
            
            #cat('timer', timer, 'number events', nrow(df.tl), '\n')
          }#End trial
          
          
        } else if (sim_cond$t_spacing[sc] == 2 & sim_cond$l_spacing[sc] != 2){
          ###############################################################
          #GO THROUGH TRIAL DYNAMICALLY SETTING TIMINGS BUT NOT LOCATIONS
          ###############################################################
          
          
          timer <- 0
          ints_assigned <- 1
          #Set the location for the initial intervention
          df.tl$location <- l_tmp[1]
          
          #Walk through the trial, determining the effects and propagating them----
          while(T)
          {
            #Establish the row of the next event in the current timeline
            tmp <- which(df.tl$time >= timer & df.tl$done == F)
            
            #End the run through once we've exhausted all the events
            if (length(tmp) > 0)
            {
              next_event <- tmp[1]
            } else {
              
              if (ints_assigned < params$n_ints)
              {
                ints_assigned <- ints_assigned + 1
                
                df.tl <- rbind(df.tl,
                               data.frame(ppt = ppt,
                                          upi = as.character(upis[ppt]),
                                          learn_cond = sc,
                                          delay_cond = params$delay_cond,
                                          trial_type = problem, 
                                          n_nodes = params$N,
                                          n_ints = params$n_ints,
                                          location = l_tmp[ints_assigned],
                                          time=timer + 3000,
                                          type = 'action',
                                          from = NA,
                                          with_delay = NA,
                                          done= F))
                
                next_event <- nrow(df.tl)
              } else {
                break
              }
              
            }
            
            #Check the time
            timer<-df.tl$time[next_event]
            
            #If its an intervention give it a location
            # if(df.tl$type[next_event]=='action')
            # {
            #   df.tl$location[next_event] <-which(rmultinom(1, 1, loc_pref)==1)
            # }
            
            
            #Tick it off
            df.tl$done[next_event] <- T
            
            #Only keep processing effects while the cause is within the observation window
            if (timer < params$timeout) {
              #Establish any effects
              effects <- params$network[[df.tl$location[next_event]]]==1
              
              #Roll dice to see if they happen
              dice <- runif(length(effects))<params$contingency
              
              #Remove any unsuccessful effects
              effects <- effects & dice
              
              #Draw delays
              delay_draws <- round(rgamma(params$N, shape = params$delay_alpha, rate =  params$delay_alpha / params$delay_mu))
              
              #Loop through effects adding them to the timeline
              for (j in 1:params$N) {
                if (effects[j]==T)
                {
                  df.tl<-rbind(df.tl,
                               data.frame(ppt = ppt,
                                          upi =  as.character(upis[ppt]),
                                          learn_cond = sc,
                                          delay_cond = params$delay_cond,
                                          trial_type = problem, 
                                          n_nodes = params$N,
                                          n_ints = params$n_ints,
                                          location = j,
                                          time=timer+delay_draws[j],
                                          type = 'effect',
                                          from = df.tl$location[next_event],
                                          with_delay = delay_draws[j],
                                          done= F))
                  
                  #Otherwise increase the loc pref for the most recent intervention
                  
                  # tmp<-which(df.tl$type=='action' & df.tl$time<=timer)
                  # cat('timer', timer, tmp)
                  # mri<-tmp[length(tmp)]
                  # 
                  # loc_pref[df.tl$location[mri]]<-loc_pref[df.tl$location[mri]]+1
                }
                
              }
              #Sort the timeline so they are all in time order
              df.tl <- arrange(df.tl, time)
              
            }#Within timing
            
            #cat('timer', timer, 'number events', nrow(df.tl), '\n')
          }#End trial
          
          
          
        } else if (sim_cond$t_spacing[sc] == 2 & sim_cond$l_spacing[sc] == 2){
          ###############################################################
          #GO THROUGH TRIAL DYNAMICALLY SETTING TIMINGS AND LOCATIONS
          ###############################################################
          
          timer <- 0
          ints_assigned <- 1
          
          #Flat initial choice preferences
          loc_pref <- rep(1, params$N)
          #Random initial location
          df.tl$location[1] <- which(rmultinom(1, 1, loc_pref)==1)
          
          #Walk through the trial, determining the effects and propagating them----
          while(T)
          {
            #Establish the row of the next event in the current timeline
            tmp <- which(df.tl$time >= timer & df.tl$done == F)
            
            #End the run through once we've exhausted all the events
            if (length(tmp) > 0)
            {
              #Set the next event as the next unprocessed event in the df that happened after the last one you processed
              next_event <- tmp[1]
            } else {
              if (ints_assigned<params$n_ints) {
                ints_assigned<-ints_assigned+1
                
                df.tl<-rbind(df.tl,
                             data.frame(ppt = ppt,
                                        upi =  as.character(upis[ppt]),
                                        learn_cond = sc,
                                        delay_cond = params$delay_cond,
                                        trial_type = problem, 
                                        n_nodes = params$N,
                                        n_ints = params$n_ints,
                                        location = NA,#l_tmp[ints_assigned],
                                        time=timer+3000,
                                        type = 'action',
                                        from = NA,
                                        with_delay = NA,
                                        done= F))
                
                next_event<-nrow(df.tl)
              } else {
                break
              }
              
            }
            
            #Check the time
            timer<-df.tl$time[next_event]
            
            #If its an intervention give it a location
            if(df.tl$type[next_event]=='action')
            {
              df.tl$location[next_event] <-which(rmultinom(1, 1, loc_pref)==1)
            }
            
            
            #Tick it off
            df.tl$done[next_event]<-T
            
            #Only keep processing effects while the cause is within the observation window
            if (timer<params$timeout)
            {
              #Establish any effects
              effects<-params$network[[df.tl$location[next_event]]]==1
              
              #Roll dice to see if they happen
              dice<-runif(length(effects))<params$contingency
              
              #Remove any unsuccessful effects
              effects<-effects & dice
              
              #Draw delays
              delay_draws<-round(rgamma(params$N, shape = params$delay_alpha, rate =  params$delay_alpha / params$delay_mu))
              
              #Loop through effects adding them to the timeline
              for (j in 1:params$N)
              {
                if (effects[j]==T)
                {
                  df.tl<-rbind(df.tl,
                               data.frame(ppt = ppt,
                                          upi = as.character(upis[ppt]),
                                          learn_cond = sc,
                                          delay_cond = params$delay_cond,
                                          trial_type = problem, 
                                          n_nodes = params$N,
                                          n_ints = params$n_ints,
                                          location = j,
                                          time=timer+delay_draws[j],
                                          type = 'effect',
                                          from = df.tl$location[next_event],
                                          with_delay = delay_draws[j],
                                          done= F))
                  
                  #Otherwise increase the loc pref for the most recent intervention
                  #TODO CHECK THIS IS IN THE RIGHT PLACE, SHOULD PERHAPS BE AT THE POINT OF THE EFFECT's OCCURANCE...
                  tmp<-which(df.tl$type=='action' & df.tl$time<=timer)
                  cat('timer', timer, tmp)
                  mri<-tmp[length(tmp)]
                  
                  loc_pref[df.tl$location[mri]]<-loc_pref[df.tl$location[mri]]+1
                }
                
              }
              #Sort the timeline so they are all in time order
              df.tl<-arrange(df.tl, time)
              
            }#Within timing
            
            #cat('timer', timer, 'number events', nrow(df.tl), '\n')
            
          }#End trial
        }#End of the condition splitter
        
        df.sim<-rbind(df.sim, df.tl)
        
      }#If theres at least one intervention
    }#problem
  }#ppt
}#cond

#Remove actions that occurred after trial over
nrow(filter(df.sim, time>45000)) / nrow(df.sim)

df.sim$overflow <- F

df.sim$overflow[df.sim$time>45000] <- T
       
save(file='../../data/simulated_participants.rdata', df.sim)


#Reorganise save file:
# rm(list=ls())
# 
# load('../../data/cogsci_data.rdata')
# load('../../data/simulated_participants.rdata')
# 
# for (i in 1:length(df.sim))
# {
#   df.sim<-
#     tmp<-ldply(df.sim[[1]][[1]], data.frame)
# }
# 

