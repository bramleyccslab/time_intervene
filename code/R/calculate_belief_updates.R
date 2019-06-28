# SQL package
library(sqldf)

rm(list=ls())
load('../data/cogsci_data.rdata')

# Get information about participants updating beliefs
# Esp. removing arrows

base <- df.tw %>% select(ppt, trial, trial_type, n_nodes, delay_cond, graph)

# Calculate belief update attributes per participant & per trial
calc_belief_updates <- function(pix, tix) {
  
  # Check if participant presents
  existence <- which(base$ppt == pix)
  
  if (length(existence) > 0)  {
    # Grab belief data for this participant and this trial
    beliefs <- filter(df.be, ppt == pix, trial == tix, time > 0) %>% 
      select(time, belief) %>% arrange(time) %>% # Reenforce order
      select(belief)
    beliefs <- unique(beliefs) # Only keep belief changes, i.e unique beliefs
    beliefs$uid <- 1: nrow(beliefs) # Use uid to keep track of temporal order
    
    
    # Q1: How many times does this participant change her mind?
    belief_updates <- nrow(beliefs)
    
    
    # Q2: How many of the belief changes are link-removals?
    n_nodes <- (filter(base, ppt == pix, trial == tix))$n_nodes
    graph_base <- if (n_nodes == 3) DBN3 else DBN4
    
    belief_removals <- 0
    
    # Function that finds linkage-removal points
    # argument: line index
    is_removal <- function(idx) {
      # Linkages from previous belief
      belief_0 <- which(graph_base[, , beliefs$belief[idx - 1]] == 1)
      # Linkages in current belief
      belief_1 <- which(graph_base[, , beliefs$belief[idx]] == 1)
      # Are all linkages in belief_0 also present in belief_1?
      removals <- which((belief_0 %in% belief_1) == FALSE)
      # If find any linkage removal, return true
      return(length(removals) > 0)
    } 
    
    if (belief_updates > 1) {
      for(i in (2 : belief_updates)) {
        belief_removals <- belief_removals + is_removal(i)
      }
    } else {
      belief_removals <- 0
    }
    
    
    # Q3: What's her final belief?
    final_belief <- beliefs$belief[nrow(beliefs)]
    
    return(data.frame('ppt' = pix,
                      'trial' = tix,
                      'belief_updates' = belief_updates,
                      'belief_removals' = belief_removals,
                      'final_belief' = final_belief))
    
  }
  
}


belief <- calc_belief_updates(1, 1)
# Do stats for all instances
for (p in (1:60)) {
  for (t in (1:12)) {
    belief <- if (p > 1 || t > 1) rbind(belief, calc_belief_updates(p, t))
  } 
}
beliefs <- rbind(calc_belief_updates(1, 1), belief)

# Combine base + belief, and save the result
export <- merge(base, beliefs, by=c('ppt', 'trial'))
save(export, file = '../data/bn_beliefs.rdata')

# Some stats
sqldf("SELECT belief_removals, COUNT(*) n FROM export GROUP BY belief_removals")
#  belief_removals   n
# 0 447
# 1  27
# 2   5
# 3   1