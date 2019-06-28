# SQL package
library(sqldf)

rm(list=ls())
load('../data/cogsci_data.rdata')
load('../data/prolific_data.rdata')

# Get information about participants updating beliefs
# For each participant and each trial, answer the following questions:
# 1. How many times does this participant change her mind?
# 2. How many of the belief changes are linkage-removals?
# 3. What's her final belief?


# Grab basic info
base <- df.tw %>% select(ppt, trial, trial_type, n_nodes, delay_cond, graph)

# Function that calculates belief update attributes per participant & per trial
# @param {int}  pix participent_index
# #param {int}  tix trial_index
calc_belief_updates <- function(pix, tix) {
  
  # Check if participant presents
  existence <- which(base$ppt == pix)
  
  if (length(existence) > 0)  {
    # Grab beliefs for this participant and this trial
    beliefs <- filter(df.be, ppt == pix, trial == tix, time > 0) %>% 
               select(time, belief) %>% 
               arrange(time) %>% # Reinforce temporal order
               select(belief)
    
    # Q1: How many times does this participant change her mind?
    if (nrow(beliefs) > 1) {
      # Loop through beliefs, if different from previous one then it is an update
      # Otherwise it is a repeated belief
      beliefs_list <- beliefs[ ,1] # Vectorize
      repeated_beliefs <- vector()
      for (i in (2 : (length(beliefs_list)))) {
        if (beliefs_list[i] == beliefs_list[i - 1]) {
          repeated_beliefs <- c(repeated_beliefs, i)
        }
      }
      updated_beliefs <- if(length(repeated_beliefs) > 0) beliefs_list[-repeated_beliefs] else beliefs_list 
      belief_updates <- length(updated_beliefs)
    } else {
      belief_updates <- 0 
    }

    
    # Q2: How many of the belief changes are link-removals?
    n_nodes <- (filter(base, ppt == pix, trial == tix))$n_nodes
    graph_base <- if (n_nodes == 3) DBN3 else DBN4
    
    belief_removals <- 0
    
    # Function that finds linkage-removal points
    # @param {int}  idx  line index
    is_removal <- function(idx) {
      # Linkages from previous belief
      belief_0 <- which(graph_base[ , ,updated_beliefs[idx - 1]] == 1)
      # Linkages in current belief
      belief_1 <- which(graph_base[ , ,updated_beliefs[idx]] == 1)
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
    final_belief <- updated_beliefs[length(updated_beliefs)]
    
    return(data.frame('ppt' = pix,
                      'trial' = tix,
                      'belief_updates' = belief_updates,
                      'belief_removals' = belief_removals,
                      'final_belief' = final_belief))
    
  }
  
}


# Create a belief_update dataframe
df.bp <- data.frame(ppt=integer(),
                    trial=integer(),
                    belief_updates=integer(),
                    belief_removals=integer(),
                    final_belief=integer()) 

for (p in (1:83)) {
  for (t in (1:12)) {
    df.bp <- rbind(df.bp, calc_belief_updates(p, t))
  } 
}

# Combine base + belief, and save the result
df.bp <- merge(base, df.bp, by=c('ppt', 'trial'))
save(file='../data/cogsci_data.rdata',df.tw, df.ev, df.be, df.bp, df.sw, df.tw, DBN3, DBN4, tgixs, get_ix)

# Results
export <- df.bp
sqldf("SELECT belief_removals, COUNT(*) n FROM export GROUP BY belief_removals")
# blief_removals   n
# 0 445
# 1  28
# 2   5
# 3   1
# 4   1
# 3   1