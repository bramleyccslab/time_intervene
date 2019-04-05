#Get posterior entropies----

#Simply loops over the posteriors calculating and storing the entropies

library(dplyr)
library(tidyr)
library(acl)

rm(list=ls())

load('../data/prolific_data.rdata')

df.be$entropy<-NA
df.tw$final_entropy<-NA

upis<-as.character(df.sw$upi)
gix<- (head(df.tw, 12) %>% arrange(trial_type))$graph

#Get 'Bayesian' model predictions
file_list<-list.files('../data/individual_fits_prolific/')
file_list<-file_list[-length(file_list)]

for (i in 1:nrow(df.sw))
{
 
  load(paste('../data/individual_fits_prolific/ppt', i, 'upi', upis[i], '.rdata',sep='')) 
  
  if (is.list(posts))
  {
    #p.unlisted<-unlist(posts, recursive=F)
    
    for (j in 1:length(posts))
    {
      these_posts<-posts[[j]]
      ix<-which(df.be$upi==upis[i] & df.be$trial_type==j & df.be$practice==F)
      
      for (k in 1:length(these_posts))
      {
        #Add the online entropies to the belief data frame
        if (any(is.nan(these_posts[[k]])))
        {
          cat('\n',i,j,k, shannon_entropy(these_posts[[k]], log_type='bits'))
          df.be$entropy[ix[k]]<-NA
        } else {

          df.be$entropy[ix[k]]<-shannon_entropy(these_posts[[k]], log_type='bits')
        }


      }
      #Add the final entropy to the trialwise data frame
      df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$trial_type==j & df.tw$practice==F]<-df.be$entropy[ix[k]]
      
      df.tw$max_p[df.tw$upi==upis[i] & df.tw$trial_type==j] <-max(posts[[j]][[length(posts[[j]])]])
      df.tw$p_truth[df.tw$upi==upis[i] & df.tw$trial_type==j] <-posts[[j]][[length(posts[[j]])]][gix[j]]
    }
 
  }
  
  df.sw$m_ent[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i]], na.rm=T)
  df.sw$m_ent3[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$n_nodes==3], na.rm=T)
  df.sw$m_ent4[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$n_nodes==4], na.rm=T)
  df.sw$m_ent_ncy3[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$cyclic==F & df.tw$n_nodes==3], na.rm=T)
  df.sw$m_ent_ncy4[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$cyclic==F & df.tw$n_nodes==4], na.rm=T)
  df.sw$m_ent_cy3[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$cyclic==T & df.tw$n_nodes==3], na.rm=T)
  df.sw$m_ent_cy4[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$cyclic==T & df.tw$n_nodes==4], na.rm=T)
  df.sw$m_ent_ncy[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$cyclic==F], na.rm=T)
  df.sw$m_ent_cy[df.sw$upi==upis[i]]<-mean(df.tw$final_entropy[df.tw$upi==upis[i] & df.tw$cyclic==T], na.rm=T)
  
  cat(i, '\n')

}

df.tw$initial_ent<-6
df.tw$initial_ent[df.tw$n_nodes==4]<-12
df.tw <- df.tw %>% mutate(igain = initial_ent - final_entropy,
                          igain.norm = igain/initial_ent)
resave(file='../data/prolific_data.rdata', df.be, df.sw, df.tw)
