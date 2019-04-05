#Compare models to data
library(dplyr)
library(tidyr)
library(acl)
library(xtable)

rm(list=ls())



load('../data/pilot_data.rdata')
pilot_upis<-as.character(df.sw$upi)

load('../data/cogsci_data.rdata')
load('../data/simulated_participants.rdata')

upis<-as.character(df.sw$upi)

get_score<-function(m1, m2, n_nodes)
{
  if (n_nodes==3)
  {
    g1<-DBN3[,,m1]
    g2<-DBN3[,,m2]
  } else {
    g1<-DBN4[,,m1]
    g2<-DBN4[,,m2]
  }
  
  g1flat<-(g1 + t(2*g1))[upper.tri(matrix(0, dim(g1)[1],dim(g1)[2]))]
  g2flat<-(g2 + t(2*g2))[upper.tri(matrix(0, dim(g2)[1],dim(g2)[2]))]
  sum(g1flat==g2flat)
}

#Get 'Bayesian' model predictions
file_list<-list.files('../data/individual_fits_cogsci/')
file_list<-file_list[-length(file_list)]

mod_preds$bayes<-0


mod_preds<-select(df.be, ppt, upi, trial_type, n_nodes, time, belief, delay_cond) %>%
  mutate(graph = tgixs[df.be$trial_type],
         initial_judgment = time==0,
         final_judgment = 1:nrow(df.be) %in% c((which(time==0)-1)[-1], nrow(df.be)),
         bayes=0,
         bayes.exist=0)

for (i in 1:nrow(df.sw))
{
  
  if (paste('ppt', i, 'upi', upis[i], '.rdata',sep='')%in%file_list)
  {
    #These are the bayesian fits... i.e. a list of likelihoods and posts
    #Lists have an entry for each problem.
    #Entries have a subentry for each judgment within the problem
    #these are vectors of length 64 or 4096 with a probability for every model
    load(paste('../data/individual_fits_cogsci/ppt', i, 'upi', upis[i], '.rdata',sep='')) 
    bayes_exist = T
  } else {
    cat('no data', i)
    likelihoods<-NA
    posts<-NA
    bayes_exist <- F
  }
  
  
  
  if (is.list(posts))
  {
    #p.unlisted<-unlist(posts, recursive=F)
    
    for (j in 1:length(posts))
    {
      tmp<-rep(1, nrow(filter(df.be, upi==upis[i], trial_type==j)))
      
      for (k in 1:length(posts[[j]]))
      {
        maxp<-which(posts[[j]][[k]]==max(posts[[j]][[k]]))
        if (length(maxp)>1)
        {
          maxp<-sample(maxp, 1)
        } else if (length(maxp)==0) {
          
          cat(i, j, k, 'nans!\n')
          maxp<-NA#sample(1:length(posts[[j]][[k]]), 1)
        }
        
        tmp[k]<-maxp
      }
      mod_preds$bayes[df.be$upi==upis[i] & df.be$trial_type==j] <- tmp
    }
  }
  mod_preds$bayes.exist[df.be$upi==upis[i]]<-bayes_exist
}



#These contain a single prediction for every judgment
file_names<-c('../data/NS_mostrecent.rdata',
              '../data/NS_mostrecent.boost.rdata',
              '../data/NS_mostlikely.rdata',
              '../data/NS_mostlikely.boost.rdata',    
              '../data/NS_morelikely.rdata',
              '../data/NS_morelikely.boost.rdata',
              '../data/NS_morelikely_boost_prune.rdata',
              '../data/NS_morelikely_prune.rdata',
              '../data/NS_morelikely_adaptbetween_boost.rdata',
              '../data/NS_morelikely_adaptbetween.rdata')

mod_names<-c('mostrecent',
             'mostrecent.boost',
             'mostlikely',
             'mostlikely.boost',
             'morelikely',
             'morelikely.boost',
             'morelikely.boost.prune',
             'morelikely.prune',
             'morelikely.adaptbetween',
             'morelikely.adaptbetween.boost')


#Get NS style model predictions
for (mod_num in 1:length(mod_names))
{
  load(file=file_names[mod_num])
  
  #PPT
  for (i in 1:length(upis))
  {
    pptix<-which(pilot_upis==upis[i])
    #TRIAL
    for (j in 1:length(belief_reports[[pptix]]))
    {
      br_traj<-belief_reports[[pptix]][[j]]
      tmp<-rep(0, length(br_traj))
      #JUDGMENT
      for (k in 1:length(br_traj))
      {
        tmp[k]<-get_ix(br_traj[[k]])
      }
      mod_preds[[mod_names[mod_num]]][mod_preds$ppt==pptix & mod_preds$trial_type==j] <-tmp
      
      
    }
    cat('model', mod_num, 'participant', pptix, 'trial', j, 'the judgment predictions for this problem', tmp, '\n')
  }
}




# load('../data/order_only_posteriors.rdata')
# for (i in 1:nrow(df.sw))
# {
# 
#   
#     for (j in 1:length(order_posts[[i]]))
#     {
#       tmp<-rep(1, nrow(filter(df.be, ppt==i, trial_type==j)))
#       
#       for (k in 1:length(order_posts[[i]][[j]]))
#       {
#         
#         post<-order_posts[[i]][[j]][[k]]
#         
#         
#         maxp<-which(post==max(post))
#         if (length(maxp)>1)
#         {
#           cat(i,j,k, 'n choices', length(maxp), '\n')
#           maxp<-sample(maxp, 1)
#         } else if (length(maxp)==0) {
#           cat(i, j,k, 'nans!\n')
#           maxp<-NA#sample(1:length(posts[[j]][[k]]), 1)
#         }
#         
#         tmp[k]<-maxp
#       }
#       
#       mod_preds$oo[df.be$ppt==i & df.be$trial_type==j]<-tmp
# 
#   }
# }


#Compare with love server model predictions
# file_list<-list.files('../data/individual_fits/from_love_server/')
# for (i in 1:nrow(df.sw))
# {
#   if (paste('ppt', i, '.rdata', sep='')%in%file_list)
#   {
#     load(paste('../data/individual_fits/ppt', i, '.rdata', sep=''))
#   } else {
#     likelihoods<-NA
#     posts<-NA
#   }
#   
#   
#   
#   if (is.list(posts))
#   {
#     #Loop over trial types
#     for (j in 1:length(posts))
#     {
#       tmp<-rep(1, nrow(filter(df.be, ppt==i, trial_type==j)))
#       
#       for (k in 1:length(posts[[j]]))
#       {
#         maxp<-which(posts[[j]][[k]]==max(posts[[j]][[k]]))
#         if (length(maxp)>1)
#         {
#           maxp<-sample(maxp, 1)
#         } else if (length(maxp)==0) {
#           cat(i, j,k, 'nans!\n')
#           maxp<-NA#sample(1:length(posts[[j]][[k]]), 1)
#         }
#         
#         tmp[k]<-maxp
#       }
#       
#       if (length(mod_preds$bayes.love[df.be$ppt==i & df.be$trial_type==j])!=length(tmp))
#       {
#         cat(i,j,k, length(mod_preds$bayes.love[df.be$ppt==i & df.be$trial_type==j]), length(tmp),  '\n')
#       }
#       
#       mod_preds$bayes.love[df.be$ppt==i & df.be$trial_type==j]<-tmp
#     }
#   }
#   
# }


mod_names<-c(mod_names, 'bayes')#,'oo')

for (i in 1:nrow(mod_preds))
{
  for (mod_num in 1:length(mod_names))
  {
    mod_preds[[paste(mod_names[mod_num], '.cor', sep='')]][i]<-get_score(mod_preds[[mod_names[mod_num]]][i], mod_preds$belief[i], mod_preds$n_nodes[i])
    mod_preds[[paste(mod_names[mod_num], '.perf', sep='')]][i]<-get_score(mod_preds[[mod_names[mod_num]]][i], mod_preds$graph[i], mod_preds$n_nodes[i])
  }
  
  
  
  mod_preds$ppt.perf[i]<-get_score(mod_preds$belief[i], mod_preds$graph[i], mod_preds$n_nodes[i])
  
  mod_preds$check[i]<-get_score(mod_preds$graph[i], mod_preds$graph[i], mod_preds$n_nodes[i])#This is just the total available on each device judgment
  cat(i, '\n')
}

resave(file='../data/mf_results.rdata', mod_preds)

head(mod_preds)
load('../data/cogsci_data.rdata')
load('../data/mf_results.rdata')
upis<-as.character(df.sw$upi)

#oo = order only - 
#mostrecent - the simple heuristic, always connect events to their most recent predecessor
#mostlikely - delay sensitive but still local, always connect events to their most likely predecessor
#morelikely - delay sensitive and local, but also only adds a new edge if its 20 times more likely than any existing explanation
#boost - uses previous judgment as a basis if available

chosen_mod_names<- c('mostrecent', 'mostlikely', 'morelikely', 'bayes')#c('mostrecent.boost', 'mostlikely.boost', 'morelikely.boost', 'bayes')#

res<-data.frame(mod=names(select(mod_preds, which(names(df) %in% paste(chosen_mod_names, '.cor', sep='')),
                                 ppt.perf, which(names(df) %in% paste(chosen_mod_names, '.perf', sep='')))))

#Proportion all judgments correctly predicted
df<-filter(mod_preds, initial_judgment==F, bayes.exist==T, !is.na(bayes))
res$all<-colSums(select(df, which(names(df) %in% paste(chosen_mod_names, '.cor', sep='')),
                        ppt.perf, which(names(df) %in% paste(chosen_mod_names, '.perf', sep=''))))/sum(df$check)

which_max_all_acc<-rep(0, 40)
for (i in 1:40)
{
  df.i<-filter(df, initial_judgment==F, bayes.exist==T, !is.na(bayes), upi==upis[i])
  accs<-colSums(select(df.i, which(names(df) %in% paste(chosen_mod_names, '.cor', sep=''))))/sum(df.i$check)
  which_max_all_acc[i]<-which.max(accs)
  print(max(accs))
}

summary(factor(which_max_all_acc, levels=1:length(chosen_mod_names), labels=names(accs)))

#Proportion final judgments correctly predicted
df<-filter(mod_preds, final_judgment==T, bayes.exist==T, !is.na(bayes))
res$final<-colSums(select(df, which(names(df) %in% paste(chosen_mod_names, '.cor', sep='')),
                          ppt.perf, which(names(df) %in% paste(chosen_mod_names, '.perf', sep=''))))/sum(df$check)

res


which_max_acc<-rep(0, 40)
for (i in 1:40)
{
  df.i<-filter(df, final_judgment==T, bayes.exist==T, !is.na(bayes), upi==upis[i])
  accs<-colSums(select(df.i, which(names(df) %in% paste(chosen_mod_names, '.cor', sep=''))))/sum(df.i$check)
  which_max_acc[i]<-which.max(accs)
  print(max(accs))
}

summary(factor(which_max_acc, levels=1:length(chosen_mod_names), labels=names(accs)))

#save(file='../data/mf_results.rdata', mod_preds, res)

# rm(list=ls())
 load('../data/mf_results.rdata')

res$mod<-as.character(res$mod)

tab<-res[grep('cor', res$mod),]
perf<-res[grep('perf', res$mod),]
perf<-perf[2:nrow(perf),2:3]

tab$mod<-substr(tab$mod, 1, (nchar(tab$mod)-4))

tab
perf
tab2<-cbind(tab[,1], perf*100, tab[,2:3]*100,
            summary(factor(which_max_all_acc, levels=1:length(chosen_mod_names), labels=names(accs))),
            summary(factor(which_max_acc, levels=1:length(chosen_mod_names), labels=names(accs))))
names(tab2)<-c('Model','Acc (all)','Acc (final)','Cor (all)','Cor (final)', 'N fit (all)', 'N fit (final)')

tab2$Model<-c('OO','TS','STS', 'Rational')
print(xtable(tab2, digits=1), include.rownames = F)


tmp<-group_by(mod_preds, trial_type) %>%summarise(Par = mean(ppt.perf/check),
                                                  Random = 1,
                                                  OO = mean(mostrecent.perf/check),
                                                  TS =mean(mostlikely.perf/check),
                                                  STS = mean(morelikely.perf/check),
                                                  Rational = mean(bayes.perf/check, na.rm=T))

cor_mat<-cor(as.matrix(tmp[,2:7]))
diag(cor_mat)<-NA
cor_mat[lower.tri(cor_mat)]<-NA
xtable(cor_mat, na.rm=T, digits=2)
