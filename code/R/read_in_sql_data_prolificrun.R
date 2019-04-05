library(dplyr)
library(tidyr)
library(acl)
library(abind)

rm(list=ls())
load('../data/dbn.rdata')
csv<-read.csv('../data/neil_time2.csv', header=F, stringsAsFactors=F)

ip<-csv[,1]
date<-csv[,2]
events<-csv[,3]
beliefs<-csv[,4]
trials<-csv[,5]
extras<-csv[,6]
feedback<-csv[,7]


######################
#Subjectwise
######################

vec = rep(0, length(extras))
sw.ch<-data.frame(upi = vec, gender = vec, age = vec,
                  learn_cond = vec, delay_cond = vec,
                  final_score = vec, final_bonus = vec)

for (i in 1:length(extras))
{
  sw.ch[i,]<-unlist(strsplit(extras[i], ','))
}

dupes<-which(duplicated(sw.ch$upi))
# sw.ch$upi[dupes]<-paste0('dupes',c(1,2), sep = '')
sql_row_ix<-which(!duplicated(sw.ch$upi))

sw.ch<-sw.ch[-dupes,]

df.sw<-mutate(sw.ch,
              ppt = 1:nrow(sw.ch),
              upi = factor(upi, levels = upi),
           gender = factor(gender, levels = c('female', 'male')),
           learn_cond = factor(learn_cond, levels=c('active','passive')),
           delay_cond = factor(delay_cond, levels = c('rel_both','rel_within_var_between', 'var_both'),
                                   labels = c('reliable','between', 'within')),
           age = as.numeric(as.character(age)),
           final_score = as.numeric(as.character(final_score)),
           final_bonus = as.numeric(as.character(final_bonus)))

df.sw$alpha<-200
df.sw$beta<-0.1333333
df.sw$alpha[df.sw$delay_cond=='reliable']<-5
df.sw$beta[df.sw$delay_cond=='reliable']<-0.003333333
  
######################
#Trialwise
######################

df.tw<-data.frame(matrix(0, 0, 11))
tw_names<-c("ppt", "upi",  "trial", "trial_type", "n_nodes", "n_ints",
           "score", "accum_score", "bonus", "accum_bonus")
names(df.tw)<-tw_names
for (i in df.sw$ppt)
{
  tmp<-unlist(strsplit(trials[sql_row_ix[i]], '\n'))
  for (j in 1:length(tmp))
  {
    tmp2<-data.frame(t(c(i,as.character(df.sw$upi[i]), unlist(strsplit(tmp[j], ',')))))
    names(tmp2)<-tw_names
    df.tw<-rbind(df.tw, tmp2)
  }
}



for (i in 1:ncol(df.tw))
{
  if (names(df.tw)[i]!='upi')
  {
    df.tw[[i]]<-as.numeric(as.character(df.tw[[i]]))
  } else {
    df.tw[[i]]<-as.factor(df.tw[[i]])
  }

}
str(df.tw)

df.tw$trial_type<-df.tw$trial_type+1
df.tw$trial<-df.tw$trial-1
df.tw$n_edges<-3
df.tw$n_edges[df.tw$n_nodes==4]<-6
df.tw$acc<- df.tw$score/df.tw$n_edges
df.tw$n_ints<-6-df.tw$n_ints
df.tw$learn_cond<-rep(df.sw$learn_cond, each=13)
df.tw$delay_cond<-rep(df.sw$delay_cond, each=13)
df.tw$practice<-df.tw$trial==0

#Get the true graph indices
tgs<-graphs<-list(
  
  matrix(c(0,0,1,0,0,1,0,0,0), ncol=3, byrow=T),
  matrix(c(0,1,0,0,0,1,0,0,0), ncol=3, byrow=T),
  matrix(c(0,1,1,0,0,0,0,0,0), ncol=3, byrow=T),
  
  matrix(c(0,0,0,1,0,0,0,1,0,0,0,1,0,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0), ncol=4, byrow=T),
  
  matrix(c(0,1,0, 0,0,1, 0,1,0), ncol=3, byrow=T),
  matrix(c(0,1,0, 0,0,1, 1,0,0), ncol=3, byrow=T),
  matrix(c(0,1,0, 1,0,1, 0,0,0), ncol=3, byrow=T),
  
  
  matrix(c(0,1,0,0, 0,0,1,0, 0,1,0,1, 0,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,0,0, 0,0,1,0, 0,0,0,1, 1,0,0,0), ncol=4, byrow=T),
  matrix(c(0,1,0,0, 0,0,1,0, 1,0,0,1, 0,0,0,0), ncol=4, byrow=T)
  
)
tgixs<-c()
for (i in 1:length(tgs)) {
  tgixs[i]<-get_ix(tgs[[i]])
}

df.tw$graph<-tgixs[df.tw$trial_type]

str(df.tw)

df.tw<-mutate(df.tw,
              ppt = as.numeric(ppt),
              upi = factor(upi, levels=df.sw$upi),
              cyclic = as.numeric(trial_type)>6,
              trial_type = factor(trial_type, levels=1:12))



tw_by_upi<-filter(df.tw, practice==F) %>% group_by(ppt)


df.sw$final_score<-unlist(summarise(tw_by_upi, sum(score))[,2])
df.sw$final_bonus<-unlist(summarise(tw_by_upi, sum(bonus))[,2])
df.sw$final_acc<-df.sw$final_score/54
df.sw$bonus_acc<-df.sw$final_bonus/54

save(file='../data/prolific_data.rdata', df.sw, df.tw, DBN3, DBN4, get_ix, tgixs)


#######################
#Events
#######################

df.ev<-data.frame(matrix(0, 0, 17), stringsAsFactors = F)
ev_col_names<-c("ppt", "upi", "learn_cond", "delay_cond","trial", "trial_type", "n_nodes", "n_ints",
                "score", "accum_score", "bonus", "accum_bonus", "location", "time", "type", "from", "with_delay")
names(df.ev)<-ev_col_names

#Loop over participants
for (i in df.sw$ppt)
{
  
  tmp<-unlist(strsplit(events[[sql_row_ix[i]]], '\n'))
  
  #Loop over events
  for (j in 1:length(tmp))
  {
    tmp2<-c(i, as.character(df.sw$upi)[i],
            as.character(df.sw$learn_cond)[i],
            as.character(df.sw$delay_cond)[i],
            unlist(strsplit(tmp[j], ',')))
    tmp3<-data.frame(t(tmp2), stringsAsFactors = F)
    names(tmp3)<-ev_col_names
    
    df.ev<-rbind(df.ev, tmp3)#stringsAsFactors = F
  }
  cat('events ppt', i, 'done\n')
}


df.ev<-mutate(df.ev, ppt=as.numeric(as.character(ppt)),
              upi=factor(upi, levels=df.sw$upi),
              learn_cond = factor(learn_cond, levels=c('active','passive')),
              delay_cond = factor(delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable')))

df.ev<-mutate(df.ev,
              trial = as.numeric(as.character(trial)),
              trial_type = as.numeric(as.character(trial_type)))

df.ev<-mutate(df.ev,
              ppt = as.numeric(as.character(ppt)),
              trial_type = trial_type+1,
              trial = trial-1,
              n_nodes = as.numeric(as.character(n_nodes)),
              n_ints = as.numeric(as.character(n_ints)),
              score = as.numeric(as.character(score)),
              accum_score = NULL,
              bonus = as.numeric(as.character(bonus)),
              accum_bonus = NULL)

df.ev<-mutate(df.ev,
              time = as.numeric(as.character(time)),
              type = factor(type, levels = c('a','effect'), labels = c('action','effect')),
              from =as.numeric(as.character(from)),
              location = as.numeric(as.character(location)),
              with_delay = as.numeric(as.character(with_delay))
              )

df.ev$from[df.ev$from==0]<-NA
df.ev$with_delay[df.ev$with_delay==0]<-NA
df.ev$practice<-df.ev$trial==0
df.ev<-df.ev[,c(1:11, 16, 13:15, 12)]#Nice order


head(df.ev)
str(df.ev)

for (i in 1:nrow(df.tw))
{
  tmp<-filter(df.ev, ppt == df.tw$ppt[i], trial == df.tw$trial[i]) %>% arrange(time)
  
  df.tw$n_ev[i]<-nrow(tmp)
  df.tw$n_ef[i]<-sum(tmp$type=='effect')
  
  df.tw$n_ac1[i]<-sum(tmp$type=='action' & tmp$location==1)
  df.tw$n_ac2[i]<-sum(tmp$type=='action' & tmp$location==2)
  df.tw$n_ac3[i]<-sum(tmp$type=='action' & tmp$location==3)
  df.tw$n_ac4[i]<-sum(tmp$type=='action' & tmp$location==4)
  df.tw$prop_int_root[i]<-df.tw$n_ac1[i] / df.tw$n_ints[i]
    
  df.tw$n_ef1[i]<-sum(tmp$type=='action' & tmp$location==1)
  df.tw$n_ef2[i]<-sum(tmp$type=='action' & tmp$location==2)
  df.tw$n_ef3[i]<-sum(tmp$type=='action' & tmp$location==3)
  df.tw$n_ef4[i]<-sum(tmp$type=='action' & tmp$location==4)
  
  #spaces between interventions
  tmp2<-unlist(filter(tmp, type=='action') %>% select(time) %>% arrange(time))
  
  if (length(tmp2)>1)
  {
    tmp3<-tmp2[2:length(tmp2)] - tmp2[1:(length(tmp2)-1)]
    df.tw$int_space.mean[i]<-mean(tmp3)
    df.tw$int_space.median[i]<-median(tmp3)
    df.tw$int_space.sd[i]<-sd(tmp3)
    df.tw$int_space.min[i]<-min(tmp3)
  }
  
  #Space between intervention and most recent event of any sort
  tmp4<-tmp$time[which(tmp$type=='action')[-1]] - tmp$time[which(tmp$type=='action')[-1]-1]
  if(length(tmp4>0))
  {
    df.tw$int_space_event.mean[i] = mean(tmp4)
    df.tw$int_space_event.sd[i] = sd(tmp4)
    df.tw$int_space_event.median[i] = median(tmp4)
  }

}

df.tw$n_ac4[df.tw$n_nodes==3]<-NA
df.tw$n_ef4[df.tw$n_nodes==3]<-NA
df.tw$prop_nodes_tested<-apply(select(df.tw, n_ac1:n_ac4)!=0, 1,sum, na.rm=T) /df.tw$n_nodes
head(df.tw)
str(df.tw)
#Pull the number of events, experienced variation in delays, number of interventions on each node and put in df.tw
#Make a short version that filters only the interventions?


tw_by_ppt<-df.tw %>% filter(practice==F) %>% group_by(ppt)

df.sw$n_ints<- unlist(summarise(tw_by_ppt, sum(n_ints))[,2])

df.sw$n_effects<- unlist(summarise(tw_by_ppt, sum(n_ef))[,2])
df.sw$prop_nodes_tested<-unlist(summarise(tw_by_ppt, mean(prop_nodes_tested))[,2])

df.sw$int_space.mean<- unlist(summarise(tw_by_ppt, mean(int_space.mean, na.rm=T))[,2])
df.sw$int_space.sd<- unlist(summarise(tw_by_ppt, mean(int_space.sd, na.rm=T))[,2])
df.sw$int_space_event.mean<- unlist(summarise(tw_by_ppt, mean(int_space_event.mean, na.rm=T))[,2])
df.sw$int_space_event.sd<- unlist(summarise(tw_by_ppt, sd(int_space_event.sd, na.rm=T))[,2])

df.sw$int_pref_parent<-  unlist(summarise(tw_by_ppt, sum(prop_int_root))[,2])

#cyclic/uncyclic versions
tw_by_ppt<-df.tw %>% filter(practice==F, cyclic==T) %>% group_by(ppt)

df.sw$n_ints.cy<-unlist(summarise(tw_by_ppt, sum(n_ints))[,2])
df.sw$n_effects.cy<- unlist(summarise(tw_by_ppt, sum(n_ef))[,2])
df.sw$int_space.mean.cy<- unlist(summarise(tw_by_ppt, mean(int_space.mean, na.rm=T))[,2])
df.sw$int_space.sd.cy<- unlist(summarise(tw_by_ppt, mean(int_space.sd, na.rm=T))[,2])
df.sw$int_space_event.mean.cy<- unlist(summarise(tw_by_ppt, mean(int_space_event.mean, na.rm=T))[,2])
df.sw$int_space_event.sd.cy<- unlist(summarise(tw_by_ppt, sd(int_space_event.sd, na.rm=T))[,2])
tmp <- summarise(tw_by_ppt, sum(n_ac1), sum(n_ac2), sum(n_ac3), sum(n_ac4))
df.sw$int_pref_parent.cy<-  unlist(tmp[,2]/apply(tmp[,3], 1, mean))

tw_by_ppt<-df.tw %>% filter(practice==F, cyclic==F) %>% group_by(ppt)

df.sw$n_ints.ncy<-unlist(summarise(tw_by_ppt, sum(n_ints))[,2])
df.sw$n_effects.ncy<- unlist(summarise(tw_by_ppt, sum(n_ef))[,2])
df.sw$int_space.mean.ncy<- unlist(summarise(tw_by_ppt, mean(int_space.mean, na.rm=T))[,2])
df.sw$int_space.sd.ncy<- unlist(summarise(tw_by_ppt, mean(int_space.sd, na.rm=T))[,2])
df.sw$int_space_event.mean.ncy<- unlist(summarise(tw_by_ppt, mean(int_space_event.mean, na.rm=T))[,2])
df.sw$int_space_event.sd.ncy<- unlist(summarise(tw_by_ppt, sd(int_space_event.sd, na.rm=T))[,2])
tmp <- summarise(tw_by_ppt, sum(n_ac1), sum(n_ac2), sum(n_ac3), sum(n_ac4))
df.sw$int_pref_parent.ncy<-  unlist(tmp[,2]/apply(tmp[,3], 1, mean))

#resave(file='../data/prolific_data.rdata', df.sw)

save(file='../data/prolific_data.rdata', df.sw, df.tw, df.ev, DBN3, DBN4, get_ix, tgixs)




##########
#Beliefs
##########

#Create arrays containing all 3v and 4v DAGs, put the acyclic ones at the start in the same order as you've used previously
#Create an index variable for the problem type to DAG index.
#Pull the final belief indices, number of belief changes, etc put them in df.tw
#DBNs = DAGs_3
df.be<-data.frame(matrix(0, 0, 17))
belief_names<-c("ppt", "upi", "learn_cond", "delay_cond","trial", "trial_type", "n_nodes", "n_ints",
                "score", "accum_score", "bonus", "accum_bonus","time", "belief")
names(df.be)<-belief_names

#Loop over participants
for (i in df.sw$ppt)
{
  tmp<-unlist(strsplit(beliefs[[sql_row_ix[i]]], '\n'))
  
  #Loop over belief changes
  for (j in 1:length(tmp))
  {
    tmp2<-unlist(strsplit(tmp[j], ','))
    
    tmp4<-matrix(as.numeric(tmp2[10:25]), 4,4, byrow=T)
    tmp4<-tmp4[1:as.numeric(tmp2[3]),1:as.numeric(tmp2[3])]
    
    tmp3<-data.frame(t(c(i, as.character(df.sw$upi)[i],
            as.character(df.sw$learn_cond)[i],
            as.character(df.sw$delay_cond)[i],
            tmp2[1:9], get_ix(tmp4))), stringsAsFactors = F)
    names(tmp3)<-belief_names
    df.be<-rbind(df.be, tmp3)
  }
  cat(i, 'done\n')
}

save(file='../data/prolific_data.rdata', df.sw, df.tw, df.ev, df.be, DBN3, DBN4, get_ix)


df.be<-mutate(df.be,
              ppt=as.numeric(as.character(ppt)),
              upi=factor(upi, levels=df.sw$upi),
              learn_cond = factor(learn_cond, levels=c('active','passive')),
              delay_cond = factor(delay_cond, levels = c('reliable','between', 'within')),
              trial = as.numeric(as.character(trial))-1,
              trial_type = as.numeric(as.character(trial_type)),
              trial_type = trial_type+1,
              trial_type = factor(trial_type, levels=1:12),
              n_nodes = as.numeric(as.character(n_nodes)),
              n_ints = as.numeric(as.character(n_ints)),
              score = as.numeric(as.character(score)),
              accum_score = as.numeric(as.character(accum_score)),
              bonus = as.numeric(as.character(bonus)),
              accum_bonus = as.numeric(as.character(accum_bonus)),
              time = as.numeric(as.character(time)),
              belief = as.numeric(as.character(belief)))
str(df.be)
head(df.be, 20)

df.be$practice<-df.be$trial==0

#Add accuracy etc at each stage
for (i in 1:nrow(df.be))
{
  tgix<-tgixs[df.be$trial_type[i]]
  if (df.be$n_nodes[i]==3)
  {
    pb<-DBN3[,,df.be$belief[i]]
    tg<-DBN3[,,tgix]
  
    pb_flat<-(pb + t(2*pb))[upper.tri(matrix(0, 3,3))]
    tg_flat<-(tg + t(2*tg))[upper.tri(matrix(0, 3,3))]
    
    df.be$n1_dir[i] <- pb_flat[1]
    df.be$n2_dir[i] <- pb_flat[2]
    df.be$n3_dir[i] <- pb_flat[3]
    df.be$n4_dir[i] <- NA
    df.be$n5_dir[i] <- NA
    df.be$n6_dir[i] <- NA
    
    df.be$n1_cor[i] <- pb_flat[1]==tg_flat[1]
    df.be$n2_cor[i] <- pb_flat[2]==tg_flat[2]
    df.be$n3_cor[i] <- pb_flat[3]==tg_flat[3]
    df.be$n4_cor[i] <- NA
    df.be$n5_cor[i] <- NA
    df.be$n6_cor[i] <- NA
  } else {
    pb<-DBN4[,,df.be$belief[i]]
    tg<-DBN4[,,tgix]
    
    pb_flat<-(pb + t(2*pb))[upper.tri(matrix(0, 4,4))]
    tg_flat<-(tg + t(2*tg))[upper.tri(matrix(0, 4,4))]
    
    df.be$n1_dir[i] <- pb_flat[1]
    df.be$n2_dir[i] <- pb_flat[2]
    df.be$n3_dir[i] <- pb_flat[3]
    df.be$n4_dir[i] <- pb_flat[4]
    df.be$n5_dir[i] <- pb_flat[5]
    df.be$n6_dir[i] <- pb_flat[6]
    
    df.be$n1_cor[i] <- pb_flat[1]==tg_flat[1]
    df.be$n2_cor[i] <- pb_flat[2]==tg_flat[2]
    df.be$n3_cor[i] <- pb_flat[3]==tg_flat[3]
    df.be$n4_cor[i] <- pb_flat[4]==tg_flat[4]
    df.be$n5_cor[i] <- pb_flat[5]==tg_flat[5]
    df.be$n6_cor[i] <- pb_flat[6]==tg_flat[6]
  }
  
  df.be$acc[i]<-sum(pb_flat==tg_flat)/length(pb_flat)
  
  #Pull out the final belief
  if (i<nrow(df.be))
  {
    if (df.be$trial[i]!=df.be$trial[i+1])
    {
      rowix<- which(df.tw$ppt == df.be$ppt[i] & df.tw$trial == df.be$trial[i])
      cat(i, ':', rowix, '\n')
      
      df.tw$final_belief[rowix]<-df.be$belief[i]
      df.tw$n1_dir[rowix] <- df.be$n1_dir[i]
      df.tw$n2_dir[rowix] <- df.be$n2_dir[i]
      df.tw$n3_dir[rowix] <- df.be$n3_dir[i]
      df.tw$n4_dir[rowix] <- df.be$n4_dir[i]
      df.tw$n5_dir[rowix] <- df.be$n5_dir[i]
      df.tw$n6_dir[rowix] <- df.be$n6_dir[i]
      
      df.tw$n1_cor[rowix] <- df.be$n1_cor[i]
      df.tw$n2_cor[rowix] <- df.be$n2_cor[i]
      df.tw$n3_cor[rowix] <- df.be$n3_cor[i]
      df.tw$n4_cor[rowix] <- df.be$n4_cor[i]
      df.tw$n5_cor[rowix] <- df.be$n5_cor[i]
      df.tw$n6_cor[rowix] <- df.be$n6_cor[i]
    }
  } else {
    rowix<- which(df.tw$ppt == df.be$ppt[i] & df.tw$trial == df.be$trial[i])
    cat(i, ':', rowix, '\n')
    
    df.tw$final_belief[rowix]<-df.be$belief[i]
    df.tw$n1_dir[rowix] <- df.be$n1_dir[i]
    df.tw$n2_dir[rowix] <- df.be$n2_dir[i]
    df.tw$n3_dir[rowix] <- df.be$n3_dir[i]
    df.tw$n4_dir[rowix] <- df.be$n4_dir[i]
    df.tw$n5_dir[rowix] <- df.be$n5_dir[i]
    df.tw$n6_dir[rowix] <- df.be$n6_dir[i]
    
    df.tw$n1_cor[rowix] <- df.be$n1_cor[i]
    df.tw$n2_cor[rowix] <- df.be$n2_cor[i]
    df.tw$n3_cor[rowix] <- df.be$n3_cor[i]
    df.tw$n4_cor[rowix] <- df.be$n4_cor[i]
    df.tw$n5_cor[rowix] <- df.be$n5_cor[i]
    df.tw$n6_cor[rowix] <- df.be$n6_cor[i]
  }
    
  #cat(i, ' ')
}

for (i in 1:nrow(df.tw))
{
  tmp<-filter(df.be, ppt == df.tw$ppt[i], trial == df.tw$trial[i])
  
  df.tw$n_be_ch[i]<-nrow(tmp)
  #df.tw$del_var[i]<-var(tmp$with_delay, na.rm=T)
}



df.tw$accum_score<-df.tw$accum_bonus<-df.ev$accum_score<-df.ev$accum_bonus<-
  df.be$accum_score<-df.be$accum_bonus<-NULL

save(file='../data/prolific_data.rdata', df.sw, df.tw, df.ev, df.be, DBN3, DBN4, get_ix)


#Relabel
df.tw$delay_cond<-factor(df.tw$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
df.be$delay_cond<-factor(df.be$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
df.sw$delay_cond<- factor(df.sw$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))

df.tw<-filter(df.tw, practice==F)
df.ev<-filter(df.ev, practice==F)
df.be<-filter(df.be, practice==F)


row.names(df.tw)<-NULL
row.names(df.ev)<-NULL
row.names(df.be)<-NULL
row.names(df.sw)<-NULL

#Make sure all upi variables have the same levels in the same order (dropping the ones that are no longer involved)
df.tw$upi<-factor(as.character(df.tw$upi), levels=as.character(df.sw$upi))
df.be$upi<-factor(as.character(df.be$upi), levels=as.character(df.sw$upi))
df.ev$upi<-factor(as.character(df.ev$upi), levels=as.character(df.sw$upi))
# mod_preds$upi<-factor(as.character(mod_preds$upi), levels=as.character(df.sw$upi))
df.sw$upi<-factor(as.character(df.sw$upi), levels=as.character(df.sw$upi))#(Has to go last)

df.tw<-arrange(df.tw, upi, trial_type)
df.ev<-arrange(df.ev, upi, trial_type, time)
df.be<-arrange(df.be, upi, trial_type, time)
head(df.tw)
save(file='../data/prolific_data.rdata', df.sw, df.tw, df.ev, df.be, DBN3, DBN4, get_ix, tgixs)


#######################################
#Bonus payements----
df<-data.frame(prolific_pid = as.character(df.sw$upi), bonus_amount = df.sw$final_bonus*0.05)
write.csv(df, file='../data/bonus_payments_prolific.csv')


tmp<-read.csv('../data/bonus_payments_prolificsent.csv')
