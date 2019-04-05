rm(list=ls())
load('../data/pilot_data.rdata')


df.tw<-filter(df.tw, practice==F, delay_cond!='between')
df.ev<-filter(df.ev, practice==F, delay_cond!='between')
not_between_ix <- df.be$delay_cond[df.be$practice==F] !='between'
df.be<-filter(df.be, practice==F, delay_cond!='between')
df.sw<-filter(df.sw, delay_cond!='between')

df.tw$delay_cond<-factor(df.tw$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
df.ev$delay_cond<-factor(df.ev$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
df.be$delay_cond<-factor(df.be$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))
df.sw$delay_cond<- factor(df.sw$delay_cond, levels = c('reliable','within'), labels = c('reliable','unreliable'))

df.sw$alpha<-5
df.sw$beta<-0.003333333
df.sw$alpha[df.sw$delay_cond=='reliable']<-200
df.sw$beta[df.sw$delay_cond=='reliable']<-0.1333333

row.names(df.tw)<-NULL
row.names(df.ev)<-NULL
row.names(df.be)<-NULL
row.names(df.sw)<-NULL

save(file='../data/cogsci_data.rdata',df.tw, df.ev, df.be, df.sw, df.tw, DBN3, DBN4, tgixs, get_ix)
