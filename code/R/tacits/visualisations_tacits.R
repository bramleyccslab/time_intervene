library(ggplot2)
library(tidyverse)
library(grid)
library(gridExtra)







########################################
#Simple gammas:

df<-data.frame(x = seq(0,3, length.out=100)) %>% 
  mutate(y1 = dgamma(x, 8, 4),
         y2 = dgamma(x, 48, 48),
         yexp = dgamma(x, 1, 1))

ggplot(df, aes(x=x)) +
  geom_area(aes(y = y1), fill='blue', alpha = .3) +
  geom_line(aes(y = y1), colour = 'black') +
  theme_bw() + 
  coord_cartesian(ylim=c(0,2)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../../figures/tacits/subparts/gamma1.pdf', width = 1.5, height = 1)


ggplot(df, aes(x=x)) +
  geom_area(aes(y = y2), fill='blue', alpha = .3) +
  geom_line(aes(y = y2), colour = 'black') +
  theme_bw() + 
  coord_cartesian(ylim=c(0,3)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../../figures/tacits/subparts/gamma2.pdf', width = 1.5, height = 1)


ggplot(df, aes(x=x)) +
  geom_area(aes(y = yexp), fill='blue', alpha = .15) +
  geom_line(aes(y = yexp), colour = 'black', linetype = 2) +
  theme_bw() + 
  coord_cartesian(ylim=c(0,2)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../../figures/tacits/subparts/exp.pdf', width = 1.5, height = 1)

df<-data.frame(x = seq(0,3000, length.out=100)) %>% 
  mutate(ychain = dgamma(x, shape = 1000, rate = 1000/1000),
         yfork = dgamma(x, shape = 1000, rate = 1000/2000),
         yshared = dgamma(x, shape= 5, rate = 5/1000))

ggplot(df, aes(x=x)) +
  geom_area(aes(y = ychain), fill='blue', alpha = .3) +
  geom_line(aes(y = ychain), colour = 'black') +
  theme_bw() + 
  # coord_cartesian(ylim=c(0,2)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../../figures/tacits/subparts/gammachain.pdf', width = 1.5, height = 1)

ggplot(df, aes(x=x)) +
  geom_area(aes(y = yfork), fill='blue', alpha = .3) +
  geom_line(aes(y = yfork), colour = 'black') +
  theme_bw() + 
  # coord_cartesian(ylim=c(0,2)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../../figures/tacits/subparts/gammafork.pdf', width = 1.5, height = 1)

ggplot(df, aes(x=x)) +
  geom_area(aes(y = yshared), fill='blue', alpha = .3) +
  geom_line(aes(y = yshared), colour = 'black') +
  theme_bw() + 
  # coord_cartesian(ylim=c(0,2)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../../figures/tacits/subparts/gammashared.pdf', width = 1.5, height = 1)
#Gamma data----
#Gamma data----
# p<-list()
# for (i in 1:100)
# {
#   set.seed(i)
#   
#   X1<-cumsum(rexp(100, rate = .01))
#   X1<-X1[X1<300]
#   print(X1)
#   X2<-cumsum(rexp(100, rate = .005))
#   X2<-c(X2, X1 + rgamma(length(X1), 3, rate = 0.2))
#   X2<-X2[X2<300]
#   print(X2)
#   X3<-cumsum(rexp(100, rate = .005))
#   X3<-c(X3, X2 + rgamma(length(X2), 3, rate = 0.2))
#   X3<-X3[X3<300]
#   print(X3)
#   
#   df<-data.frame(variables = c(rep('X1', length(X1)), rep('X2',length(X2)), rep('X3', length(X3))),
#                  events = c(X1, X2, X3)) %>% 
#     mutate(variables.f = factor(variables, levels = c('X3','X2','X1')))
#   
#   p[[i]]<-ggplot(df, aes(y=variables.f, x = events)) +
#     geom_point() +
#     theme_bw() + 
#     theme(panel.grid = element_blank())
# }
# g<-grid.arrange(grobs = p)
# ggsave('search_point_event_sequences.pdf', g, width = 20, height = 20)


set.seed(17)

  X1<-cumsum(rexp(100, rate = .01))
  X1<-X1[X1<300]
  print(X1)
  X2<-cumsum(rexp(100, rate = .005))
  X2<-c(X2, X1 + rgamma(length(X1), 5,1))
  X2<-X2[X2<300]
  print(X2)
  X3<-cumsum(rexp(100, rate = .005))
  X3<-c(X3, X2 + rgamma(length(X2), 48,8))
  X3<-X3[X3<300]
  print(X3)
  
  df<-data.frame(variables = c(rep('X1', length(X1)), rep('X2',length(X2)), rep('X3', length(X3))),
                 events = c(X1, X2, X3)) %>% 
    mutate(variables.f = factor(variables, levels = c('X3','X2','X1')))
  
ggplot(df, aes(y=variables.f,  x = events)) +
  geom_point(size = 6, shape = 21, colour = 'black', fill='white') +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable)) +fill = variables.f,
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  labs(y='', x='Time', colour = 'Variable') +
  theme_bw() + 
  scale_fill_hue(direction = -1, h.start = 90) +
  theme(panel.grid.minor.y= element_blank(),
        panel.grid.minor.x= element_blank(),
        panel.grid.major.x= element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = 'none') +
ggsave('../../../figures/tacits/subparts/delay_data.pdf', width = 6, height = 2)



ggplot(df, aes(y=variables.f,  x = events)) +
  # geom_point(size = 6, shape = 21, colour = 'black', fill='white') +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable)) +fill = variables.f,
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  labs(y='', x='Time', colour = 'Variable') +
  theme_bw() + 
  scale_fill_hue(direction = -1, h.start = 90) +
  theme(panel.grid.minor.y= element_blank(),
        panel.grid.minor.x= element_blank(),
        panel.grid.major.x= element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = 'none') +
  ggsave('../../../figures/tacits/subparts/delay_data_empty.pdf', width = 6, height = 2)


#Chain delay data
rm(list=ls())
load('chain_vs_cc_evidence_gamma.rdata')


set.seed(1)
df<-data.frame(trial.ord = factor(1:12), trial = factor(sample(12)), X1 = 0, X2 = data[,1,1], X3  = data[,2,1])
df.l<-df %>% gather(var, val, X1:X3)
ggplot(df.l, aes(x=val, y=trial, fill=var)) +
  geom_point(size = 7, shape = 21) +
  theme_bw() + 
  scale_fill_grey(start = 1, end = 0.6) +
  # coord_cartesian(xlim=c(0,3000)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = 'none') +
  ggsave('../../../figures/tacits/subparts/chain_propagation.pdf', width = 3, height = 3)

#Fork delay data
df<-data.frame(trial.ord = factor(1:12), trial = factor(sample(12)), X1 = 0, X2 = data[,1,3], X3  = data[,2,3])
df.l<-df %>% gather(var, val, X1:X3)
ggplot(df.l, aes(x=val, y=trial, fill=var)) +
  geom_point(size = 7, shape = 21) +
  theme_bw() + 
  scale_fill_grey(start = 1, end = 0.6) +
  # coord_cartesian(xlim=c(0,3000)) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        legend.position = 'none') +
  ggsave('../../../figures/tacits/subparts/fork_propagation.pdf', width = 3, height = 3)




#############################################
#Simple visulalization OU

df<-data.frame(x=seq(-10, 40, length.out = 100)) %>% 
  mutate(x1tprime = dnorm(x, mean = 0, sd = 4),
         x2tprime = dnorm(x, mean = 8, sd = 4))


ggplot(df, aes(x=x)) +
  geom_area(aes(y = x1tprime), fill='green', alpha = .15) +
  geom_line(aes(y = x1tprime), colour = 'black', linetype = 2) +
  geom_vline(xintercept = 0, colour = 'black') +
  theme_bw() + 
  coord_cartesian(xlim=c(-25, 25)) +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank()) +
  ggsave('../../figures/tacits/subparts/random_walk.pdf', width = 1.5, height = 1)

ggplot(df, aes(x=x)) +
  geom_area(aes(y = x2tprime), fill='green', alpha = .15) +
  geom_line(aes(y = x2tprime), colour = 'black', linetype = 1) +
  geom_vline(xintercept = 0, colour = 'black') +
  geom_vline(xintercept = 30, colour = 'black', linetype = 3) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank())+
  ggsave('../../figures/tacits/subparts/OU.pdf', width = 1.5, height = 1)

#####################################################
#A general visualization on observing vs intervening
####################################################
rm(list=ls())

source('tacits/OU_generative.R')


df.cbn<-data.frame(
  variable = c('A','B','C'),
  time =
    event = intervention
)



#SIMULATE SOME CTCV
if(0) {
  betas <- array(c(-1,0,0,1,-1,0,0,0,-1), dim=c(3,3))
  vars  <- c(100,0,0)
  blah <- 0
  nruns <- 100000
  for (e in 1:nruns) {
    blah <- blah + OU_general(vars=c(100,0,0),betas = betas)
  }
  print(blah/nruns)
}

betas <- array(c(0,0,0,1,0,0,0,0,0), dim=c(3,3))
betas <- array(c(0,0,0,1,0,0,-2,0,0), dim=c(3,3))

#Chain
betas <- matrix(c(0.75,1.25,0,
                  0,0,-1.25,
                  0,0,0), ncol = 3, byrow = T)

betas

#No interventions
no_interventions<-matrix(NA, nrow=250, ncol=3)

#Large sine wave interventions
interventions<-matrix(NA, nrow=250, ncol=3)
interventions[51:100, 1]<-sin(seq(0,6*pi,length.out=50)) * 80
interventions[111:160, 3]<-sin(seq(0,6*pi,length.out=50)) * 80

#Static interventions
interventions<-matrix(NA, nrow=250, ncol=3)
interventions[31:70, 1]<--50
interventions[101:150, 2]<-50
interventions[191:230, 1]<-sin(seq(0,2*pi,length.out=40)) * -50+40
#Sine wave interventiosn
# interventions<-matrix(NA, nrow=200, ncol=3)
# interventions[101:140, 1]<-sin(seq(0,3*pi,length.out=40)) * 40 + 30
# interventions[151:190, 2]<-sin(seq(1,4*pi,length.out=40)) * 40 - 30



set.seed(101)
out.ni<-OU_general_run(vars = c(33,0,-33), betas = betas, interventions = no_interventions, ts_len = 250)
set.seed(101)
out<-OU_general_run(vars = c(33,0,-33), betas = betas, interventions = interventions, ts_len = 250)



df<-out.ni %>% data.frame %>% 
  setNames(., c("X[1]", "X[2]", "X[3]")) %>%
  gather(variable, value, "X[1]":"X[3]") %>%
  mutate(timestep = rep(1:nrow(out.ni), 3))

ggplot(df, aes(x=timestep, y=value, colour = variable, linetype = variable)) +
  geom_line() +
  labs(y='Value', x='Time', colour = '', linetype = '') +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable), guide = F) +
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  coord_cartesian(ylim=c(-100,100)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10)) +
  ggsave('../../figures/tacits/subparts/ctcv_observe.pdf', width = 5, height = 2)


df<-out %>% data.frame %>% 
  setNames(., c("X[1]", "X[2]", "X[3]")) %>%
  gather(variable, value, "X[1]":"X[3]") %>%
  mutate(timestep = rep(1:nrow(out), 3))

df$interventions<-c(rbind(c(NA,NA, NA),
                          interventions[1:nrow(interventions)-1,]))

ggplot(df, aes(x=timestep, y=value, linetype = variable, colour = variable)) +
  geom_line() +
  geom_line(aes(y=interventions, group=variable), colour = 'black',
            linetype = 2, size = 0.75) +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable)) +
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  labs(y='Value', x='Time', linetype = 'Variable', colour = 'Variable') +
  coord_cartesian(ylim=c(-100,100)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10)) +
  ggsave('../../figures/tacits/subparts/ctcv_full.pdf', width = 6, height = 2)





# df.imp<-df %>% filter(timestep %in% c(25, 75, 102, 120, 152, 170))
# df.imp$value[df.imp$timestep %in% c(102, 152) & is.na(df.imp$interventions)]<-NA
# df.imp$alpha<-1
# df.imp$alpha[df.imp$timestep %in% c(120, 170) & !is.na(df.imp$interventions)]<-.5
# df.imp$time<-c(1,2, 2.75, 3.25, 3.75, 4.25)
# 
# df.lines<-data.frame(x1 = c(102, 152),
#                      x2= c(120, 170),
#                      y1 = c(50, -50),
#                      y2 = c(50, -50))
# 
# ggplot(df.imp) +
#   geom_segment(data = df.lines, aes(y=y1, yend = y2, x=x1, xend = x2), colour = 'black') +
#   geom_point(aes(x=timestep, y=interventions, group=variable), colour = 'black', size = 6, alpha = .5) +
#   geom_point(aes(x=timestep, y=value, colour = variable, alpha = alpha), size = 4) +
#   labs(y='Value', x='Time', colour = 'Variable') +
#   coord_cartesian(ylim=c(-100,100), xlim=c(0,250)) +
#   theme_bw() + 
#   scale_alpha_continuous(guide = F) +
#   theme(panel.grid = element_blank(),
#         axis.text.x=element_blank(),
#         axis.ticks.x=element_blank()) +
#   ggsave('../../figures/tacits/ctcv_obscured.pdf', width = 6, height = 2)
# 
# 
# cormat<-matrix(c(1,
#                  cor((df %>% filter(variable =='x1') %>% select(value))$value[1:199],
#                      (df %>% filter(variable =='x2') %>% select(value))$value[2:200]),
#                  cor((df %>% filter(variable =='x1') %>% select(value))$value[1:199],
#                      (df %>% filter(variable =='x3') %>% select(value))$value[2:200]),
#                  
#                  cor((df %>% filter(variable =='x2') %>% select(value))$value[1:199],
#                      (df %>% filter(variable =='x3') %>% select(value))$value[2:200]),
#                  1,
#                  cor((df %>% filter(variable =='x2') %>% select(value))$value[1:199],
#                      (df %>% filter(variable =='x1') %>% select(value))$value[2:200]),
#                  
#                  cor((df %>% filter(variable =='x3') %>% select(value))$value[1:199],
#                      (df %>% filter(variable =='x1') %>% select(value))$value[2:200]),
#                  cor((df %>% filter(variable =='x3') %>% select(value))$value[1:199],
#                      (df %>% filter(variable =='x2') %>% select(value))$value[2:200]),
#                  1), ncol = 3, byrow = T)
# cormat
# cor(df$value[df$variable =='x1'], df$value[df$variable =='x2'])
# cor(df$value[df$variable =='x1'], df$value[df$variable =='x3'])
# cor(df$value[df$variable =='x2'], df$value[df$variable =='x3'])


# ggplot(df, aes(x=timestep, y=value, colour = variable)) +
#   geom_line() +
#   facet_wrap(~ variable, ncol=1) +
#   theme_bw() + 
#   theme(panel.grid = element_blank())


df<-data.frame(x = seq(0,10, length.out = 250)) %>%
  mutate(exp = dexp(x, rate = 1),
         prevent1 = dgamma(x, shape = 2, rate = 1),
         prevent.5 = dgamma(x, shape = 1.5, rate = 1),
         prevent_mixed = dexp(x, rate = 1)*.35 + dgamma(x, shape = 2, rate = 1)*.765)

df.l<-df %>% gather(key, val, prevent1:prevent_mixed)
ggplot(df.l, aes(x=x, y = val, colour = key))+geom_line() +theme_bw()




############################
#OU emergent behaviour
#############################

rm(list=ls())

source('OU_generative.R')


#OSCILIATOR INHIBITED
betas <- matrix(c(0,.5,
                  .5,0), ncol = 2, byrow = T)

betas

#No interventions
no_interventions<-matrix(NA, nrow=100, ncol=2)

set.seed(101)
out.ni<-OU_general_run(vars = c(100,-100), betas = betas, interventions = no_interventions, ts_len = 100)

df<-out.ni %>% data.frame %>% 
  setNames(., c("X[1]", "X[2]")) %>%
  gather(variable, value, "X[1]":"X[2]") %>%
  mutate(timestep = rep(1:nrow(out.ni), 2))

ggplot(df, aes(x=timestep, y=value, colour = variable, linetype = variable)) +
  geom_line() +
  labs(y='Value', x='Time', colour = '', linetype = '') +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable), guide = F) +
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  coord_cartesian(ylim=c(-100,100)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10)) +
  ggsave('../../../figures/tacits/subparts/ctcv_inhibit.pdf', width = 2, height = 2)



#OSCILIATOR EXCITED
betas <- matrix(c(0,2,
                  2,0), ncol = 2, byrow = T)

betas

#No interventions
no_interventions<-matrix(NA, nrow=100, ncol=2)

set.seed(101)
out.ni<-OU_general_run(vars = c(100,-100), betas = betas, interventions = no_interventions, ts_len = 100, sigma =5)

df<-out.ni %>% data.frame %>% 
  setNames(., c("X[1]", "X[2]")) %>%
  gather(variable, value, "X[1]":"X[2]") %>%
  mutate(timestep = rep(1:nrow(out.ni), 2))

ggplot(df, aes(x=timestep, y=value, colour = variable, linetype = variable)) +
  geom_line() +
  labs(y='Value', x='Time', colour = '', linetype = '') +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable), guide = F) +
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  coord_cartesian(ylim=c(-100,100)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10)) +
  ggsave('../../../figures/tacits/subparts/ctcv_excite.pdf', width = 2, height = 2)



#OSCILIATOR
betas <- matrix(c(0,2,
                  -2,0), ncol = 2, byrow = T)

betas

#No interventions
no_interventions<-matrix(NA, nrow=200, ncol=2)

set.seed(101)
out.ni<-OU_general_run(vars = c(5,-5), betas = betas, interventions = no_interventions, ts_len = 200, sigma = 5)

df<-out.ni %>% data.frame %>% 
  setNames(., c("X[1]", "X[2]")) %>%
  gather(variable, value, "X[1]":"X[2]") %>%
  mutate(timestep = rep(1:nrow(out.ni), 2))

ggplot(df, aes(x=timestep, y=value, colour = variable, linetype = variable)) +
  geom_line() +
  labs(y='Value', x='Time', colour = '', linetype = '') +
  # scale_linetype_discrete(labels = function(variable) parse(text=variable), guide = F) +
  # scale_colour_discrete(labels = function(variable) parse(text=variable)) +
  coord_cartesian(ylim=c(-100,100)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'bottom',
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-10,-10,0,-10)) +
  ggsave('../../../figures/tacits/subparts/ctcv_oscillate.pdf', width = 2, height = 2)
