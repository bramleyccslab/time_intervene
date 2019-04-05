#Create artificial data for demonstration purposes
library(rjson)
library(dplyr)
library(tidyr)


head(df.ev)

#Artificial data
df.a<-data.frame(location=c(3,2,1,1, 2,3, 3,2),type=c(rep('action', 4), rep('effect', 4)),
                 time=c(2000, 6000, 10000, 14000, 11000, 11500, 15500, 16000))
df.a %>% arrange(time)

#Df by frame
df.f<-data.frame(A = rep(0, 20*60), B = rep(0, 20*60), C = rep(0, 20*60))
ints<-filter(df.a, type== 'action')
effs<-filter(df.a, type=='effect')
for (i in 1:(20*60))
{
  time = i*(1000/60)
  #Effect
  if (any(time>=effs$time & time<=(effs$time+300) & effs$location ==1))
  {
    df.f$A[i]<-1
  }
  if (any(time>=effs$time & time<=(effs$time+300) & effs$location ==2))
  {
    df.f$B[i]<-1
  }
  if (any(time>=effs$time & time<=(effs$time+300) & effs$location ==3))
  {
    df.f$C[i]<-1
  }
  
  #Action
  if (any(time>ints$time & time<(ints$time+300) & ints$location ==1))
  {
    df.f$A[i]<-2
  }
  if (any(time>ints$time & time<(ints$time+300) & ints$location ==2))
  {
    df.f$B[i]<-2
  }
  if (any(time>ints$time & time<(ints$time+300) & ints$location ==3))
  {
    df.f$C[i]<-2
  }

}

json_data<-toJSON(df.f)
write(file='../python/cc_example_data.json', json_data)