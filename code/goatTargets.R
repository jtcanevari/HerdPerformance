#simulate a herd of goats to estimate target rep parameters

#load libraries
library(dplyr)

#define parameters
mx <- 21 #mean oestrus cylce (Castro et al 1999, mean=20.8,sem=0.4)
sem <- 1 #standard error of the mean
sem*sqrt(6) -> sdx #standard deviation (n=6)
det=0.80 #detection of oestrus
pRate <- 0.60 #conception rate
ngoats = 10000 #number of goats to simulate
ncycles <- 3 #how many cycles to follow

#create data frame
id<- rep(1:ngoats, each=ncycles) 
oestNumber <- rep(1:ncycles,ngoats)
day <- NA
df <- cbind(id, oestNumber,day)
head(df)
df<-data.frame(df)

df$day[df$oestNumber==1] <- sample(1:mx, ngoats, replace = T) 
# df$day[df$oestNumber==1] <- rexp(ngoats, 1/mx) #cycle day for each goat at psm
df$day[df$oestNumber==2] <- df$day[df$oestNumber==1]+rnorm(ngoats,mean = mx, sd=sdx)
df$day[df$oestNumber==3] <- df$day[df$oestNumber==2]+rnorm(ngoats,mean = mx, sd=sdx)
df <- df %>%
  group_by(id) %>%
  mutate(diff = day - lag(day, default = first(day)))

mean(df$diff[!df$oestNumber==1]) #test if length of cycle is right

#simulate oestrus detection
df$detection <- NA
df$detection[df$oestNumber==1] <- sample(c(0,1),ngoats, prob = c(1-det,det), replace=T)
df$detection[df$oestNumber==2] <- sample(c(0,1),ngoats, prob = c(1-det,det), replace=T)
df$detection[df$oestNumber==3] <- sample(c(0,1),ngoats, prob = c(1-det,det), replace=T)

#that's 'real' but what's observed?
obs <- subset(df, detection==1) #keep only detected oestrus

#simulate conceptions
obs$pregnant <- NA
obs$pregnant[obs$oestNumber==1] <- sample(c(0,1),length(obs$pregnant[obs$oestNumber==1]), prob = c(1-pRate,pRate), replace=T)
# prop.table(table(obs$pregnant[oestNumber==1]))

obs$pregnant[obs$oestNumber==2] <- sample(c(0,1),length(obs$pregnant[obs$oestNumber==2]), prob = c(1-pRate,pRate), replace=T)

obs$pregnant[obs$oestNumber==3] <- sample(c(0,1),length(obs$pregnant[obs$oestNumber==3]), prob = c(1-pRate,pRate), replace=T)

obs$day <- floor(obs$day+1) #days don't start in 0 but in 1 and are integers
obs <- obs[with(obs, order(id, day)), ]

#rmv rows after id gets pregnant
obs$csum <- ave(obs$pregnant, obs$id, FUN=cumsum)
obs <- obs[obs$csum<=1,]
obs$temp <- obs$csum+obs$pregnant  
obs <- obs[obs$temp %in% c(0,2),]

#recalculate diff (observed)
obs <- obs[1:6] %>%
  group_by(id) %>%
  mutate(diff = day - lag(day, default = first(day)))
mean(obs$diff[!obs$diff==0]) #cycle length is a bit longer than mx because of missing cycles

#ratio 20-26 to 43-49?
#43-49
obsCycles <- obs$diff[!obs$diff==0]
hist(obsCycles)

length(obsCycles[obsCycles >= (mx-3) & obsCycles <= (mx+3)]) / length(obsCycles[obsCycles >= ((mx*2)-3) & obsCycles <= ((mx*2)+3)])

#number of services per conception
sum(table(obs$id))/length(obs$id[obs$pregnant==1]) #1.66

#submission:
#how many submitted by day 21?
length(obs$id[obs$day <= 21])/ngoats 
#how many submitted by day 28?
length(obs$id[obs$day <= 28])/ngoats 

#in-kid
length(obs$id[obs$day <= 28 & obs$pregnant==1])/ngoats #four weeks
length(obs$id[obs$day <= 28*2 & obs$pregnant==1])/ngoats #eigth weeks

#kidding-rates
#look at the first that've kidded for the start of kidding season
start <- min(obs$day[obs$pregnant==1])
length(obs$id[obs$day >= start & obs$day <= start  + 27 & obs$pregnant==1])/ngoats #four weeks
length(obs$id[obs$day >= start & obs$day <= start  + 27 + 28 & obs$pregnant==1])/ngoats #four weeks

#percentage of group no in-kid by PSM date plus 100 days
(ngoats - length(obs$id[obs$day <= 100 & obs$pregnant == 1]))/ngoats*100 #13.4%

#return intervals
# 2 to 7 days
(pnorm(7, mx, sdx) - pnorm(2, mx, sdx))*100

# 8 to 10 days
(pnorm(17, mx, sdx) - pnorm(9, mx, sdx))*100

# 17 to 19 days
(pnorm(20, mx, sdx) - pnorm(17, mx, sdx))*100

# 20 to 21 days 
(pnorm(22, mx, sdx) - pnorm(20, mx, sdx))*100

# 22 to 24 days
(pnorm(24, mx, sdx) - pnorm(22, mx, sdx))*100

# 25 to 26 days
(pnorm(26, mx, sdx) - pnorm(24, mx, sdx))*100

# 25 to 26 days
(pnorm(28, mx, sdx) - pnorm(26, mx, sdx))*100

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------

#check method for first oestrus
ngoats <- 1000
rval<- rep(NA,ngoats)
for(i in 1:ngoats){
  a=0
  j=0
  clen=rnorm(1,mx,sdx)
  while(a==0){
    a <- sample(c(0,1),1, prob = c(1-1/clen,1/clen))
    j <- j + 1
    rval[i] <- j
  }
}
plot(density(rval))
plot(rval)

tmp <- rexp(1000, 1/mx)
plot(tmp)
par(mfrow=c(1,2))
hist(tmp,20,xlim=c(0,150), ylim = c(0,400))
hist(rval,20,xlim=c(0,150), ylim = c(0,400))
dev.off()
