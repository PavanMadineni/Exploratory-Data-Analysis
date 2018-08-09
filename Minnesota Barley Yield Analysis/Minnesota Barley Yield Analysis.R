df = read.table("minnesota.barley.yield.txt",header = T)
library(ggplot2)
attach(df)

#Q1
ggplot(df,aes(x=yield)) + geom_histogram(bins=20)
ggplot(df,aes(x=year,y=yield,color = gen)) + geom_line()+facet_wrap(~site,ncol=2)
ggplot(df,aes(x=site,y=yield,color=site)) + geom_line()+facet_wrap(~year,ncol=3)

#As we can observe that there is some missing data for the type of Barley
#We can interpret that as at that particular site they have gone for a different
#crop or the data is missing for that particular time period
#We can see that the GrandRapids StPaul and Waseca have a similar pattern of yield



#We can also say that the the direction of change of yield for different
#types of Barley is same with in a site over the time. So, for simplicity we
#can group and aggregate them together
df.ag = aggregate(yield ~ year+site, data=df, sum)
ggplot(df.ag,aes(x=year, y=yield)) + geom_point()+ geom_line()+
  facet_wrap(~site,ncol=2)



library(dplyr)
library(tidyr)
df.ag.sp = spread(df.ag, key=site, value=yield)
#There is one missing value at Crookston at 1928
#There are 2 missing values at Morris on 1933 and 1934

#For checking increasing or decreasing trend we will go for yield's successive 
#difference overthe years. 

df.ag.sp.df = data.frame(lapply(df.ag.sp,diff))
df.ag.sp.df$year = c(1928:1936)
#convert in to long form
df.lg.dif = gather(df.ag.sp.df, key =site,Crookston:Waseca, value = yield)

ggplot(df.lg.dif,aes(x=year, y=yield)) + geom_point()+ geom_line() +
  facet_wrap(~site,ncol=3) + geom_hline(yintercept = 0)






#Q2 
library(MASS)
df.rlm = rlm(yield ~ gen+year*site, psi = psi.bisquare,data = df)
#we are considering only year and site interaction with gen as yield is almost
#uniform for all the varieties of Barley.
#We are going for robust linear model as this will take care of all the outliers
#
library(broom)
df.rlm.au = augment(df.rlm)

ggplot(df.rlm.au, aes(y= .resid, x= year)) + geom_line()+ geom_smooth(span=0.75)+
  facet_wrap(~site,ncol=2) 




#Q3
ggplot(df.rlm.au, aes(y= .resid, x= .fitted)) + geom_point()+geom_smooth(span =0.5)+
  facet_wrap(~site,ncol=2) + geom_hline(yintercept = 0)

var(df.rlm.au$.fitted)/var(df.rlm.au$yield)







#from notes
ggplot(df, aes(x = yield, y = gen, color = factor(year))) +
  geom_line() + facet_wrap(~site)
barley = df = read.table("minnesota.barley.yield.txt",header = T)

barley.rlm = rlm(yield ~ gen + year +  site, psi = psi.bisquare, data = barley)
var(fitted.values(barley.rlm))/var(barley$yield)
