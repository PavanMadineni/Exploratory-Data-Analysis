df = read.table("rustdrugs2016.txt", header = T)
df$State = str_sub(as.vector(df$County), start =-2)
df$DM = 100000*df$Deaths/df$Population
df$IM = df$Income/1000
df$TM = 100*df$Trump
df$logdm = log10(df$DM)
df = subset(df, df$IM < 70)


dfw = df[, c("DM","IM","TM","PctWhite")]

#install.packages("stringr")
library(stringr)
library(ggplot2)
library(GGally)
library(broom)
library(tidyr)
library(ggplot2)
df$State = str_sub(as.vector(df$County), start =-2)


ggpairs(dfw)


#Highly suited graph for explanation
ggplot(dfw, aes(y= DM, x=IM)) + geom_point() +
  geom_smooth(span = 1,formula = y ~ x, method.args = list(degree = 1),se = FALSE)+
  facet_wrap(~cut_number(TM, n =2 ) + cut_number(PctWhite, n = 2))  




ggplot(dfw, aes(y= TM, x=PctWhite)) + geom_point() + 
  geom_abline(intercept = -20 , slope = 1, color = "blue") +
  geom_abline(intercept = -45 , slope = 1, color = "blue")



attach(dfw)

crop = (PctWhite > 65) & (TM > (PctWhite - 45)) & (TM < (PctWhite - 20)) & (TM < 78) 

dfwc = dfw[crop,]


ggpairs(dfwc)

dfwc.lm = lm(DM ~ IM*PctWhite*TM)


attach(dfwc)
lm.grid = expand.grid(IM= min(IM):max(IM), TM= c(21,58,76), PctWhite = c(55,92,98))
DM.predict = predict(dfwc.lm,lm.grid)
df.DM = data.frame(lm.grid, fit = as.vector(DM.predict))

ggplot(df.DM, aes(y= fit, x= IM)) + geom_line() + facet_wrap(~TM +PctWhite, drop = FALSE)


dfwc.lm.au = augment(dfwc.lm)
ggplot(dfwc.lm.au, aes(y= .resid, x= .fitted)) + geom_point() +
  geom_smooth() + geom_hline(yintercept = 0, color= "red")


#var(dfwc.lm.au$.fitted)/(var(dfwc.lm.au$.fitted) + var(dfwc.lm.au$.resid))
var(dfwc.lm.au$.fitted)/(var(dfwc.lm.au$DM))
cor(dfwc.lm.au$.fitted,dfwc.lm.au$DM )






#New approach as correlation b/w TM and PctWhite is high 0.746 we consider only one inorder not to
# complicate the things


df = read.table("rustdrugs2016.txt", header = T)
df$State = str_sub(as.vector(df$County), start =-2)
df$DM = 100000*df$Deaths/df$Population
df$IM = df$Income/1000
df$TM = 100*df$Trump
df$logdm = log10(df$DM)
df = subset(df, df$IM < 70)


dfw = df[, c("DM","IM","TM","PctWhite")]
ggplot(dfw, aes(y= TM, x=PctWhite)) + geom_point() + 
  geom_abline(intercept = -20 , slope = 1, color = "blue") +
  geom_abline(intercept = -45 , slope = 1, color = "blue")

#install.packages("stringr")
library(stringr)
library(ggplot2)
library(GGally)
library(broom)
library(tidyr)
library(ggplot2)
df$State = str_sub(as.vector(df$County), start =-2)


ggpairs(dfw)

ggplot(dfw, aes(y= DM, x=TM)) + geom_point() +
  geom_smooth(span = 1,formula = y ~ x, method.args = list(degree = 1),se = FALSE) +
  facet_wrap(~cut_number(IM, n =4)) + 
  ggtitle(" Death rates vs Trump Votes percentage over different income levels")+
  ylab("Drug Related Death rates per 100,000 population ") +
  xlab("Percentage of trump votes")

ggplot(dfw, aes(y= DM, x=TM)) + geom_point() +
  geom_smooth(span = 1,formula = y ~ x, method.args = list(degree = 1),se = FALSE) +
  facet_wrap(~cut_number(IM, n =3) + ~cut_number(PctWhite, n =3)) + 
  ggtitle("Left to right: Increasing % of White Population; Top to bottom: Increasing Income")+
  ylab("Drug Related Death rates per 100,000 population ") +
  xlab("Percentage of trump votes")


attach(dfw)
dfw.lm = lm(DM ~ IM*TM)
lm.grid = expand.grid(TM= min(TM):max(TM), IM= c(33,43,50,54,65,70) )
DM.predict = predict(dfw.lm,lm.grid)
df.DM = data.frame(lm.grid, fit = as.vector(DM.predict))

ggplot(df.DM, aes(y= fit, x= TM)) + geom_line() + facet_wrap(~IM, drop = FALSE)+
  xlab("Percentage of trump votes")+
  ggtitle("Fitted Dearh rate vs % of Trump votes at different income levels")

ggplot(df.DM, aes(y= fit, x= TM, group = IM,color = IM)) +
  geom_line() +xlab("Percentage of trump votes")+
  xlab("Percentage of trump votes")+
  ggtitle("Fitted Dearh rate vs % of Trump votes at different income levels")
# For higher income levels the death rate has a little effect from 
# the percentage of votes that went to Trump on 2016 election
# But for lower income levels percentage of votes for Trump has a significant effect
# on the Death rate
                              
#So from this plot we can infer that we can drop the higher income as it is not following our trend

dfw.lm.au = augment(dfw.lm)
ggplot(dfw.lm.au, aes(y= .resid, x= .fitted)) + geom_point() +
  geom_smooth() + geom_hline(yintercept = 0, color= "red")

var(dfw.lm.au$.fitted)/(var(dfw.lm.au$DM))

min(IM)
max(IM)
dim(df[df$IM > 70,])

