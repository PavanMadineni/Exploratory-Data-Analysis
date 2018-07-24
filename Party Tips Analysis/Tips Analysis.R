df = read.table("tips.txt",header = T)
df$tippercentage = 100*df$tip/df$total_bill
library(ggplot2)

head(df)

summary(df$tippercentage)

#Q1
ggplot(df,aes(x=tippercentage)) + geom_density(adjust=0.65) + 
  geom_vline(xintercept= quantile(df$tippercentage,probs=c(0.25,0.5,0.75)),color=c("red","blue","green"))+
  scale_x_continuous(breaks = round(seq(min(df$tippercentage)-5, max(df$tippercentage)+5, by = 4),1)) +
  ylab("Probability Density") + xlab("Tip Percentage") + ggtitle("Probability Density of Tip Percentage")


#Q2
ggplot(df, aes(sample = tippercentage)) + stat_qq() + facet_wrap(~size, ncol = 2)+
  scale_y_continuous(breaks = round(seq(min(df$tippercentage)-5, max(df$tippercentage)+5, by = 10),1)) +
  xlab("Theoretical values") + ylab("Tip Percentage") + ggtitle("QQplots for different party sizes")


#Q3
df.medians = aggregate(tippercentage ~ size, FUN = "median", data = df)

ggplot(df.medians,aes(x=size,y=tippercentage))+geom_point()+
  scale_x_continuous(breaks=seq(0,7,1))+
  xlab("Party Size(number of people)")+
  ylab("Median Tip Percentage")+
  ggtitle("Median Tip Percentages for different party sizes")