library(NHANES)
data = NHANESraw
data = data[,c("BPSysAve","Age","Weight","Height", "WTMEC2YR","Gender")]

colnames(data)[5] = "Survey_Weights"

head(data)

sapply(data, function(x) sum(is.na (x)))

data = data[complete.cases(data),]
sapply(data, function(x) sum(is.na (x)))
dim(data)

total_weights = sum(data$Survey_Weights)
data$Survey_Weights = data$Survey_Weights/total_weights
head(data)

library(repr)
options(repr.plot.width = 8, repr.plot.height = 5)
library(GGally)
ggpairs(data = data[, -5], title="NHANES data", mapping = ggplot2::aes(colour = Gender))

Age = data$Age
Weight = data$Weight
Height = data$Height

Age.cat = rep(NA, nrow(data))
Age.cat[Age <= quantile(Age, 1/3)] = "Young"
Age.cat[Age > quantile(Age, 1/3) & Age <= quantile(Age, 2/3)] = "Medium Age"
Age.cat[Age > quantile(Age, 2/3)] = "Old"
Age.cat = factor(Age.cat, levels = c("Young", "Medium Age", "Old Age"))

Weight.cat = rep(NA, nrow(data))
Weight.cat[Weight <= quantile(Weight, 1/3)] = "Light"
Weight.cat[Weight > quantile(Weight, 1/3) & Weight <= quantile(Weight, 2/3)] = "Medium Weight"
Weight.cat[Weight > quantile(Weight, 2/3)] = "Heavy"
Weight.cat = factor(Weight.cat, levels = c("Light", "Medium Weight", "Heavy"))

Height.cat = rep(NA, nrow(data))
Height.cat[Height <= quantile(Height, 1/3)] = "Short"
Height.cat[Height > quantile(Height, 1/3) & Height <= quantile(Height, 2/3)] = "Medium Height"
Height.cat[Height > quantile(Height, 2/3)] = "Tall"
Height.cat = factor(Height.cat, levels = c("Short", "Medium Height", "Tall"))

data.cat = data.frame(data, Age.cat, Weight.cat, Height.cat)


library(ggplot2)
ggplot(data.cat, aes(x = Age, y = BPSysAve, size = Survey_Weights)) + geom_point(alpha = 0.5) +
  geom_smooth(span = 1, method = lm) + 
  facet_grid(Height.cat~ Weight.cat + Gender)

data.lm = lm(BPSysAve ~ Age + Weight + Height + Gender, data = data, weights = Survey_Weights)
lm.grid = expand.grid(Age = seq(10, 80, 10), Height = seq(120, 200, 20),
                      Weight = seq(20, 220, 55),
                      Gender = c("female", "male"))
data.predict = predict(data.lm, newdata = lm.grid)
data.df = data.frame(lm.grid, fit = as.vector(data.predict))

ggplot(data.df, aes(x = Age, y = fit, color = Gender)) + geom_smooth() + facet_wrap(~ Weight + Height) +
  ggtitle('Left to Right: Increasing Height; Top to Bottom: Increasing Weight') + ylab('Average Systolic Blood Pressure')

# Q2---------------------------------------------------------

Diab = NHANESraw[,c("BPSysAve","Age","Weight","Height", "WTMEC2YR","Gender", "Diabetes")]

Diab = Diab[complete.cases(Diab),]
sapply(Diab, function(x) sum(is.na (x)))
dim(Diab)

Diab$Diabetes = as.numeric(Diab$Diabetes)
Diab$Diabetes = Diab$Diabetes - 1

ggplot(Diab, aes(x = Age, y = Diabetes, color = Gender)) + geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~(cut_number(Height, n = 3)) + ~(cut_number(Weight,n = 3))) + 
  ggtitle('Left to Right: Increasing Weight; Top to Bottom: Increasing Height') + 
  ylab('Chance of Diabetes (0 - No, 1 - Yes)')

head(Diab)

Diab.glm = glm(Diabetes ~ Age + Weight + Height + Gender, data = Diab, family = "binomial")
glm.grid = expand.grid(Age = seq(10, 80, 10), Height = seq(120, 200, 20),
                       Weight = seq(20, 220, 55),
                       Gender = c("female", "male"))
Diab.predict = predict(Diab.glm, newdata = glm.grid, type = "response")
Diab.df = data.frame(glm.grid, fit = as.vector(Diab.predict))

ggplot(Diab.df, aes(x = Age, y = fit, color = factor(Weight))) + geom_smooth() + 
  facet_wrap(~ Gender + Height) +
  ggtitle('Chance of being diabetic at different Ages, Heights and Weights') + 
  ylab('Predicted Chance of Having Diabetes')


#Q3----------------------------------------------------------------------------------------


Diab2 = NHANESraw[,c("Age","Weight","Height", "WTMEC2YR","Gender", "Diabetes", "SleepHrsNight")]

Diab2 = Diab2[complete.cases(Diab2),]
sapply(Diab2, function(x) sum(is.na (x)))
dim(Diab2)

Diab2$Diabetes = as.numeric(Diab2$Diabetes)
Diab2$Diabetes = Diab2$Diabetes - 1
Diab2$SleepHrsNight = as.numeric(Diab2$SleepHrsNight)

unique(Diab2$SleepHrsNight)

Diab2$SleepHrsNight[Diab2$SleepHrsNight >= 2 & Diab2$SleepHrsNight <= 6] = "Lack of Sleep"
Diab2$SleepHrsNight[Diab2$SleepHrsNight >= 7 & Diab2$SleepHrsNight <= 9] = "Healthy Sleep"
Diab2$SleepHrsNight[Diab2$SleepHrsNight >= 10 & Diab2$SleepHrsNight <= 12] = "Excess Sleep"
Diab2$SleepHrsNight = factor(Diab2$SleepHrsNight, levels = c("Lack of Sleep", "Healthy Sleep", "Excess Sleep"))

unique(Diab2$SleepHrsNight)

summary(Diab2$SleepHrsNight)

ggplot(Diab2, aes(x = Age, y = Diabetes, color = Gender)) + geom_point() +
  geom_smooth(method = "loess") +
  facet_wrap(~ SleepHrsNight + ~(cut_number(Weight,n = 3))) + 
  ggtitle('Left to Right: Increasing Weight; Top to Bottom: Change of Sleep Pattern') + 
  ylab('Chance of Diabetes (0 - No, 1 - Yes)')

Diab2.glm = glm(Diabetes ~ Age + Weight + Height + Gender + SleepHrsNight, data = Diab2, family = "binomial")
glm2.grid = expand.grid(Age = seq(10, 80, 10), Height = seq(120, 200, 20),
                        Weight = seq(20, 220, 55),
                        Gender = c("female", "male"),
                        SleepHrsNight = c("Lack of Sleep","Healthy Sleep", "Excess Sleep")  )
Diab2.predict = predict(Diab2.glm, newdata = glm2.grid, type = "response")
Diab2.df = data.frame(glm2.grid, fit = as.vector(Diab2.predict))

ggplot(Diab2.df, aes(x = Age, y = fit, color = factor(Weight))) + geom_smooth() + 
  facet_wrap(~ SleepHrsNight + Height) +
  ggtitle('Chance of being diabetic at different Ages, Heights, Weights and Sleep Patterns') + 
  ylab('Predicted Chance of Having Diabetes')
















