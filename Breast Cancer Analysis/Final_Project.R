#Loading the data
nam = c("Sample_code_number", "Clump_thickness","Uniformity_of_cell_size", "Uniformity_of_cell_shape",
        "Marginal_adhesion", "Single_Epithelial_Cell_size", "Bare_Nuclei", "Bland_Chromatin", "Normal_Nucleoli", 
        "Mitoses", "Is_Malignant")
df = read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data",
                sep = ",",col.names= nam)
dim(df)
head(df)



#Preprocessing the data
unique(df$Bare_Nuclei)
df$Is_Malignant = ((df$Is_Malignant)/2) -1
df = df[,2:11]
head(df)
df = df[df$Bare_Nuclei != "?",]
df$Bare_Nuclei = as.numeric(df$Bare_Nuclei)
df$Bare_Nuclei = df$Bare_Nuclei -1
df$Is_Malignant = as.numeric(df$Is_Malignant)




#Pair plots of our data
library(GGally)
ggpairs(df)



#Correlation heatmap of our data 
library(corrplot)
cor_df =  cor(df)
corrplot(cor_df)



#Tie fighter plot of our data
library(ElemStatLearn)
predictors.scaled = scale(df[, 1:9])
cancer = df$Is_Malignant
db.lm = lm(cancer ~ predictors.scaled)
library(broom)
db.lm.tidy = tidy(db.lm, conf.int = TRUE)
db.lm.tidy[2:10, 1] =names(df)[1:9]
data.frame(db.lm.tidy[, 1],round(db.lm.tidy[,-1], 2))
head(predictors.scaled)
library(ggplot2)
ggplot(db.lm.tidy[-1, ],aes(x = estimate, y = term, xmin = conf.low,xmax = conf.high, color = term))+
  geom_point()+ geom_errorbarh()+ geom_vline(xintercept = 0)



#Logistic Regression with Backward elimination technique
cancer.logit =glm(Is_Malignant~., family = binomial, data = df)
summary(cancer.logit)

cancer.logit =glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape + Marginal_adhesion+
                    Single_Epithelial_Cell_size + Bare_Nuclei + Bland_Chromatin + Normal_Nucleoli+
                    Mitoses, family = binomial, data = df)
summary(cancer.logit)

cancer.logit =glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape + Marginal_adhesion+
                    Bare_Nuclei + Bland_Chromatin + Normal_Nucleoli+
                    Mitoses, family = binomial, data = df)
summary(cancer.logit)

cancer.logit =glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape + Marginal_adhesion+
                    Bare_Nuclei + Bland_Chromatin + Normal_Nucleoli,
                  family = binomial, data = df)
summary(cancer.logit)

cancer.logit =glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape + Marginal_adhesion+
                    Bland_Chromatin + Normal_Nucleoli,
                  family = binomial, data = df)
summary(cancer.logit)

cancer.logit =glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape + Marginal_adhesion+
                    Bland_Chromatin ,
                  family = binomial, data = df)
summary(cancer.logit)

cancer.logit =glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape + 
                    Bland_Chromatin ,
                  family = binomial, data = df)
summary(cancer.logit)





#Histogram Plot 
nice.palette = c("#999999", "#E69F00", "#56B4E9", "#0072B2")
gg9 = ggplot(df, aes(x = Clump_thickness, fill = Is_Malignant)) + geom_histogram(breaks = seq(1,10,1)) 
gg9 = gg9  + scale_fill_manual(values = nice.palette)


gg10 = ggplot(df, aes(x = Uniformity_of_cell_shape, fill = Is_Malignant)) +  geom_histogram(breaks = seq(1,10,1)) 
gg10 = gg10  + scale_fill_manual(values = nice.palette)


gg11 = ggplot(df, aes(x = Bland_Chromatin, fill = Is_Malignant)) + geom_histogram(breaks = seq(1,10,1))
gg11 = gg11  + scale_fill_manual(values = nice.palette)


gg12 = ggplot(df, aes(x = Marginal_adhesion, fill = Is_Malignant)) + geom_histogram(breaks = seq(1,10,1))
gg12 = gg12  + scale_fill_manual(values = nice.palette)



require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size

plot_grid(gg9, gg10, gg11, gg12, labels = c('Clump_Thickness', 'Uniformity of Cell Shape','Bland Chromatin','Marginal Adhesion'), ncol = 2)




#average of attributes

ct = aggregate(Is_Malignant ~ Clump_thickness, mean, data = df)

ma = aggregate(Is_Malignant ~ Marginal_adhesion, mean, data = df)

bc = aggregate(Is_Malignant ~ Bland_Chromatin, mean, data = df)

cs = aggregate(Is_Malignant ~ Uniformity_of_cell_shape, mean, data = df)

ct$Type = rep('Clump_thickness', nrow(ct))
colnames(ct) = c('Severity', 'Is_Malignant', 'Type')

ma$Type = rep('Marginal_adhesion', nrow(ma))
colnames(ma) = c('Severity', 'Is_Malignant', 'Type')

bc$Type = rep('Bland_Chromatin', nrow(bc))
colnames(bc) = c('Severity', 'Is_Malignant', 'Type')

cs$Type = rep('Uniformity_of_cell_shape', nrow(cs))
colnames(cs) = c('Severity', 'Is_Malignant', 'Type')

dfm = data.frame(rbind(ct, ma, bc, cs))

ggplot(dfm, aes(y = Is_Malignant, x = Severity)) + 
  geom_point() + geom_line() + ggtitle('Aggregate Malignant cases for each attribute') + 
  facet_wrap( ~ factor(Type), ncol = 2)



#grid based classification using 2 variables

ggplot(df, aes(x= Marginal_adhesion, y = Clump_thickness,  color = factor(Is_Malignant))) + geom_point() +
  geom_vline(xintercept = 7) + 
  geom_hline(yintercept = 8) + ggtitle("Clump_Thickness vs Marginal Adhesion")

ggplot(df, aes(x= Uniformity_of_cell_shape, y = Bland_Chromatin,  color = factor(Is_Malignant))) + geom_point() +
  geom_vline(xintercept = 7) + 
  geom_hline(yintercept = 8) + ggtitle("Uniformity of Cell Shape vs Bland Chromatin")


#Interaction Plots

install.packages('leaps')
library(leaps)
df1 = df
cancer.leaps = regsubsets(Is_Malignant ~ ., data = df1)
summary(cancer.leaps)$which
summary(cancer.leaps)$cp

tidy(lm(Is_Malignant ~ Clump_thickness + Marginal_adhesion + 
          Uniformity_of_cell_shape + Bland_Chromatin, data = df1))

ct_ucs.glm = glm(Is_Malignant ~ Clump_thickness + Uniformity_of_cell_shape, 
                 family = "binomial", data = df1)

# summary(partyIm.glm)
ct_ucs.int.glm = glm(Is_Malignant ~ Clump_thickness * Uniformity_of_cell_shape, 
                     family = "binomial", data = df1)

# summary(partyIm.int.glm)
grid = expand.grid(Clump_thickness = 1:10, Uniformity_of_cell_shape = 1:10)
cancer.no = predict(ct_ucs.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
cancer.int = predict(ct_ucs.int.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
grid2 = data.frame(grid, cancer.no, cancer.int)

library(tidyr)
grid2 = gather(grid2, key = model, value = cancer, c("cancer.no", "cancer.int"))

grid2$model = recode_factor(grid2$model, "cancer.no" = "No interaction", 
                            "cancer.int" = "With interaction")

gg1 = ggplot(grid2, aes(x = Clump_thickness, y = cancer, group = model, color = model)) + 
  geom_line() + facet_grid(~Uniformity_of_cell_shape) + xlab("Clump Thickness") + 
  ylab("Chance of cancer") + 
  ggtitle("Interaction between Clump Thickness and Uniformity of cell shape")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

ucs_ma.glm = glm(Is_Malignant ~ Uniformity_of_cell_shape + Marginal_adhesion, 
                 family = "binomial", data = df1)

# summary(partyIm.glm)
ucs_ma.int.glm = glm(Is_Malignant ~ Uniformity_of_cell_shape * Marginal_adhesion, 
                     family = "binomial", data = df1)

# summary(partyIm.int.glm)
grid = expand.grid(Uniformity_of_cell_shape = 1:10, Marginal_adhesion = 1:10)
cancer.no = predict(ucs_ma.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
cancer.int = predict(ucs_ma.int.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
grid2 = data.frame(grid, cancer.no, cancer.int)

library(tidyr)
grid2 = gather(grid2, key = model, value = cancer, c("cancer.no", "cancer.int"))

grid2$model = recode_factor(grid2$model, "cancer.no" = "No interaction", 
                            "cancer.int" = "With interaction")

gg2 = ggplot(grid2, aes(x = Uniformity_of_cell_shape, y = cancer, group = model, color = model)) + 
  geom_line() + facet_grid(~Marginal_adhesion) + xlab("Uniformity of cell shape") + 
  ylab("Chance of cancer") + 
  ggtitle("Interaction between Uniformity of cell shape and Marginal Adhesion")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

ucs_bc.glm = glm(Is_Malignant ~ Uniformity_of_cell_shape + Bland_Chromatin, 
                 family = "binomial", data = df1)

# summary(partyIm.glm)
ucs_bc.int.glm = glm(Is_Malignant ~ Uniformity_of_cell_shape * Bland_Chromatin, 
                     family = "binomial", data = df1)

# summary(partyIm.int.glm)
grid = expand.grid(Uniformity_of_cell_shape = 1:10, Bland_Chromatin = 1:10)
cancer.no = predict(ucs_bc.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
cancer.int = predict(ucs_bc.int.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
grid2 = data.frame(grid, cancer.no, cancer.int)

library(tidyr)
grid2 = gather(grid2, key = model, value = cancer, c("cancer.no", "cancer.int"))

grid2$model = recode_factor(grid2$model, "cancer.no" = "No interaction", 
                            "cancer.int" = "With interaction")

gg3 = ggplot(grid2, aes(x = Uniformity_of_cell_shape, y = cancer, group = model, color = model)) + 
  geom_line() + facet_grid(~Bland_Chromatin) + xlab("Uniformity of cell shape") + 
  ylab("Chance of cancer") + 
  ggtitle("Interaction between Uniformity of cell shape and Bland Chromatin")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
ct_ma.glm = glm(Is_Malignant ~ Clump_thickness + Marginal_adhesion, family = "binomial", data = df1)

# summary(partyIm.glm)
ct_ma.int.glm = glm(Is_Malignant ~ Clump_thickness * Marginal_adhesion, 
                    family = "binomial", data = df1)
# summary(partyIm.int.glm)
grid = expand.grid(Clump_thickness = 1:10, Marginal_adhesion = 1:10)
cancer.no = predict(ct_ma.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
cancer.int = predict(ct_ma.int.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
grid2 = data.frame(grid, cancer.no, cancer.int)

library(tidyr)
grid2 = gather(grid2, key = model, value = cancer, c("cancer.no", "cancer.int"))

grid2$model = recode_factor(grid2$model, "cancer.no" = "No interaction", 
                            "cancer.int" = "With interaction")

gg4 = ggplot(grid2, aes(x = Clump_thickness, y = cancer, group = model, color = model)) + 
  geom_line() + facet_grid(~Marginal_adhesion) + xlab("Clump Thickness") + ylab("Chance of cancer") + 
  ggtitle("Interaction between Clump Thickness and Marginal Adhesion")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
ct_bc.glm = glm(Is_Malignant ~ Clump_thickness + Bland_Chromatin, family = "binomial", data = df1)

# summary(partyIm.glm)
ct_bc.int.glm = glm(Is_Malignant ~ Clump_thickness * Bland_Chromatin, family = "binomial", data = df1)
# summary(partyIm.int.glm)
grid = expand.grid(Clump_thickness = 1:10, Bland_Chromatin = 1:10)
cancer.no = predict(ct_bc.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
cancer.int = predict(ct_bc.int.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
grid2 = data.frame(grid, cancer.no, cancer.int)

library(tidyr)
grid2 = gather(grid2, key = model, value = cancer, c("cancer.no", "cancer.int"))

grid2$model = recode_factor(grid2$model, "cancer.no" = "No interaction", 
                            "cancer.int" = "With interaction")

gg5 = ggplot(grid2, aes(x = Clump_thickness, y = cancer, group = model, color = model)) + 
  geom_line() + facet_grid(~Bland_Chromatin) + xlab("Clump Thickness") + ylab("Chance of cancer") + 
  ggtitle("Interaction between Clump Thickness and Bland_Chromatin")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())
bc_ma.glm = glm(Is_Malignant ~ Marginal_adhesion + Bland_Chromatin, family = "binomial", data = df1)

# summary(partyIm.glm)
bc_ma.int.glm = glm(Is_Malignant ~ Marginal_adhesion * Bland_Chromatin, family = "binomial", data = df1)
# summary(partyIm.int.glm)
grid = expand.grid(Marginal_adhesion = 1:10, Bland_Chromatin = 1:10)
cancer.no = predict(bc_ma.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
cancer.int = predict(bc_ma.int.glm, newdata = grid, type = "response", se.fit = TRUE)$fit
grid2 = data.frame(grid, cancer.no, cancer.int)

library(tidyr)
grid2 = gather(grid2, key = model, value = cancer, c("cancer.no", "cancer.int"))


grid2$model = recode_factor(grid2$model, "cancer.no" = "No interaction", 
                            "cancer.int" = "With interaction")

gg6 = ggplot(grid2, aes(x = Bland_Chromatin, y = cancer, group = model, color = model)) + 
  geom_line() + facet_grid(~Marginal_adhesion) + xlab("Bland Chromatin") + 
  ylab("Chance of cancer") + ggtitle("Interaction between Bland_Chromatin and Marginal adhesion")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())

require(cowplot)
theme_set(theme_cowplot(font_size=12)) # reduce default font size

plot_grid(gg1, gg2, gg3, gg4, gg5, gg6, ncol = 2)

cancer.model = glm(Is_Malignant ~ Clump_thickness + Marginal_adhesion + 
                     Uniformity_of_cell_shape + Bland_Chromatin + 
                     Clump_thickness:Marginal_adhesion + Clump_thickness:Uniformity_of_cell_shape + 
                     Clump_thickness:Bland_Chromatin , family = "binomial", data = df1)

cancer.model.df = df1
cancer.model.df$.fitted = fitted.values(cancer.model)
cancer.model.df$.resid = residuals(cancer.model, type = "response")
ggplot(cancer.model.df, aes(x = .fitted, y = .resid)) + geom_point() + geom_smooth(method = "lm", 
                                                                                   method.args = list(degree = 1)) + xlab("Fitted values") + ylab("Residuals")

summary(cancer.model)

cancer.grid = expand.grid(Clump_thickness = 1:6, Marginal_adhesion = 1:6, 
                          Uniformity_of_cell_shape = 1:6, Bland_Chromatin = 1:6)

ucs_names = c("1" = "UCS: 1", "2" = "UCS: 2", "3" = "UCS: 3", "4" = "UCS: 4", 
              "5" = "UCS: 5", "6" = "UCS: 6" )
ma_names = c("1" = "MA: 1", "2" = "MA: 2", "3" = "MA: 3", "4" = "MA: 4", "5" = "MA: 5", "6" = "MA: 6" )
ct_names = c("1" = "CT: 1", "2" = "CT: 2", "3" = "CT: 3", "4" = "CT: 4", "5" = "CT: 5", "6" = "CT: 6" )
bc_names = c("1" = "BC: 1", "2" = "BC: 2", "3" = "BC: 3", "4" = "BC: 4", "5" = "BC: 5", "6" = "BC: 6" )

cancer.pred = predict(cancer.model, newdata = cancer.grid, type = "response")
cancer.grid1 = data.frame(cancer.grid, cancer.prob = as.vector(cancer.pred))
ggplot(cancer.grid1, aes(x = Clump_thickness, y = cancer.prob*100, 
                         group = Bland_Chromatin, color = Bland_Chromatin)) + 
  geom_line() + facet_grid(Marginal_adhesion ~ Uniformity_of_cell_shape, 
                           labeller = labeller(Uniformity_of_cell_shape = as_labeller(ucs_names), 
                                      Marginal_adhesion = as_labeller(ma_names)))+ 
  xlab("Clump Thickness") + ylab("Probability of Cancer") + labs(color = "Bland Chromatin")

ggplot(cancer.grid1, aes(x = Clump_thickness, y = cancer.prob*100, 
                         group = Marginal_adhesion, color = Marginal_adhesion)) + 
  geom_line() + facet_grid(Uniformity_of_cell_shape ~ Bland_Chromatin, 
                           labeller = labeller(Uniformity_of_cell_shape = as_labeller(ucs_names), 
                                               Bland_Chromatin = as_labeller(bc_names)))+ 
  xlab("Clump Thickness") + ylab("Probability of Cancer") + labs(color = "Marginal Adhesion")


# Fitting the model

cancer.model = glm(Is_Malignant ~ Clump_thickness + Marginal_adhesion + 
                     Uniformity_of_cell_shape + Bland_Chromatin + 
                     Clump_thickness:Marginal_adhesion + 
                     Clump_thickness:Uniformity_of_cell_shape + 
                     Clump_thickness:Bland_Chromatin , family = "binomial", data = df)

cancer.model.df = df
cancer.model.df$.fitted = fitted.values(cancer.model)
cancer.model.df$.resid = residuals(cancer.model, type = "response")
ggplot(cancer.model.df, aes(x = .fitted, y = .resid)) + geom_point() + 
  geom_smooth(method = "lm", method.args = list(degree = 1)) + 
  xlab("Fitted values") + ylab("Residuals")

summary(cancer.model)

#Train_Test Split

train <- df1[1:400,]
test <- df1[401:683,]

fitted.results <- predict(cancer.model,newdata=subset(test,select=c(1,3,4,7)),type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Is_Malignant)
print(paste('Accuracy',1-misClasificError))























