Adults = subset(NHANES, Age >= 18, c("Gender", "Age", "Height", "Weight"))

attach(Adults)
mean_men_height = mean(Height[Gender == 'male'], na.rm = TRUE)
mean_fem_height = mean(Height[Gender == 'female'], na.rm = TRUE)

# Handling missing Height data. 
# There are 57 missing height values which are replaced by the mean height of male and females

Height = ifelse(is.na(Height) & Gender == 'male', mean_men_height, 
ifelse(is.na(Height) & Gender == 'female', mean_fem_height, Height))

men_height = Height[Gender == 'male']
fem_height = Height[Gender == 'female']

#Depicting the Additive shift

qqplot(fem_height, men_height, xlab = 'Heights of Adult Females(cms)', 
       ylab = 'Heights of adult Males(cms)', 
       main = 'The comparision of heights of adult males and females')
abline(0, 1, col = 'red', lty = 1, lwd = 3 )

qqplot(fem_height + 13.5, men_height, xlab = 'Heights of Adult Females(cms) + 13.5(cms)', 
       ylab = 'Heights of adult Males(cms)', 
       main = 'The comparision of heights of adult males and females')
abline(0, 1, col = 'red', lty = 1, lwd = 3 )


mean_men_weight = mean(Weight[Gender == 'male'], na.rm = TRUE)
mean_fem_weight = mean(Weight[Gender == 'female'], na.rm = TRUE)

# Handling missing Weight data. 
# There are 61 missing height values which are replaced by the mean weight of male and females

Weight = ifelse(is.na(Weight) & Gender == 'male', mean_men_weight, 
                ifelse(is.na(Weight) & Gender == 'female', mean_fem_weight, Weight))

men_weight = Weight[Gender == 'male']
fem_weight = Weight[Gender == 'female']

qqplot(fem_weight, men_weight, xlab = 'Weights of Adult Females(kgs)', 
       ylab = 'Weights of adult Males(kgs)', 
       main = 'The comparision of weights of adult males and females')
abline(0, 1, col = 'red', lty = 1, lwd = 3 )

qqplot(fem_weight + 11, men_weight, xlab = 'Weights of Adult Females(kgs) 11(kgs)', 
       ylab = 'Weights of adult Males(kgs)', 
       main = 'The comparision of weights of adult males and females')
abline(0, 1, col = 'red', lty = 1, lwd = 3 )

# Residual Plot

Adults_lm = lm(Height ~ Gender, data = Adults)

Adults_fitted = sort(fitted.values(Adults_lm)) - mean(fitted.values(Adults_lm))
Adults_residuals = sort(residuals(Adults_lm))

n = length(Adults_residuals)
f.value = (0.5:(n - 0.5))/n
Adults_fit = data.frame(f.value, Fitted = Adults_fitted, Residuals = Adults_residuals)

Adults_fit_long = Adults_fit %>% gather(type, value, Fitted:Residuals)

ggplot(Adults_fit_long, aes(sample = value)) + stat_qq(distribution = "qunif") +
  facet_grid(~type) + xlab('Quantiles of the standard uniform distribution') + 
  ylab('Values from the model(cm)') +
  ggtitle('Residual-fit spread plot for model predicting height from gender')

var_explained = var(Adults_fitted)/var(Height)

