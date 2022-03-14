#install libraries
library(dplyr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(car)
library(caTools)
library(caret)
library(ggpubr)

#read in the data
mm_final = fread(file = 'mm_final.csv')
attach(mm_final)

#use pairs to look at some of the relationships
pairs(upsetMOV~Upset+Seed1+Score1+Seed2+Score2+WP1+WP2)
pairs(upsetMOV~Upset+WP1+WP2+seedDiff+SRS1+SRS2+SOS1+SOS2)
pairs(upsetMOV~Upset+Pts1+Pts2+Opp1+Opp2+MOV1+MOV2)
pairs(upsetMOV~WP1+WP2+seedDiff+SRS1+SRS2+SOS1+SOS2+MOV1+MOV2)
#WP1, WP2, Seed1, Seed2, seedDiff, SRS1, SRS2, SOS1, SOS2, MOV1, MOV2

#check the normality of response variable upsetMOV
ggplot(mm_final, aes(x = upsetMOV)) + 
  geom_histogram(aes(y = ..density..), color = 'blue', fill = 'blue', bins = 20) +
  geom_density(alpha = 0.05, color = 'red')

#check relationships for logistic model
boxplot(WP1~Upset)
boxplot(WP2~Upset)
boxplot(seedDiff~Upset)
boxplot(SRS1~Upset)
boxplot(SRS2~Upset)
boxplot(SOS1~Upset)
boxplot(SOS2~Upset)
boxplot(MOV1~Upset)
boxplot(MOV2~Upset)

#based on boxplots: WP1, WP2, seedDiff, SRS1, SRS2, SOS2, MOV1, MOV2


#split into training and test set
set.seed(28)
split = sample.split(Y = mm_final$Upset, SplitRatio = 0.8)
log_train = subset(x = mm_final, split == TRUE)
log_test = subset(x = mm_final, split == FALSE)

#create logistic model with variables identified in boxplot analysis
m1 = glm(Upset~WP1+WP2+seedDiff+SRS1+SRS2+SOS2+MOV1+MOV2, family = binomial, data = log_train)
m1.probs = predict(m1, log_test, type = 'response')
m1.pred = rep(0, length(m1.probs))
m1.pred[m1.probs>0.5] = 1
#confusion matrix and test error
table(m1.pred, log_test$Upset)
mean(m1.pred != log_test$Upset)
summary(m1)

#update model
m1_updated = glm(Upset~WP1+WP2+seedDiff+SRS1+MOV1, family = binomial, data = log_train)
m1_up.probs = predict(m1_updated, log_test, type = 'response')
m1_up.pred = rep(0, length(m1_up.probs))
m1_up.pred[m1_up.probs>0.5] = 1
table(m1_up.pred, log_test$Upset)
mean(m1_up.pred != log_test$Upset)
summary(m1_updated)

#final model
m1_final = glm(Upset~WP1+WP2+SRS1+MOV1, family = binomial, data = log_train)
m1_final.probs = predict(m1_final, log_test, type = 'response')
m1_final.pred = rep(0, length(m1_final.probs))
m1_final.pred[m1_final.probs>0.5] = 1
table(m1_final.pred, log_test$Upset)
mean(m1_final.pred != log_test$Upset)
summary(m1_final)

#fit logistic model to full dataset
m1_finalFull = glm(Upset~WP1+WP2+SRS1+MOV1, family = binomial, data = mm_final)
m1_finalFull.probs = predict(m1_finalFull, mm_final, type = 'response')
m1_finalFull.pred = rep(0, length(m1_finalFull.probs))
m1_finalFull.pred[m1_finalFull.probs>0.5] = 1
mean(m1_finalFull.pred != mm_final$Upset)
table(m1_finalFull.pred, mm_final$Upset)
summary(m1_finalFull)

#WP1, WP2, Seed1, Seed2, seedDiff, SRS1, SRS2, SOS1, SOS2, MOV1, MOV2

#create intercept only model to be used in stepwise regression
m_int = lm(upsetMOV~1)

#create the model using all the variables that appeared to have significant effect on the outcome
m_full = lm(upsetMOV~WP1+WP2+seedDiff+SRS1+SRS2+SOS1+SOS2+MOV1+MOV2)

#create f to use in stepwise
f = ~WP1+WP2+seedDiff+SRS1+SRS2+SOS1+SOS2+MOV1+MOV2

#create 3 stepwise models, forward, backward, and both to determine best model fit
m_forward = step(m_int, direction = 'forward', scope = f)

m_backward = step(m_full, direction = 'backward', scope = f)

m_both = step(m_int, direction = 'both', scope = f)

#all three stepwise paths found the same model, so create that as new model
m_poststep = lm(upsetMOV~WP1+WP2+seedDiff+SRS1+SRS2)
summary(m_poststep)

#look at intervals, anova and plots to examine model fit
# confidence interval
confint(m_poststep)
# anova
anova(m_poststep)
#density plot
plot(density(resids))
#residual plots 
plot(m_poststep)

#based on these plots, the residuals look to be fairly normally distributed, indicating this model should be a
# good fit

#VIF for each set of regressors to observe multicollinearity
vif(m_poststep)
vif(lm(upsetMOV~WP1+WP2+SRS1+SRS2))
vif(lm(upsetMOV~WP1+WP2))
vif(lm(upsetMOV~SRS1+SRS2))
vif(lm(upsetMOV~WP1+SRS1))
vif(lm(upsetMOV~WP2+SRS2))
vif(lm(upsetMOV~WP1+WP2+seedDiff))
#low vif values (<<10) indicate low multicollinearity

m2_final = m_poststep

#plot predictions vs actual
prediction = predict.lm(m2_final)
ggplot(mm_final, aes(x = prediction, y = upsetMOV)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor(aes(label =..rr.label..), label.x = -50, label.y = 20) +
  ggtitle('Predicted vs. Actual for Full Dataset') + 
  xlab('Predicted Margin of Victory') +
  ylab('Actual Margin of Victory')

mm_final$predictedMOV = prediction

detach(mm_final)

#read in 2021 data
games21 = fread(file = '2021_games.csv')

#read in 2022 data (will need two files because of play in games)

games22_v1 = fread(file = '2022_games1.csv')
games22_v2 = fread(file = '2022_games2.csv')


#predict 2021 games as a test
test21.probs = predict(m1_finalFull, games21, type = 'response')
test21.pred = rep(0, length(test21.probs))
test21.pred[test21.probs>0.5] = 1
table(test21.pred, games21$Upset)
mean(test21.pred != games21$Upset)
games21$upsetPred = test21.pred

predict21 = predict(m2_final, newdata = games21)
games21$prediction = predict21

#plot predictions vs actual for 2021 games
ggplot(games21, aes(x = predict21, y = upsetMOV)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  stat_cor(aes(label =..rr.label..), label.x = -30, label.y = 10) +
  ggtitle("2021 Predicted vs. Actual") +
  xlab('Predicted Margin of Victory') +
  ylab('Actual Margin of Victory')

#predict 2022 games for dataset 1 (first set of play in teams win)
predict22_v1 = predict(m1_finalFull, games22_v1, type = 'response')
predict22_v1.pred = rep(0, length(predict22_v1))
predict22_v1.pred[predict22_v1>0.5] = 1
games22_v1$upsetPred = predict22_v1.pred

upsets22_v1 = games22_v1 %>% filter(upsetPred == 1)

predict22_v1_linear = predict(m2_final, newdata = games22_v1)
games22_v1$MOVpred = predict22_v1_linear

upsets22_v1_linear = games22_v1 %>% filter(MOVpred > 0)

#predict 2022 games for dataset 2 (second set of play in teams win)
predict22_v2 = predict(m1_finalFull, games22_v2, type = 'response')
predict22_v2.pred = rep(0, length(predict22_v2))
predict22_v2.pred[predict22_v2>0.5] = 1
games22_v2$upsetPred = predict22_v2.pred

upsets22_v2 = games22_v2 %>% filter(upsetPred == 1)

predict22_v2_linear = predict(m2_final, newdata = games22_v2)
games22_v2$MOVpred = predict22_v2_linear

upsets22_v2_linear = games22_v2 %>% filter(MOVpred > 0)

close22_v1 = games22_v1 %>% filter(MOVpred > -3)
close22_v2 = games22_v2 %>% filter(MOVpred >-3)


