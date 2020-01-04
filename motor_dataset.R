library(DataExplorer)
library(dplyr)
library(Hmisc)
library(funModeling)
library(tidyverse)
library(misc)
library(elementR)
library(MASS)
library(ggplot2)
library(readr)
library(shiny)

library(rsconnect)
rsconnect::deployApp("C:\\Users\\reddymv\\Desktop")
motor <- read.csv(file.choose())
motor_less <- read.csv("C:\\Users\\reddymv\\Downloads\\pmsm_temperature_data.csv", nrow = 10000)
attach(motor)
attach(motor_less)
#EDA
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
motor <- normalize(motor)
motor_less <- normalize(motor_less)
summary(motor)
plot_str(motor, fontSize = 40)
plot_histogram(motor)
plot_missing(motor)
plot_density(motor)
plot_correlation(motor)
boxplot(motor)
#pairs(motor)
qqnorm(motor$ambient)
qqline(motor$ambient)
qqnorm(motor$coolant)
qqline(motor$coolant)
qqnorm(motor$u_d)
qqline(motor$u_d)
qqnorm(motor$u_q)
qqline(motor$u_q)
qqnorm(motor$stator_tooth)
qqline(motor$stator_tooth)
qqnorm(motor$stator_winding)
qqline(motor$stator_winding)
qqnorm(motor$profile_id)
qqline(motor$profile_id)

#outlier treatment
boxplot(motor$ambient)$out
boxplot(motor$ambient, plot=FALSE)$out
outliers <- boxplot(motor$ambient, plot=FALSE)$out
print(outliers)
motor[which(motor$ambient %in% outliers),]
motor <- motor[-which(motor$ambient %in% outliers),]
boxplot(motor$ambient)

boxplot(motor$u_d)$out
boxplot(motor$u_d, plot=FALSE)$out
outliers <- boxplot(motor$u_d, plot=FALSE)$out
print(outliers)
motor[which(motor$u_d %in% outliers),]
motor <- motor[-which(motor$u_d %in% outliers),]
boxplot(motor$u_d)

boxplot(motor$torque)$out
boxplot(motor$torque, plot=FALSE)$out
outliers <- boxplot(motor$torque, plot=FALSE)$out
print(outliers)
motor[which(motor$torque %in% outliers),]
motor <- motor[-which(motor$torque %in% outliers),]
boxplot(motor$torque)


boxplot(motor$i_d)$out
boxplot(motor$i_d, plot=FALSE)$out
outliers <- boxplot(motor$i_d, plot=FALSE)$out
print(outliers)
motor[which(motor$i_d %in% outliers),]
motor <- motor[-which(motor$i_d %in% outliers),]
boxplot(motor$i_d)

boxplot(motor$i_q)$out
boxplot(motor$i_q, plot=FALSE)$out
outliers <- boxplot(motor$i_q, plot=FALSE)$out
print(outliers)
motor[which(motor$i_q %in% outliers),]
motor <- motor[-which(motor$i_q %in% outliers),]
boxplot(motor$i_q)

boxplot(motor$pm)$out
boxplot(motor$pm, plot=FALSE)$out
outliers <- boxplot(motor$pm, plot=FALSE)$out
print(outliers)
motor[which(motor$pm %in% outliers),]
motor <- motor[-which(motor$pm %in% outliers),]
boxplot(motor$i_d)

boxplot(motor)
boxplot(motor_less)

#splitting the data 

smp_size <- floor(0.75 * nrow(motor))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(motor)), size = smp_size)

train <- motor[train_ind, ]
test <- motor[-train_ind, ]

smp_size1 <- floor(0.50 * nrow(motor))

## set the seed to make your partition reproducible
set.seed(123)
train_ind1 <- sample(seq_len(nrow(motor)), size = smp_size1)

train1 <- motor[train_ind1, ]
test1 <- motor[-train_ind1, ]
y <- pm+stator_yoke

smp_size_less <- floor(0.75 * nrow(motor_less))
set.seed(123)
train_less <- sample(seq_len(nrow(motor_less)), size = smp_size_less)
traindataless <- motor_less[train_less, ]
testdataless <- motor_less[-train_less, ]

#model building linear regressions
model1 <- lm(pm+stator_yoke~., data = train)
summary(model1) #R2 = 0.931
pred1 <- predict(model1)
model1$residuals
sum(model1$residuals) #sum of errors = 0
sqrt(sum(model1$residuals^2)/nrow(train)) #RMSE = 0.005274
plot(model1)
pred1_test_train <- predict(model1, data = test)
rmse1 <- sqrt(sum((pred1_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse1, R2=summary(model1)$r.squared)
actuals_preds <- data.frame(cbind(actuals=pm+stator_yoke, predicteds=pred1_test_train))
correlation_accuracy <- cor(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  


model2 <- lm(pm+stator_yoke~ambient+coolant+u_d+u_q+i_d+i_q+log(motor_speed)+stator_tooth+stator_winding+torque+profile_id, data= train)
summary(model2) #R2 = 0.9309
pred2 <- predict(model2)
model2$residuals
sum(model2$residuals) #sum of errors = 0
sqrt(sum(model2$residuals^2)/nrow(train)) #RMSE = 0.005277573
plot(model2)
pred2_test_train <- predict(model2 , data = test)
rmse2 <- sqrt(sum((pred2_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse2, R2=summary(model2)$r.squared)

model3 <- lm(pm+stator_yoke~ambient+coolant+u_d+log(u_q)+i_d+i_q+log(motor_speed)+stator_tooth+stator_winding+torque+profile_id, data = train)
summary(model3)#R2= 0.9313
pred3 <- predict(model3)
model3$residuals
sum(model3$residuals) #sum of errors = 0
sqrt(sum(model3$residuals^2)/nrow(train)) #RMSE =0.0052616
pred3_test_train <- predict(model3 , data = test)
plot(model3)
rmse3 <- sqrt(sum((pred3_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse3, R2=summary(model3)$r.squared)
plot(model3)

model4 <- lm(pm+stator_yoke~ambient^2+coolant^2+u_d^2+u_q^2+i_d^2+i_q^2+motor_speed^2+stator_tooth^2+stator_winding^2+torque^2+profile_id^2)
summary(model4) #R2 = 0.9335
pred4 <- predict(model4)
model4$residuals
sum(model4$residuals)
sqrt(sum(model4$residuals^2)/nrow(train)) #RMSE =0.60
plot(model4)
pred4_test_train <- predict(model4 , data = test)
rmse4 <- sqrt(sum((pred4_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse4, R2=summary(model4)$r.squared)

model5 <- lm(pm+stator_yoke~ambient+I(coolant^2)+I(u_d^3)+I(u_q^4)+I(i_d^5)+I(i_q^6)+I(motor_speed^7)+I(stator_tooth^8)+I(stator_winding^9)+I(torque^10)+I(profile_id^11), data=train)
summary(model5) #R2 = 0.814
pred5 <- model5$residuals
sum(model5$residuals)
sqrt(sum(model5$residuals^2)/nrow(train)) #RMSE = 0.0086
plot(model5)
pred5_test_train <- predict(model5 , data = test)
rmse5 <- sqrt(sum((pred5_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse5, R2=summary(model5)$r.squared)

model6 <- lm(log(pm)+stator_yoke~., data=train)
summary(model6) #R2 = 0.776
pred6 <- predict(model6)
model6$residuals
sum(model6$residuals)
logdt <- predict(model6)
dt <- exp(logdt)
error = (motor$pm) -dt
error
sqrt(sum(error^2)/nrow(train)) #RMSE = 0.0211
plot(model6)
pred6_test_train <- predict(model6 , data = test)
rmse6 <- sqrt(sum((exp(pred6_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke))
c(RMSE = rmse6, R2=summary(model6)$r.squared)

model7 <- lm(pm+log(stator_yoke)~., data=train)
summary(model7) #R2 = 0.9947
logdt_model7 <- predict(model7)
dt_model7 <- exp(logdt_model7)
error_model7 = (motor$stator_yoke) - dt_model7
error_model7
sqrt(sum(error_model7^2)/nrow(train)) #RMSE = 0.022
plot(model7)
pred7_test_train <- predict(model7 , data = test)
rmse7 <- sqrt(sum((exp(pred7_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke))
c(RMSE = rmse7, R2=summary(model7)$r.squared)
mse_model7 <- mean((train$pm+train$stator_yoke-logdt_model7)^2) #mse= 6.012323

#polynomial regression
model8 <- lm(pm+stator_yoke~ambient+I(ambient^2)+coolant+I(coolant^2)+u_d+I(u_d^2)+u_q+I(u_q^2)+i_d+I(i_d^2)+i_q+I(i_q^2)+motor_speed+I(motor_speed^2)+stator_tooth+I(stator_tooth^2)
             +stator_winding+I(stator_winding^2)+torque+I(torque^2)+profile_id+I(profile_id^2), data=train)
summary(model8) #R2=0.9342
pred8 <- predict(model8)
model8$residuals
sum(model8$residuals) #sum of errors = 0
sqrt(sum(model8$residuals^2)/nrow(train)) #RMSE = 0.00514
plot(model8)
pred8_test_train <- predict(model8 , data = test)
rmse8 <- sqrt(sum((pred8_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse8, R2=summary(model8)$r.squared)
plot(model8)


model9 <- lm(pm+stator_yoke~ambient+I(ambient^2)+I(ambient^3)+coolant+I(coolant^2)+I(coolant^3)+u_d+I(u_d^2)+I(u_d^3)+u_q+I(u_q^2)+I(u_q^3)+i_d+I(i_d^2)+I(i_d^3)+i_q+I(i_q^2)+I(i_q^3)
             +motor_speed+I(motor_speed^2)+I(motor_speed^3)+stator_tooth+I(stator_tooth^2)+I(stator_tooth^3)
             +stator_winding+I(stator_winding^2)+I(stator_winding^3)+torque+I(torque^2)+I(torque^3)+profile_id+I(profile_id^2)+I(profile_id^3), data=train)

summary(model9) #R2 0.94
model9$residuals
sum(model9$residuals) #sum of errors = 0
sqrt(sum(model9$residuals^2)/nrow(train)) #RMSE = 0.0049
plot(model9)
pred9_test_train <- predict(model9, data = test)
rmse9 <- sqrt(sum((pred9_test_train) - pm+stator_yoke)^2)/length(pm+stator_yoke)
c(RMSE = rmse9, R2=summary(model9)$r.squared)

final <- predict(model9)
view(final)

#decision tree
library(ctree)
library(readr)
library(party)
library(caret)
library(C50)
library(tree)
library(gmodels)
library(knitr)
library(png)
library(rpart)
library(e1071)
library(rpart.plot)

#Neural networks
library(neuralnet)
library(nnet)
library(NeuralNetTools)
model_neuralnet <- neuralnet(pm+stator_yoke~., data = traindataless, hidden=10, stepmax =1e6, linear.output = F)
plot(model_neuralnet)
par(mar = numeric(4), family = 'serif')
plotnet(model_neuralnet, alpha = 0.6)
#evaluating model performanace 
set.seed(12323)
model_results <- compute(model_neuralnet,motor_less[1:13])
predicted_motor <- model_results$net.result
cor(predicted_motor, motor_less$pm) # 0.8936585 0.4568065
cor(predicted_motor, motor_less$stator_yoke) #0.8130647  0.4641767
RMSE.NN = (sum((motor_less$pm - predicted_motor)^2) / nrow(motor_less)) ^ 0.5 #1.357431
RMSE.NN_stator_yoke = (sum((motor_less$stator_yoke - predicted_motor)^2) / nrow(motor_less)) ^ 0.5 #pm rmse =1.357431 #stator_yoke rmse = 1.560865
r2_nn <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(model_neuralnet, testdataless))
summary(model_neuralnet)
mse_nn <- mean((traindataless$pm+traindataless$stator_yoke-predicted_motor)^2)
print(mse_nn) #3.44123
  
#xgboost
install.packages("xgboost")
require(xgboost)
library(xgboost)
library(Matrix)
library(caTools)

set.seed(100)  # For reproducibility
# Create index for testing and training data
inTrain <- createDataPartition(y = motor_less$stator_yoke, p = 0.8, list = FALSE)
# subset power_plant data to training
training <- motor_less[inTrain,]
# subset the rest to test
testing <- motor_less[-inTrain,]
X_train = xgb.DMatrix(as.matrix(training [,-10]))
y_train = training$stator_yoke
X_test = xgb.DMatrix(as.matrix(testing [-10]))
y_test = testing$stator_yoke
xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)
xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)
set.seed(0) 
xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)
xgb_train$bestTune
predicted = predict(xgb_model, X_test)
residuals = y_test - predicted
RMSE = sqrt(mean(residuals^2)) #0.009650178
cat('The root mean square error of the test data is ', round(RMSE,3),'\n') #0.01 #0.003
y_test_mean = mean(y_test)
# Calculate total sum of squares
tss =  sum((y_test - y_test_mean)^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
cat('The R-square of the test data is ', round(rsq,3), '\n') #1 #1
options(repr.plot.width=8, repr.plot.height=4)
my_data = as.data.frame(cbind(predicted = predicted,
                              observed = y_test))
# Plot predictions vs test data
ggplot(my_data,aes(predicted, observed)) + geom_point(color = "darkred", alpha = 0.5) + 
  geom_smooth(method=lm)+ ggtitle('Linear Regression ') + ggtitle("Extreme Gradient Boosting: Prediction vs Test Data") +
  xlab("predicted stator_yoke ") + ylab("Observed stator_yoke") + 
  theme(plot.title = element_text(color="darkgreen",size=16,hjust = 0.5),
        axis.text.y = element_text(size=12), axis.text.x = element_text(size=12,hjust=.5),
        axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))


#Decision Tree
set.seed(3033)
intrain <- createDataPartition(y= motor_less$pm + motor_less$stator_yoke, p=0.5, list = FALSE)
training_tree <- motor_less[intrain,]
testing_tree <- motor_less[-intrain,]
dim(training_tree)
dim(testing_tree)
anyNA(motor)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)
dtree_fit <- train(pm+stator_yoke ~., data = motor_less, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10, na.action = na.omit)

fit <- rpart(motor_less$pm+motor_less$stator_yoke~., data = motor_less)
fit  #accuracy selected low rmse and high rsquared cp = 0.004643267
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
#prediction
predict_dtree <- predict(fit, data = motor_less)
r_sq_dt <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(fit, testdataless)) #R2
print(r_sq_dt) #0.9484638

#Random Forests
library(randomForest)
library(caTools)
library(rpart) #classification and regression trees
library(partykit) #treeplots
library(MASS) #breast and pima indian data
library(ElemStatLearn) #prostate data
library(randomForest) #random forests
library(gbm) #gradient boosting 
library(caret)
library(miscTools)
library(ggplot2)
library(rsq)

rf <- randomForest(pm+stator_yoke~., data= traindataless)
plot(rf)
summary(rf)
pred_rf <- predict(rf, newdata = testdataless)
cm = table(testdataless, pred_rf)
which.min(rf$mse) #213
sqrt(rf$mse[which.min(rf$mse)]) #RMSE = 0.01988228
mse_rf <- mean((motor_less$pm-pred_rf)^2)
print(mse_rf) #4.034594
mse_rf_sy <- mean((motor_less$stator_yoke-pred_rf)^2)
print(mse_rf_sy)#2.549176
mean(pred_rf == motor_less)
r_sq1 <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(rf, testdataless))
print(r_sq1) #0.9998986

#Rpart function
df_motor <-rpart(pm+stator_yoke~., data=traindataless, control = rpart.control(minsplit = 5))
plot(df_motor)
plotcp(df_motor)
plot(as.party(df_motor))
summary(df_motor)
cp = min(df_motor$pm)
prune.tree.pros = prune(df_motor, cp = cp)
predictions_decisiontree <- predict(df_motor, testdataless)
mse_dt <- mean((motor_less$pm-predictions_decisiontree)^2)
print(mse_dt) #3.923536
mse_dtsy <- mean((motor_less$stator_yoke-predictions_decisiontree)^2)
print(mse_dtsy) #2.440488
r2_rpart <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(df_motor, testdataless))
print(r2_rpart) # 0.9484353

#Rules system
library(RWeka)
model_tree <- M5P(pm+stator_yoke~., data = traindataless)
summary(model_tree)
predictions_model_tree <- predict(model_tree, newdata = testdataless)
mse_motor_tree <- mean((motor_less$pm - predictions_model_tree)^2)
print(mse_motor_tree) # 4.031904
mse_motor_tree_sy <- mean((motor_less$stator_yoke - predictions_model_tree)^2)
print(mse_motor_tree_sy)
r2_rpart_m5p <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(model_tree, testdataless))
print(r2_rpart_m5p) #0.9991537

#Rule System
fit_tree <- M5Rules(pm+stator_yoke~., data = traindataless)
summary(fit_tree)
predictions_model <- predict(fit_tree, newdata = testdataless)
mse_fitree <- mean((motor_less$pm - predictions_model)^2)
print(mse_fitree) #4.034029
mse_fitree_sy <- mean((motor_less$stator_yoke - predictions_model)^2)
print(mse_fitree_sy) #2.545989
r2_rpart_m5rules <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(fit_tree, testdataless))
print(r2_rpart_m5rules) #0.9993745

#Bagging Cart
fit_bagging <- Bagging(pm+stator_yoke~., data = traindataless)
summary(fit_bagging)
predictions_bagging <- predict(fit_bagging, newdata = testdataless)
mse_bagging <- mean((motor_less$pm - predictions_bagging)^2)
print(mse_bagging) #4.027077
mse_bagging_sy <- mean((motor_less$stator_yoke- predictions_bagging)^2)
print(mse_bagging_sy) #2.541354
r2_bagging <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(fit_bagging, testdataless))
print(r2_bagging) # 0.9987972

#Gradient Boost Machine
library(gbm)
library(Metrics)
fit_gradient <- gbm(pm+stator_yoke~., data = traindataless,  distribution="gaussian")
summary(fit_gradient)
pred_gbm <- predict(fit_gradient, testdataless, n.trees=fit_gradient$n.trees)
mse_gradient <- mean((motor_less$pm - pred_gbm)^2)
print(mse_gradient) #3.78016
mse_gradient_sy <- mean((motor_less$stator_yoke-pred_gbm)^2)
print(mse_bagging_sy) #2.541354
r2_gradient <- rSquared((testdataless$pm+testdataless$stator_yoke), (testdataless$pm+testdataless$stator_yoke)-predict(fit_gradient, testdataless, n.trees=fit_gradient$n.trees))
print(r2_gradient) #0.9759362
rmse_gbm = sqrt( mean( (traindataless$pm+stator_yoke) - pred_gbm)^2 )
print(rmse_gbm) # 0.798006

