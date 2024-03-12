v=read.csv("water_potability.csv")
o=nrow(v)
nas<-sum(is.na(v$ph))
#install.packages("VIM")
library(VIM)
k=10
imputed_data=kNN(v,k=k)
#df_imputed <- as.data.frame(imputed_data)

# Calculate the IQR of the specific column----
Q1 <- quantile(df_imputed$ph, 0.25)
Q3 <- quantile(df_imputed$ph, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$ph >= lower_bound & df_imputed$ph <= upper_bound, ]
n=nrow(df_imputed)


#hardness
Q1 <- quantile(df_imputed$Hardness, 0.25)
Q3 <- quantile(df_imputed$Hardness, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Hardness >= lower_bound & df_imputed$Hardness <= upper_bound, ]
n=nrow(df_imputed)

#Solids
Q1 <- quantile(df_imputed$Solids, 0.25)
Q3 <- quantile(df_imputed$Solids, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Solids >= lower_bound & df_imputed$Solids <= upper_bound, ]
n=nrow(df_imputed)

#Chloramines
Q1 <- quantile(df_imputed$Chloramines, 0.25)
Q3 <- quantile(df_imputed$Chloramines, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Chloramines >= lower_bound & df_imputed$Chloramines <= upper_bound, ]
n=nrow(df_imputed)

#Sulfate
Q1 <- quantile(df_imputed$Sulfate, 0.25)
Q3 <- quantile(df_imputed$Sulfate, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Sulfate >= lower_bound & df_imputed$Sulfate <= upper_bound, ]
n=nrow(df_imputed)

#Conductivity
Q1 <- quantile(df_imputed$Conductivity, 0.25)
Q3 <- quantile(df_imputed$Conductivity, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Conductivity >= lower_bound & df_imputed$Conductivity <= upper_bound, ]
n=nrow(df_imputed)

#Organic_carbon
Q1 <- quantile(df_imputed$Organic_carbon, 0.25)
Q3 <- quantile(df_imputed$Organic_carbon, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Organic_carbon >= lower_bound & df_imputed$Organic_carbon <= upper_bound, ]
n=nrow(df_imputed)

#Trihalomethanes
Q1 <- quantile(df_imputed$Trihalomethanes, 0.25)
Q3 <- quantile(df_imputed$Trihalomethanes, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Trihalomethanes >= lower_bound & df_imputed$Trihalomethanes <= upper_bound, ]
n=nrow(df_imputed)

#Turbidity
Q1 <- quantile(df_imputed$Turbidity, 0.25)
Q3 <- quantile(df_imputed$Turbidity, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
df_imputed <- df_imputed[df_imputed$Turbidity >= lower_bound & df_imputed$Turbidity <= upper_bound, ]
n=nrow(df_imputed)


# Class Imbalance----
p=sum(df_imputed$Potability==0)
l=sum(df_imputed$Potability==1)
summary(df_imputed)
#install.packages("ROSE")
library(ROSE)
oversampled_data <- ovun.sample(Potability ~ ., data = df_imputed, method = "over")$data

z=sum(oversampled_data$Potability == 1)
y=sum(oversampled_data$Potability == 0)
i=nrow(oversampled_data)
#write.csv(oversampled_data, "OverSampled_Rose.csv")

# Data Division-----
#install.packages("caret") data splitting
library(caret)
set.seed(123)
split <- createDataPartition(oversampled_data$Potability, p=0.7, list=FALSE)
training_data <- oversampled_data[split, ]
testing_data <- oversampled_data[-split, ]
training_data$Potability <- as.factor(training_data$Potability)

# Decision Tree
#install.packages("rpart")
library(rpart)
model=rpart(Potability ~ ph + Hardness + Solids + Chloramines + Sulfate + Conductivity + Organic_carbon + Trihalomethanes + Turbidity,data = training_data,method = "class")
predictions <- predict(model, testing_data, type = "class")
testing_data$Potability <- as.factor(testing_data$Potability)
cm_dt <- confusionMatrix(predictions, testing_data$Potability)
cat("Decision Tree ")
print(cm_dt)



library(caret)

# Support Vector Machine----
#install.packages("e1071")  support vector machine
ctrl <- trainControl(method = "cv", number = 10)
model_svm1 <- train(Potability ~ ph + Hardness + Solids + Chloramines + Sulfate + Conductivity + Organic_carbon + Trihalomethanes + Turbidity, data = training_data, method = "svmRadial", trControl = ctrl)
predictions_svm <- predict(model_svm1, newdata = testing_data)
cm_svm <- confusionMatrix(predictions_svm, testing_data$Potability)
cat("SVM ")
print(cm_svm)

# Random Forest----
library(randomForest)
fitControl <- trainControl(method = "cv", number = 10)
model_rf <- train(Potability ~ ph + Hardness + Solids + Chloramines + Sulfate + Conductivity + Organic_carbon + Trihalomethanes + Turbidity, data = training_data, method = "rf", trControl = fitControl)
predictions_rf <- predict(model_rf, newdata = testing_data)
cm_rf <- confusionMatrix(predictions_rf, testing_data$Potability)
cat("Random Forest ")
print(cm_rf)

#xgboost
#install.packages("xgboost")
library(xgboost)
xgControl <- trainControl(method = "cv", number = 5)
xgb_grid <- expand.grid(nrounds = 100,  max_depth = c(6, 8, 10),  eta = 0.3,  gamma = 0,  colsample_bytree = 0.6,  min_child_weight = 1,  subsample = 1)
model_xg <- train(Potability ~ ph + Hardness + Solids + Chloramines + Sulfate + Conductivity + Organic_carbon + Trihalomethanes + Turbidity, data = training_data, method = "xgbTree", tuneGrid = xgb_grid)
predictions_xg <- predict(model_xg, newdata = testing_data)
cm_xg <- confusionMatrix(predictions_xg, testing_data$Potability)
cat("XGBoost ")
print(cm_xg)


#install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model, box.palette = "auto")

