################################################################################
#                                 Classification
################################################################################


######################### Part 1 - Data Loading and Pre-processing #############

# data used - https://archive.ics.uci.edu/ml/datasets/census+incom

#1.0-----------------Downloading the libraries----------------------------------

library(caTools) ;library(RColorBrewer) ;library(ggplot2)
library(car) ; library(corrplot); library(caTools) 
library(tidyr); library(dplyr); library(tidyverse)
library(GGally); library(ggcorrplot);  library(ggplot2)
library(tidyverse); library(cluster); library(purrr)
library(dendextend); library(DataExplorer); library(factoextra)
library(gridExtra); library(party); library(rpart)
library(rpart.plot); library(mlbench); library(caret)
library(pROC); library(tree); library(rpart)
library(nnet); library(randomForest); options(scipen = 1)

#1.0-------------------------------Reading in data------------------------------

data_import <- read.table("income.txt",header= F, sep=",")
colnames(data_import) <- c("AGE","WORKCLASS","FNLW","EDUCATION","EDUCATION-NUM","MARITAL STATUS","OCCUPATION","RELATIONSHIP","RACE","SEX","CAPITAL_GAIN","CAPITAL_LOSS","HOURS_PER_WEEK","COUNTRY","INCOME")
data_import

#1.1------------------------------Data Cleaning---------------------------------

# Construcing a plot to identify missing values
plot_missing(
  title = "Missing Values",
  data_import, 
  missing_only = FALSE,
  ggtheme = theme_dark(),
  theme_config = list(legend.position = c("bottom"))
)


#Removing Outliers
#FOR FNLWGT
data_fnlw <- data_import[order(data_import$FNLW, decreasing = FALSE), ]
lower_qf <- data_fnlw$FNLW[round(((nrow(data_import)+1)*0.25),digits=0)] 
upper_qf <- data_fnlw$FNLW[round(((nrow(data_import)+1)*0.75),digits=0)]
iqr_qf <- upper_qf - lower_qf
data_import <- subset(data_import, data_import$FNLW > (lower_qf - 3*iqr_qf) & 
                        data_import$FNLW < (upper_qf + 3*iqr_qf) )

boxplot(data_import$FNLW)
#REMOVING UNKOWN VALUES
data_import <- subset(data_import, data_import$WORKCLASS != " ?")
data_import <- subset(data_import, data_import$OCCUPATION != " ?")

#PRE PROCESSING FOR DATA VISUALIZATION
data_import$FIN_INC <- ifelse(data_import$INCOME == " <=50K", 0, 1)
data_modified <- data_import
unique(data_import$INCOME)
data_modified$WORKCLASS <- ifelse(data_modified$WORKCLASS == ' Federal-gov', 1 , 
                                  ifelse(data_modified$WORKCLASS == ' State-gov' , 2 , 
                                         ifelse(data_modified$WORKCLASS == ' Local-gov' , 3 , 
                                                ifelse(data_modified$WORKCLASS == ' Private' , 4 ,
                                                       ifelse(data_modified$WORKCLASS == ' Self-emp-inc' , 5 ,
                                                              ifelse(data_modified$WORKCLASS == ' Self-emp-not-inc' , 6 ,
                                                                     ifelse(data_modified$WORKCLASS == ' Without-pay' , 7 , 8)))))))

data_modified <- subset(data_modified, select = -(EDUCATION))

data_modified$`MARITAL STATUS` <- ifelse(data_modified$`MARITAL STATUS` == ' Never-married', 1 , ifelse(data_modified$`MARITAL STATUS` == ' Married-spouse-absent' , 2 , 
                                                                                                        ifelse(data_modified$`MARITAL STATUS` == ' Married-civ-spouse' , 3 , 
                                                                                                               ifelse(data_modified$`MARITAL STATUS` == ' Married-AF-spouse' , 4 ,
                                                                                                                      ifelse(data_modified$`MARITAL STATUS` == ' Separated' , 5 ,
                                                                                                                             ifelse(data_modified$`MARITAL STATUS` == ' Divorced' , 6 , 7))))))

data_modified$OCCUPATION <- ifelse(data_modified$OCCUPATION == ' Adm-clerical', 1 , ifelse(data_modified$OCCUPATION == ' Exec-manageria' , 2 , 
                                                                                           ifelse(data_modified$OCCUPATION == ' Handlers-cleaners' , 3 , 
                                                                                                  ifelse(data_modified$OCCUPATION == ' Prof-specialty' , 4 ,
                                                                                                         ifelse(data_modified$OCCUPATION == ' Sales' , 5 ,
                                                                                                                ifelse(data_modified$OCCUPATION == ' Craft-repair' , 6 ,
                                                                                                                       ifelse(data_modified$OCCUPATION == ' Transport-moving' , 7 , 
                                                                                                                              ifelse(data_modified$OCCUPATION == ' Farming-fishing' , 8 , 
                                                                                                                                     ifelse(data_modified$OCCUPATION == ' Machine-op-inspct' , 9 ,
                                                                                                                                            ifelse(data_modified$OCCUPATION == ' Tech-support' , 10 ,
                                                                                                                                                   ifelse(data_modified$OCCUPATION == ' Protective-serv' , 11 ,
                                                                                                                                                          ifelse(data_modified$OCCUPATION == ' Armed-Forces' , 12 ,
                                                                                                                                                                 ifelse(data_modified$OCCUPATION == ' Priv-house-serv' , 13 , 14)))))))))))))

data_modified$`RELATIONSHIP` <- ifelse(data_modified$`RELATIONSHIP` == ' Husband', 1 , ifelse(data_modified$`RELATIONSHIP` == ' Wife' , 2 , 
                                                                                              ifelse(data_modified$`RELATIONSHIP` == ' Own-child' , 3 , 
                                                                                                     ifelse(data_modified$`RELATIONSHIP` == ' Unmarried' , 4 ,
                                                                                                            ifelse(data_modified$`RELATIONSHIP` == ' Not-in-family' , 5 ,6 )))))

data_modified$`RACE` <- ifelse(data_modified$`RACE` == ' White', 1 , 
                               ifelse(data_modified$`RACE` == ' Black' , 2 , 
                                      ifelse(data_modified$`RACE` == ' Asian-Pac-Islander' , 3 , 
                                             ifelse(data_modified$`RACE` == ' Amer-Indian-Eskimo' , 4 ,5))))

data_modified$SEX <- ifelse(data_modified$SEX == ' Male', 0 , 1)

data_modified$WORKCLASS <- as.factor(data_modified$WORKCLASS)
data_modified$`EDUCATION-NUM` <- as.factor(data_modified$`EDUCATION-NUM`)
data_modified$`MARITAL STATUS` <- as.factor(data_modified$`MARITAL STATUS`)
data_modified$OCCUPATION <- as.factor(data_modified$OCCUPATION)
data_modified$RELATIONSHIP <- as.factor(data_modified$RELATIONSHIP)
data_modified$RACE <- as.factor(data_modified$RACE)

data_for_sp <- data_modified[,(1:12)]
data_for_sp <- lapply(data_for_sp,as.numeric)


######################## Part 2 - Exploratory Data Analysis ####################

data_import %>%
  gather(Attributes, value, c(2, 4, 6, 7, 8, 9, 10, 15)) %>%
  ggplot(aes(x = value, fill = Attributes)) +
  geom_histogram(colour = "white", show.legend = FALSE, stat = "count") +
  facet_wrap(~Attributes, scales = "free_x") +
  labs(x = "Values", y = "Frequency", title = "Exploratory Data Analysis") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),plot.title = element_text(hjust = 0.5))

######################### Part 3 - Model Selection and Fitting  ################

#MODEL SELECTION

data_import$FIN_INC <- as.factor(data_import$FIN_INC)
data_import$INCOME <- as.factor(data_import$INCOME)
model_1 <- glm(FIN_INC ~ . - INCOME - EDUCATION, data=data_import, family = binomial)
summary(model_1)
model_1_prob <- predict(model_1, type = "response")
length(model_1_prob)
contrasts(data_import$INCOME)
model_1_pred <- rep(" <=50K",30570)
model_1_pred[model_1_prob>0.5] = " >50K"
table(model_1_pred,data_import$INCOME)
mean(model_1_pred == data_import$INCOME)
error_rate =   ((2965+1659)/length(model_1_pred))
print(paste("THE MISCLASSIFICATION ERROR RATE FOR MODEL 1:" ,error_rate))
#0.15122

#WE CAN SEE RACE AND COUNTRY ARE NOT SIGNIFICANT AT 1% SIGNIFICANCE LEVEL

model_2 <- glm(FIN_INC ~ . - INCOME - EDUCATION - RACE - COUNTRY, data=data_import, family = binomial)
summary(model_2)
model_2_prob <- predict(model_2, type = "response")
length(model_2_prob)
contrasts(data_import$INCOME)
model_2_pred <- rep("<=50K",30570)
model_2_pred[model_2_prob>0.5] = ">50K"
table(model_2_pred,data_import$INCOME)
error_rate_2 <- (2992+1671)/length(model_2_pred)
print(paste("THE MISCLASSIFICATION ERROR RATE FOR MODEL 2:" ,error_rate_2))
#0.15251

#MODEL 1 HAS A LOWER ERROR RATE AND HENCE THOSE PREDICTORES ARE PREFERRED


## 3.0-------------Splitting our Data for Training and Testing purposes -> 80% train, 20% test

library(class)
set.seed(7)
train <- sample.split(Y = data_modified, SplitRatio = 0.9)
trainset <- subset(data_modified, train == T)
testset <- subset(data_modified, train == F)
testset_inc <- testset$INCOME
train_fin_inc <-trainset$FIN_INC
test_fin_inc <- testset$FIN_INC
trainset <- trainset[,c(1,2,3,4,5,6,7,8,9,10,11,12,15)]
testset <- testset[,c(1,2,3,4,5,6,7,8,9,10,11,12,15)]

# 3.1-------------------------------KNN-----------------------------------------


knn_pred <- testset[,13]
k_optm=rep(1,50)
#PREDICTING OPTIMAL VALUE FOR K
for (i in 1:50){
  knn_mod <- knn(trainset, testset,train_fin_inc, k=i)
  k_optm[i] <- 100 * sum(knn_pred == knn_mod)/NROW(testset)
}
k_optm <- as.data.frame(k_optm)
k_optm$INDEX <- 1:50
colnames(k_optm) <- c('ACCURACY_LEVELS','INDEX')
max_accuracy_row <- k_optm[which.max(k_optm$ACCURACY_LEVELS), ]
#PLOTTING THE ACCURACY LEVELS FOR DIFFERENT VALUES OF K 
ggplot(data=k_optm, aes(x=INDEX, y=ACCURACY_LEVELS, group=1)) +
  geom_line( color = "red") + 
  geom_point(data = max_accuracy_row, aes(x = INDEX, y = ACCURACY_LEVELS), color = "steelblue", size = 3) +
  geom_vline(xintercept = max_accuracy_row$INDEX, linetype = "dashed", color = "black") +
  ggtitle("Optimal Number for K")

print(paste("THE VALUE OF K =", k_optm$INDEX[max(k_optm$ACCURACY_LEVELS) == k_optm$ACCURACY_LEVELS] ,"IS THE MOST ACCURATE, WITH ACCURACY LEVEL OF",  round(max(k_optm$ACCURACY_LEVELS),2)))
knn_pred_prob <- knn(trainset, testset,train_fin_inc, k=29)
#THE OPTIMAL NUMBER OF K NEAREST NEIGHBORS IS 29
old <- Sys.time()
knn_pred <- ifelse(knn_pred_prob == 1," >50K"," <=50K")
#COMPUTING CONFUSION MATRIX
table(knn_pred,testset_inc)
#CONFUSION MATRIX
#         testset_inc
# knn_pred  <=50K  >50K
#    <=50K   3036   812
#    >50K      34   195

mean(knn_pred==testset_inc)
#MODEL ACCURACY = 0.7924945
computation_time <- Sys.time()-old
# Time difference of 0.1206419 secs


# 3.2----------------Logistic Regression----------------------------------------

#TRAIN AND TEST SET
library(MASS)
old <- Sys.time()
train_model <- glm(FIN_INC ~ ., data=trainset, family = binomial)
summary(train_model)
test_prob <- predict(train_model, testset, type= "response")
test_pred <- rep(" <=50K",4077)
test_pred[test_prob>0.5] <- " >50K"
#COMPUTING CONFUSION MATRIX
table(test_pred, testset_inc)
#        testset_inc
#test_pred  <=50K  >50K
#    <=50K   2851   418
#   >50K     219   589
mean(test_pred == testset_inc)
#MODEL ACCURACY = 0.8437577
computation_time <- Sys.time()-old
#Time difference of 3.608204 secs


# 3.3----------------Linear Discriminant Analysis-------------------------------


library(MASS)
old <- Sys.time()
lda_fit=lda(FIN_INC ~ . , data=trainset)
#plot(lda_fit)
lda_pred_prob=predict(lda_fit, testset)$class
lda_pred_2 <- predict(lda_fit, testset)$posterior
lda_pred <- rep(" <=50K",4077)
lda_pred[lda_pred_prob==1] <- " >50K"
#COMPUTING CONFUSION MATRIX
table(lda_pred,testset_inc)
#  testset_inc
# lda_pred  <=50K  >50K
#    <=50K   2855   469
#    >50K     215   538
mean(lda_pred == testset_inc)
#MODEL ACCURACY =  0.8322296
computation_time <- Sys.time()-old
#Time difference of 1.036161 secs


# 3.4----------------Decision Trees---------------------------------------------

library(partykit)
old <- Sys.time()
trees_fit <- ctree(FIN_INC ~ ., trainset)
plot(trees_fit)
trees_pred_prob <- predict(trees_fit,testset)
#NUMBER OF SPLITS
no_of_splits <- capture.output(print(trees_fit[1]))
#NUMBER OF INNER NODES: 78
trees_pred <- rep(" <=50K",4077)
trees_pred[trees_pred_prob>0.5] <- " >50K"
#CONFUSION MATRIX
table(trees_pred, testset_inc)
#   testset_inc
# trees_pred  <=50K  >50K
#      <=50K   2877   420
#      >50K     193   587
mean(test_pred == testset_inc)
#MODEL ACCURACY = 0.8437577
computation_time <- Sys.time()-old
#Time difference of 1.261948 secs

# 3.5----------------Random Forest----------------------------


colnames(trainset)[colnames(trainset) == "EDUCATION-NUM"] <- "EDUCATION"
colnames(trainset)[colnames(trainset) == "MARITAL STATUS"] <- "MARTIAL_STATUS"
colnames(testset)[colnames(testset) == "EDUCATION-NUM"] <- "EDUCATION"
colnames(testset)[colnames(testset) == "MARITAL STATUS"] <- "MARTIAL_STATUS"

old <- Sys.time()
rf_fit <- randomForest(FIN_INC ~ ., data = trainset,ntree = 100)
rf_pred_prob <- predict(rf_fit,testset)
rf_pred <- rep(" <=50K",4077)
rf_pred[rf_pred_prob>0.5] <- " >50K"
#CONFUSION MATRIX
table(rf_pred,testset_inc)
#         testset_inc
# rf_pred   <=50K  >50K
#    <=50K   2886   397
#    >50K     184   610
mean(rf_pred==testset_inc)
#MODEL ACCURACY = 0.8574933
#MEAN SQUARED ERROR FOR RANDOM FOREST
mean((rf_pred_prob-test_fin_inc)^2)
#0.09940933
computation_time <- Sys.time()-old
#Time difference of 3.291192 mins


############################# Part 4 - Model Evaluation ###########################################

#ROC CURVES
library(pROC)
#LOGISTIC REGRESSION
roc_scores_lg <- roc(testset$FIN_INC,as.numeric(test_prob), direction = "<")
auc_lg <- roc_scores_lg$auc #Area under the curve: 0.8999
#RANDOM FOREST
roc_scores_rf <- roc(testset$FIN_INC,as.numeric(rf_pred_prob),direction = "<")
auc_rf <- roc_scores_rf #Area under the curve: 0.9111
#KNN
roc_scores_knn <- roc(testset$FIN_INC,as.numeric(knn_pred_prob))
auc_knn <- roc_scores_knn #Area under the curve: 0.5918
#LINEAR DISCRIMINANT ANALYSIS
roc_scores_lda <- roc(testset$FIN_INC,as.numeric(lda_pred_2[,2]),direction = "<")
auc_lda <- roc_scores_lda #Area under the curve: 0.8856
#DECISION TREES
roc_scores_trees <- roc(testset$FIN_INC,as.numeric(trees_pred_prob),direction = "<")
auc_trees <- roc_scores_trees #Area under the curve: 0.9052


plot_roc <- ggroc(list(Logistic_regression = roc_scores_lg, KNN = roc_scores_knn,
                       Random_forest  = roc_scores_rf, LDA = roc_scores_lda,
                       Decision_trees = roc_scores_trees)) +labs(title ="ROC Curves for the different models" )+
  theme(legend.position = c(0.95, 0.2), legend.justification = c(1, 0),
        plot.title = element_text(hjust = 0.5)) 

plot_roc 


#####################################################################################################



################################################################################
#                                 Regression
################################################################################


################### Part 1 - Data Loading and Pre-processing ###################

# data used - https://www.kaggle.com/datasets/muonneutrino/us-census-demographic-data?select=acs2017_census_tract_data.csv

#1.0------------------------------Reading in data-------------------------------
data <- read.csv("acs2017_census_tract_data.csv")
head(data)
summary(data)
names(data)

#1.1------------------------------Data Cleaning---------------------------------
missing <-colSums(is.na(data)) 
#Removing TractID, State, County -> Data is grouped by TractID
#These categorical variables have no impact on final outcome
dif <- setdiff(names(data), c("TractId", "State", "County"))
data <- data[, dif]
#Numerical data imputation with column mean
for (i in names(data)) {
  data[, i][is.na(data[, i])] <- mean(data[, i], na.rm = TRUE)
}

###################### Part 2 - Exploratory Data Analysis ######################

# 2.0 --------------------------Correlation Matrix------------------------------
cor(data)

library(ggplot2)
cor_matrix <- cor(data)

# Create a heatmap using ggplot2
library(reshape2)
melted_cor_matrix <- melt(cor_matrix)
head(melted_cor_matrix)
library(ggplot2)
ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))


######################## Part 3 - Model Fitting  ###############################


# 3.0-------------Splitting our Data for Training and Testing purposes -> 80% train, 20% test
set.seed(123)
train_data = sample(nrow(data), size = 0.8* nrow(data))
train <- data[train_data,]
test <- data[-train_data,]



# 3.1-----------------------------Linear Regression-----------------------------

#Fitting our model using lm
old <- Sys.time() 
lm.fit = lm(Income ~ ., data = train) 
time.lm <- Sys.time() - old
time.lm
summary(lm.fit)

#Predicting using test data
predictions <- predict(object = lm.fit, newdata = test)
#Calculating RMSE
lm.rmse = sqrt(mean(((test$Income - predictions)) ^ 2))
lm.rmse



# 3.2---------------------------Log Linear Regression---------------------------

#Response Variable is left skewed -> calls for Log Linear Regression
hist(train$Income)


#Fitting our model using lm(log)
old <- Sys.time()
loglinear = lm(log(Income) ~., data = train)
time.ll <- Sys.time() - old
time.ll
summary(loglinear)

#Predicting using test data
ll.predictions <- exp(predict(object = loglinear, newdata = test))
#Calculating RMSE
ll.rmse = sqrt(mean((test$Income - ll.predictions) ^ 2))
ll.rmse 


# 3.3----------------------------Lasso Regression-------------------------------

library(glmnet)

#Splitting our data into X train, X test, Y train, Y test
x_train <-model.matrix(Income~., data=train)[,-1]
y_train <-train$Income
x_test <- model.matrix(Income ~ ., data = test)[,-1]
y_test <- test$Income

old <- Sys.time()
#Fitting our model using GLMNET
fit.lasso <-glmnet(x_train, y_train)
plot(fit.lasso, xvar = "lambda", label= TRUE)
plot(fit.lasso, xvar = "dev", label= TRUE)
cv.lasso <-cv.glmnet(x_train, y_train)
plot(cv.lasso)
coef(cv.lasso)

#Solving for coefficients and taking lambda minimum
coef(glmnet(x_train,y_train, lambda=cv.lasso$lambda.min))
lasso.train <- glmnet(x_train, y_train)
#Predicting using test data
pred.test <-predict(lasso.train, x_test)
dim(pred.test)

#Calculating RMSE using lambda best
rmse <-sqrt(apply((y_test-pred.test)^2,2,mean))
plot(log(lasso.train$lambda), rmse, type="b", xlab="Log(lambda)")
lambda.best <-lasso.train$lambda[order(rmse)[1]]
lambda.best

lasso.fit = glmnet(x_train,y_train, lambda=lambda.best)
time.lasso <- Sys.time() - old
time.lasso

lasso.predictions <- predict(object = lasso.fit, newx = x_test)
lasso.rmse = sqrt(mean((test$Income - lasso.predictions) ^ 2))
lasso.rmse


# 3.4---------------------Ridge Regression--------------------------------------

old <- Sys.time()
#Grid for lambda values
grid <-10^seq(10, -2, length=100)
#Fitting our model using GLMNET, alpha = 0
fit.ridge <-glmnet(x_train, y_train, alpha=0)
plot(fit.ridge, xvar="lambda", label= TRUE)
plot(fit.ridge, xvar="dev", label= TRUE)
cv.ridge <-cv.glmnet(x_train, y_train, alpha=0, lambda = grid)
plot(cv.ridge)
coef(cv.ridge)
time.ridge <- Sys.time() - old
time.ridge

#Predicting using test data
ridge.predictions <- predict(cv.ridge, s = "lambda.min", newx = x_test)
#Calculating RMSE using lambda minimum
rmse_ridge <- sqrt(mean((y_test - ridge.predictions)^2))
rmse_ridge



# 3.5--------------------------------GAM----------------------------------------
old <- Sys.time()
library(mgcv)
#Fitting our models while considering smoothing factors
gam.fit <- gam(Income ~ s(TotalPop) + s(Men) + s(Women) + s(Hispanic) + s(White) + 
                 s(Black) + s(Native) + s(Asian) + s(Pacific) + s(VotingAgeCitizen) + 
                 + s(IncomeErr) + s(IncomePerCap) + s(IncomePerCapErr) + 
                 s(Poverty) + s(ChildPoverty) + s(Professional) + s(Service) + 
                 s(Office) + s(Construction) + s(Production) + s(Drive) + 
                 s(Carpool) + s(Transit) + s(Walk) + s(OtherTransp) + s(WorkAtHome) + 
                 s(MeanCommute) + s(Employed) + s(PrivateWork) + s(PublicWork) + 
                 s(SelfEmployed) + s(FamilyWork) + s(Unemployment), 
               data = train)
time.gam <- Sys.time() - old
time.gam

summary(gam.fit) #all the variables are significant

gam.predictions <- predict(object = gam.fit, newdata = test)
gam.rmse <- sqrt(mean(((test$Income - gam.predictions)) ^ 2))
gam.rmse 
time.gam <- Sys.time() - old
time.gam


# 3.6 -------------------- Random Forest Model ---------------------------------

old <- Sys.time()
library(randomForest)
set.seed(1)
## Recall that bagging is simply a special case of a random forest with m=p, here we use mtry=13
rf.fit <- randomForest(Income~., data= train, mtry=13, importance=TRUE, ntree = 100)
(time.rf <- Sys.time() - old)

## Predicted values on the testing data using bagging
yhat  <-predict(rf.fit, newdata= test)
plot(yhat, test$Income)
abline(0,1)
## Compute the test RMSE
rf.rmse = sqrt(mean((yhat-test$Income)^2))
rf.rmse
## We can view the importance of each variable
importance(rf.fit)
# The plot indicates lstat and rm are two most important variables
varImpPlot(rf.fit)


########################## Part 4 - Model Evaluation ###########################

library(ggplot2)
# Creating a data frame with actual vs predicted values for each model
input_data <- subset(data, select = -Income)

results <- data.frame(
  Actual = data$Income,
  LinearRegression = predict(object = lm.fit, newdata = input_data),
  LogLinearRegression = exp(predict(object = loglinear, newdata = input_data )),
  GAM = predict(object = gam.fit, newdata = input_data),
  Ridge = predict(cv.ridge, newx = as.matrix(input_data)),
  Lasso = predict(lasso.fit, newx = as.matrix(input_data)),
  RandomForest = predict(object = rf.fit, newdata = input_data)
)

# Reshape the data for easier plotting
library(tidyr)
results_long <- gather(results, key = "Model", value = "Predicted", -Actual)

# Creating scatter plots comparing actual and predicted values
library(ggplot2)
ggplot(results_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Actual vs Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal()


ggplot(results_long, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Actual vs Predicted Values",
       x = "Actual Values",
       y = "Predicted Values") +
  theme_minimal() +
  facet_wrap(~Model, scales = "fixed")


# performance metric table
performance_table <- data.frame(
  Model = c("Linear Regression", "LogLinear Regression", "GAM", "Ridge", "Lasso", "Random Forest"),
  RMSE = c(lm.rmse, ll.rmse, gam.rmse, rmse_ridge, lasso.rmse, rf.rmse)
)
# Print the performance table
print(performance_table)





