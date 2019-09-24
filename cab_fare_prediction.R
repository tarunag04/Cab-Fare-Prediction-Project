rm(list=ls())

# set working directory
setwd("F:/R_Programming/Edwisor")
getwd()

##############################################################

# loading Libraries
x = c("tidyr", "ggplot2", "corrgram", "usdm", "caret", "DMwR", "rpart", "randomForest",'xgboost')

# tidyr - drop_na
# ggplot2 - for visulization, boxplot, scatterplot
# corrgram - correlation plot
# usdm - vif
# caret - createDataPartition
# DMwR - regr.eval
# rpart - decision tree
# randomForest - random forest
# xgboost - xgboost

# load Packages
lapply(x, require, character.only = TRUE)
rm(x)

#############################################################

# loading datasets
train = read.csv("train_cab.csv", header = T, na.strings = c(" ", "", "NA"))
test = read.csv("test_cab.csv")

######################
# Exploring Datasets
######################

# Structure of data
str(train)
str(test)

# Summary of data
summary(train)
summary(test)

# Viewing the data
head(train,5)
head(test,5)

#####################################
# EDA, Missing value and Outlier analysis
#####################################

# Changing the data types of variables
train$fare_amount = as.numeric(as.character(train$fare_amount))
train$passenger_count = round(train$passenger_count)

# Checking Missing data
apply(train, 2, function(x) {sum(is.na(x))})

#Creating dataframe with missing values present in each variable 
val = data.frame(apply(train, 2, function(x){sum(is.na(x))}))
val$Columns = row.names(val) 
names(val)[1] = "null_percentage"

#Calculating percentage missing value
val$null_percentage = (val$null_percentage/nrow(train)) * 100

# Sorting null_val in Descending order
val = val[order(-val$null_percentage),] 
row.names(val) = NULL
# Reordering columns 
val = val[,c(2,1)]

#viewing the % of missing data for all variales 
val

# delete the rows having missing values
train = drop_na(train)

# Verifying missing values after deletion 
sum(is.na(train))

#Splitting Date and time on train data
train$pickup_date = as.Date(as.character(train$pickup_datetime))
train$weekday = as.factor(format(train$pickup_date,"%u")) # Monday = 1
train$month = as.factor(format(train$pickup_date,"%m"))
train$year = as.factor(format(train$pickup_date,"%Y"))
pickup_time = strptime(train$pickup_datetime,"%Y-%m-%d %H:%M:%S")
train$hour = as.factor(format(pickup_time,"%H"))

# Now drop the column pickup_datetime and pickup_date from train data
train = subset(train, select = -c(pickup_datetime))
train = subset(train, select = -c(pickup_date))

#Splitting Date and time on test data
test$pickup_date = as.Date(as.character(test$pickup_datetime))
test$weekday = as.factor(format(test$pickup_date,"%u")) # Monday = 1
test$month = as.factor(format(test$pickup_date,"%m"))
test$year = as.factor(format(test$pickup_date,"%Y"))
pickup_time_test = strptime(test$pickup_datetime,"%Y-%m-%d %H:%M:%S")
test$hour = as.factor(format(pickup_time_test,"%H"))

# Now drop the column pickup_datetime and pickup_date from test data
test = subset(test, select = -c(pickup_datetime))
test = subset(test, select = -c(pickup_date))

# Make a copy
df_train = train
df_test = test

#train = df_train
#test = df_test

# calculate distance
my_dist = function(long1, lat1, long2, lat2) {
  rad = pi/180
  a1 = lat1*rad
  a2 = long1*rad
  b1 = lat2*rad
  b2 = long2*rad
  dlon = b2 - a2
  dlat = b1 - a1
  a = (sin(dlat/2))^2 + cos(a1)*cos(b1)*(sin(dlon/2))^2
  c = 2*atan2(sqrt(a), sqrt(1 - a))
  R = 6371
  d = R*c
  return(d)
}

#Running the distance function for all rows in train dataframe 
for (i in 1:nrow(train)){
  train$distance[i]= my_dist(train$pickup_longitude[i], train$pickup_latitude[i], train$dropoff_longitude[i], train$dropoff_latitude[i])
}

#Running the distance function for all rows in test dataframe 
for (i in 1:nrow(test)){
  test$distance[i]= my_dist(test$pickup_longitude[i], test$pickup_latitude[i], test$dropoff_longitude[i], test$dropoff_latitude[i])
}

# remove the variables which were used to feature engineer new variables
train = subset(train,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))
test = subset(test,select = -c(pickup_longitude,pickup_latitude,dropoff_longitude,dropoff_latitude))

summary(train)

# in train data, passangers count can not be zero or more than 6
# so, remove rows having passangers count zero or more than 6
train$passenger_count[train$passenger_count<1] = NA 
train$passenger_count[train$passenger_count>6] = NA

sum(is.na(train))
train = drop_na(train)

# Make a copy
df_train1 = train
df_test1 = test

# creating boxplot of the continous variables to check outlier
ggplot(data = train, aes(x = "", y = distance)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150))

ggplot(data = train, aes(x = "", y = fare_amount)) + 
  geom_boxplot() +
  coord_cartesian(ylim = c(0, 150))

# sorting fare data in decending order to check outlier
train_fare_dec = train$fare_amount
train_fare_dec = sort(train_fare_dec, decreasing = TRUE, na.last = TRUE)

# fare_amount greater than 100 seems to be outlier
# fare_amount can not be zero  or negative
# drop rows having fare negative, zero and greater than 100.

train$fare_amount[train$fare_amount<1] = NA 
train$fare_amount[train$fare_amount>100] = NA

sum(is.na(train))
train = drop_na(train)

# sorting distance in decending order to check outlier for train data
train_distance_dec = train$distance
train_distance_dec = sort(train_distance_dec, decreasing = TRUE, na.last = TRUE)

# distance greater than 30 km seems to be outlier
# distance can not be zero  or negative
# drop rows having distance negative, zero and greater than 30 km.

train$distance[train$distance<=0] = NA 
train$distance[train$distance>30] = NA

sum(is.na(train))
train = drop_na(train)

# sorting distance in decending order to check outlier for test data
test_distance_dec = test$distance
test_distance_dec = sort(test_distance_dec, decreasing = TRUE, na.last = TRUE)

test$distance[test$distance<=0] = NA 
test$distance[test$distance>30] = NA

sum(is.na(test))
test = drop_na(test)

# Make a copy
df_train2 = train
df_test2 = test
#train=df_train2
#test=df_test2

############### Visualization ########################
# Scatter plot between distance and fare on train data
ggplot(data = train, aes_string(x = train$distance, y = train$fare_amount))+ 
  geom_point()

# Scatter plot between passenger and fare on train data
ggplot(data = train, aes_string(x = train$passenger_count, y = train$fare_amount))+ 
  geom_point()

# Scatter plot between weekday and fare on train data
ggplot(data = train, aes_string(x = train$weekday, y = train$fare_amount))+ 
  geom_point()

# Scatter plot between hour and fare on train data
ggplot(data = train, aes_string(x = train$hour, y = train$fare_amount))+ 
  geom_point()

##################### Feature Selection #######################
# generate correlation plot between numeric variables

numeric_index=sapply(train, is.numeric)
corrgram(train[,numeric_index], order=F, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Correlation plot")

# check VIF
vif(train[,-1])
# if vif is greater than 10 then variable is not suitable/multicollinerity

# creating dummy variables for categorical variables
# require(fastDummies)
# results = fastDummies::dummy_cols(train)

################ Feature Scaling ################
# check normality - single continous variable
qqnorm(train$fare_amount)
hist(train$fare_amount)

qqnorm(train$distance)
hist(train$distance)

train[,'distance'] = (train[,'distance'] - min(train[,'distance']))/
  (max(train[,'distance'] - min(train[,'distance'])))

test[,'distance'] = (test[,'distance'] - min(test[,'distance']))/
  (max(test[,'distance'] - min(test[,'distance'])))

######################### Model Development ###################

############ Splitting train into train and test ###################
set.seed(101)
split_index = createDataPartition(train$fare_amount, p = 0.75, list = FALSE) 
train_data = train[split_index,]
test_data = train[-split_index,]

#############  Linear regression Model  #################
lm_model = lm(fare_amount ~., data=train_data)

# summary of trained model
summary(lm_model)

# residual plot
plot(lm_model$fitted.values,rstandard(lm_model),main = "Residual plot",
     xlab = "Predicted values of fare_amount",
     ylab = "standardized residuals")

# prediction on test_data
lm_predictions = predict(lm_model,test_data[,2:7])

qplot(x = test_data[,1], y = lm_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1],lm_predictions)
#        mae        mse       rmse       mape 
#     2.1719005 17.8044656  4.2195338  0.2130411

# compute r^2
rss_lm = sum((lm_predictions - test_data$fare_amount) ^ 2)
tss_lm = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_lm = 1 - rss_lm/tss_lm
#    r^2 - 0.7966621

############## Decision Tree Model ###############
Dt_model = rpart(fare_amount ~ ., data=train_data, method = "anova")

# summary on trainned model
summary(Dt_model)

#Prediction on test_data
predictions_DT = predict(Dt_model, test_data[,2:7])

qplot(x = test_data[,1], y = predictions_DT, data=test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1], predictions_DT)
#       mae       mse      rmse      mape 
#    2.498668 20.471238  4.524515  0.255439

# compute r^2
rss_dt = sum((predictions_DT - test_data$fare_amount) ^ 2)
tss_dt = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_dt = 1 - rss_dt/tss_dt
#    r^2 - 0.766206

#############  Random forest Model #####################
rf_model = randomForest(fare_amount ~., data=train_data)

# summary on trained model
summary(rf_model)

# prediction of test_data
rf_predictions = predict(rf_model, test_data[,2:7])

qplot(x = test_data[,1], y = rf_predictions, data=test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1], rf_predictions)
#        mae        mse       rmse       mape 
#     2.2628045 18.4661108  4.2972213  0.2370227

# compute r^2
rss_rf = sum((rf_predictions - test_data$fare_amount) ^ 2)
tss_rf = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_rf = 1 - rss_rf/tss_rf
#    r^2 - 0.7891057

############  XGBOOST Model ###########################
train_data_matrix = as.matrix(sapply(train_data[-1],as.numeric))
test_data_matrix = as.matrix(sapply(test_data[-1],as.numeric))

xgboost_model = xgboost(data = train_data_matrix,label = train_data$fare_amount, nrounds = 15,verbose = FALSE)

# summary of trained model
summary(xgboost_model)

# prediction on test_data
xgb_predictions = predict(xgboost_model,test_data_matrix)

qplot(x = test_data[,1], y = xgb_predictions, data = test_data, color = I("blue"), geom = "point")

regr.eval(test_data[,1], xgb_predictions)
#        mae        mse       rmse       mape 
#     2.0897725 18.5837037  4.3108820  0.2181592

# compute r^2
rss_xgb = sum((xgb_predictions - test_data$fare_amount) ^ 2)
tss_xgb = sum((test_data$fare_amount - mean(test_data$fare_amount)) ^ 2)
rsq_xgb = 1 - rss_xgb/tss_xgb
#    r^2 - 0.7977628


# from above models, it is clear that xgboost is best model
# so, we are using xgboost to predict test dataset

#############   Final Test data prediction   ####################
# we have already clean the test data
# we use whole training Dataset to predict the fare on test dataset
train_data_matrix2 = as.matrix(sapply(train[-1],as.numeric))
test_data_matrix2 = as.matrix(sapply(test,as.numeric))

xgboost_model2 = xgboost(data = train_data_matrix2,label = train$fare_amount,nrounds = 15,verbose = FALSE)

# Lets now predict on test dataset
xgb = predict(xgboost_model2, test_data_matrix2)

test_xgb_pred = data.frame(df_test2$passenger_count, df_test2$distance,"predictions_fare" = xgb)

write.csv(test_xgb_pred,"test_cab_predicted_fare.csv",row.names = FALSE)

