#Author: Suneesha Kudipudi

# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(h2o)
source('scripts/air_definitions.R')

set.seed(1)

# load data
df_train = read_csv("train_users_2.csv")
df_test = read_csv("test_users.csv")
labels = df_train['country_destination']
df_train = df_train[,-c(16)]

# combine train and test data
df_all = rbind(df_train,df_test)
# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]

#check for the NA columns
cat("NA columns are ...")
names(which(sapply(df_all, function(x)(any(is.na(x))))==TRUE))


# replace missing values
df_all[is.na(df_all)] <- -1

# split date_account_created in year, month and day
dac = as.data.frame(str_split_fixed(df_all$date_account_created, '-', 3))
df_all['dac_year'] = as.numeric(dac[,1])
df_all['dac_month'] = as.numeric(dac[,2])
df_all['dac_day'] = as.numeric(dac[,3])
date_account_created<-as.Date(df_all$date_account_created)
df_all['week_ID']<-as.numeric(format(as.POSIXct(date_account_created), "%U"))
df_all = df_all[,-c(which(colnames(df_all) %in% c('date_account_created')))]


# split timestamp_first_active in year, month and day
df_all['tfa_year'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 1, 4))
df_all['tfa_month'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 5, 6))
df_all['tfa_day'] = as.numeric(substring(as.character(df_all[,'timestamp_first_active']), 7, 8))
df_all = df_all[,-c(which(colnames(df_all) %in% c('timestamp_first_active')))]


# clean Age by removing values
df_all[df_all$age < 14 | df_all$age > 100,'age'] <- -1

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')
dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)
df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

#removing the old datasets for 
rm(df_all)
rm(df_all_ohe)

# split train and test
X_train = df_all_combined[df_all_combined$id %in% df_train$id,]
y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
X_test = df_all_combined[df_all_combined$id %in% df_test$id,]

X_train<-cbind(X_train,y)

#names(which(sapply(df_all, function(x)(any(is.na(x))))==TRUE))

rm(df_all_combined)

trainIndex<-generatefolds(y,0.8,99)
k<-as.vector(trainIndex[[1]])
valIndex<-as.vector(trainIndex[[2]])


#h2o randomForest
cat("Applying the model")

#create training frame in H20:
#start h20 and convert dataframe:
set.seed(777)
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')

trainHex<-as.h2o(X_train[,-c(1)])
testHex<-as.h2o(X_test[,-1])

trainHex[,146]<-as.factor(trainHex[,146])


## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=c(1:145),
                          y=146, 
                          ntrees = 250,
                          max_depth = 10,
                          mtries=20,
                          balance_classes=FALSE,
                          sample_rate = 0.7,
                          nbins_cats = 69, ## allow it to fit department descriptions
                          training_frame=trainHex[k,],
                          validation_frame =trainHex[valIndex,] )
summary(rfHex)
rfHex@model$validation_metrics
cv<-1
#crossvalidation of NCDG score
if(cv==1)
{
  predictions<-as.data.frame(h2o.predict(rfHex,trainHex[valIndex,]))
  predictions<-predictions[,-1]
  colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
  predictions_top5 <- matrix(as.vector(apply(predictions, 1, function(x) names(sort(x)[12:8]))),nrow=length(valIndex), byrow= TRUE)
  #true_obs=recode(labels[-k],"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")
  true_obs<-c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')[y[valIndex]+1]
  score <- score_predictions( predictions_top5, true_obs)
  print(mean(score))
}


predictions<-as.data.frame(h2o.predict(rfHex,testHex))
predictions<-predictions[,-1]
colnames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
#writing out the prediction set
id<-X_test$id
predictions<-cbind(id,predictions)
predictions<-predictions[order(id),]
write.csv(file="h2rf_prob.csv",predictions,row.names=FALSE)
h2o.shutdown()