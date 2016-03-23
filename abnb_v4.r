# This R script is based on Sandro's python script, which produces a LB score of 0.8655
# This script should produce a LB score of 0.86547

# load libraries
library(xgboost)
library(readr)
library(stringr)
library(caret)
options(stringsAsFactors = FALSE) 
# library(car)
# setwd("c:/Users/manishm/Downloads/abnb/")
setwd("/home/manishm/abnb/")

set.seed(1)
rm(list = ls())
# load data
df_train = read_csv("train_users_2.csv")
df_test = read_csv( "test_users.csv")
labels = df_train[,'country_destination']
df_train = df_train[-grep('country_destination', colnames(df_train))]

# combine train and test data
df_all = rbind(df_train,df_test)

# remove date_first_booking
df_all = df_all[-c(which(colnames(df_all) %in% c('date_first_booking')))]
# replace missing values

df_all$date_account_created = as.Date(df_all$date_account_created,format = "%d-%m-%Y")
df_all = mutate(df_all 
	,ac.currweek = as.integer(format(date_account_created,'%w'))
	,ac.dayofmonth = as.integer(format(date_account_created,'%d'))
	,ac.dayofyear = as.integer(format(date_account_created,'%j'))
	,ac.month = as.integer(format(date_account_created, "%m"))
	,ac.year = as.integer(format(date_account_created, "%Y"))
	)
df_all$date_account_created = NULL

df_all$timestamp_first_active.dt = as.Date(substring(as.character(df_all[,'timestamp_first_active']), 1, 8),format = "%Y%m%d")
df_all$timestamp_first_active = as.POSIXct(as.character(df_all[,'timestamp_first_active']), format = "%Y%m%d%H%M%S")
df_all = mutate(df_all 
	,fa.currweek = as.integer(format(timestamp_first_active.dt,'%w'))
	,fa.dayofmonth = as.integer(format(timestamp_first_active.dt,'%d'))
	,fa.dayofyear = as.integer(format(timestamp_first_active.dt,'%j'))
	,fa.month = as.integer(format(timestamp_first_active.dt, "%m"))
	,fa.year = as.integer(format(timestamp_first_active.dt, "%Y"))
	,fa.hour = as.integer(substring(as.character(df_all[,'timestamp_first_active']), 9, 10))
	)

df_all$timestamp_first_active = NULL

# clean Age by removing values
df_all$age1 = df_all$age
df_all$age1 = ifelse( is.na(df_all$age1) | df_all$age1 < 14 | df_all$age1 > 65, mean(df_all$age1[df_all$age1>=14 & df_all$age1 <= 65 & !is.na(df_all$age1)]), df_all$age1 )
df_all$age = NULL

df_all$first_affiliate_tracked = ifelse( df_all$first_affiliate_tracked == "", "-other-", df_all$first_affiliate_tracked)

# one-hot-encoding features
ohe_feats = c('gender', 'signup_method', 'signup_flow', 'language', 'affiliate_channel', 'affiliate_provider', 'first_affiliate_tracked', 'signup_app', 'first_device_type', 'first_browser')

dummies <- dummyVars(~ gender + signup_method + signup_flow + language + affiliate_channel + affiliate_provider + first_affiliate_tracked + signup_app + first_device_type + first_browser, data = df_all)

df_all_ohe <- as.data.frame(predict(dummies, newdata = df_all))
df_all_combined <- cbind(df_all[,-c(which(colnames(df_all) %in% ohe_feats))],df_all_ohe)

# split train and test
train = df_all_combined[df_all_combined$id %in% df_train$id,]
test = df_all_combined[!(df_all_combined$id %in% df_train$id),]
train$id = test$id = NULL

# train = cbind(train,labels)
# colnames(train)[ncol(train)] = "country_destination"

ycodes = data.frame(  code = c('NDF' ,'US' ,'other','FR' ,'CA' ,'GB' ,'ES' ,'IT' ,'PT' ,'NL' ,'DE' ,'AU')
					,values = c(0,1,2,3,4,5,6,7,8,9,10,11) )

ycodes$code = as.character(ycodes$code)
labels = as.data.frame(labels)
labels$id  <- 1:nrow(labels)
y1 = merge(x = labels, y = ycodes, by.x = "labels", by.y = "code")
y1 = y1[order(y1$id), ]
# y1$values = as.factor(y1$values)
# y <- recode(labels$country_destination,"'NDF'=0; 'US'=1; 'other'=2; 'FR'=3; 'CA'=4; 'GB'=5; 'ES'=6; 'IT'=7; 'PT'=8; 'NL'=9; 'DE'=10; 'AU'=11")

dtrain<-xgb.DMatrix(data=data.matrix(train),label=y1$values)

dt = format(Sys.time(), "%Y%m%d_%H%M")

clfs = list()
i = 0
for ( peta in c(0.1, 0.3, 0.5) )                 {
    for ( pcolsample_bytree in seq(0.5, 1, 0.1) )     {
        for ( psubsample in seq(0.5, 1, 0.1) )          {
            for ( pmin_child_weight in c(10,30) )          {
                i = i+1
                print(paste('i=',i,'eta=',peta,'  colsample_bytree=',pcolsample_bytree,'  subsample=',psubsample,'  min_child_weight=',pmin_child_weight))
                clf = xgb.cv(   
                    data 				= dtrain, 
                    nround				= 300, 
                    nthread 			= 7,
                    nfold 				= 5,
                    objective           = "multi:softprob", 
					num_class 			= 12,
					booster				= "gbtree",
                    eval_metric 		= "merror",
                    max_depth           = 8, #changed from default of 8
                    eta 				= peta,
                    colsample_bytree 	= pcolsample_bytree,
                    subsample			= psubsample,
                    min_child_weight	= pmin_child_weight,
                    verbose             = 1,  #1
                    print.every.n		= 10,
                    early.stop.round	= 30
                )
                clfs[[i]] = clf
            }
        }
    }
	save.image(file=paste0("abnb",dt,".RData"), compress=T)
}


# train xgboost
xgb <- xgboost(data = data.matrix(train),
               label = y1$values,
               eta = 0.1,
               max_depth = 20,
               nround=500,
               subsample = 0.75,
               colsample_bytree = 0.75,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softprob",
               num_class = 12,
               nthread = 7
)

               clf = xgb.cv(   
                    data 				= dtrain, 
                    nround				= 500, 
                    nthread 			= 7,
                    nfold 				= 5,
                    objective           = "multi:softprob", 
					num_class 			= 12,
					booster				= "gbtree",
                    eval_metric 		= "merror",
                    max_depth           = 20, #changed from default of 8
                    eta 				= .1,
                    colsample_bytree 	= .5,
                    subsample			= .5,
                    verbose             = 1,  #1
                    print.every.n		= 10
				)

# predict values in test set
y_pred <- predict(xgb, data.matrix(X_test[,-1]))

# extract the 5 classes with highest probabilities
predictions <- as.data.frame(matrix(y_pred, nrow=12))
rownames(predictions) <- c('NDF','US','other','FR','CA','GB','ES','IT','PT','NL','DE','AU')
predictions_top5 <- as.vector(apply(predictions, 2, function(x) names(sort(x)[12:8])))

# create submission
ids <- NULL
for (i in 1:NROW(X_test)) {
  idx <- X_test$id[i]
  ids <- append(ids, rep(idx,5))
}
submission <- NULL
submission$id <- ids
submission$country <- predictions_top5

# generate submission file
submission <- as.data.frame(submission)
write.csv(submission, "submission_v3.csv", quote=FALSE, row.names = FALSE)
