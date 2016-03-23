#Setup
rm(list = ls(all = TRUE)) #CLEAR WORKSPACE

#Directory

rm(list = ls())
options(stringsAsFactors = FALSE) 

setwd("/home/manishm/tel")
library(RODBC)
library(RCurl)
library(stringi)
library(stringr)
library(stringdist)
library(sqldf)
library(tm)
library(RTextTools)
library(e1071)
library(plyr)
library(dplyr)
lapply(c("ggplot2","dplyr","caret","doParallel","foreach","pROC","randomForest","ROCR","data.table","dplyr"),require,character.only=T)




severity_type  = read.csv("severity_type.csv" )
test           = read.csv("test.csv"          )
train          = read.csv("train.csv"         )
event_type     = read.csv("event_type.csv"    )
log_feature    = read.csv("log_feature.csv"   )
resource_type  = read.csv("resource_type.csv" )

colnames(severity_type) = tolower(colnames(severity_type))
colnames(test         ) = tolower(colnames(test         ))
colnames(train        ) = tolower(colnames(train        ))
colnames(event_type   ) = tolower(colnames(event_type   ))
colnames(log_feature  ) = tolower(colnames(log_feature  ))
colnames(resource_type) = tolower(colnames(resource_type))


meltedres = dcast(resource_type , id ~ resource_type )
meltedres = cbind( meltedres$id, sapply(X = meltedres[2:11], FUN = function(x) ifelse(is.na(x) , 0, 1) ) ) 
meltedres = as.data.frame(meltedres)
for (i in 2:ncol(meltedres)) { meltedres[,i] = as.factor(meltedres[,i]) }
colnames(meltedres)[1] = "id"
colnames(meltedres) = gsub(' ' , '', colnames(meltedres))

meltedlog = dcast(log_feature , id ~ log_feature )
meltedlog = cbind( meltedlog$id, sapply(X = meltedlog[2:length(meltedlog)], FUN = function(x) ifelse(is.na(x) , 0, x) ) ) 
meltedlog = as.data.frame(meltedlog)
for (i in 2:ncol(meltedlog)) { meltedlog[,i] = as.factor(meltedlog[,i]) }
colnames(meltedlog)[1] = "id"
colnames(meltedlog) = gsub(' ' , '', colnames(meltedlog))

meltedsev = dcast(severity_type , id ~ severity_type )
meltedsev = cbind( meltedsev$id, sapply(X = meltedsev[2:length(meltedsev)], FUN = function(x) ifelse(is.na(x) , 0, 1) ) ) 
meltedsev = as.data.frame(meltedsev)
for (i in 2:ncol(meltedsev)) { meltedsev[,i] = as.factor(meltedsev[,i]) }
colnames(meltedsev)[1] = "id"
colnames(meltedsev) = gsub(' ' , '', colnames(meltedsev))

meltedevent = dcast(event_type , id ~ event_type )
meltedevent = cbind( meltedevent$id, sapply(X = meltedevent[2:length(meltedevent)], FUN = function(x) ifelse(is.na(x) , 0, 1) ) ) 
meltedevent = as.data.frame(meltedevent)
for (i in 2:ncol(meltedevent)) { meltedevent[,i] = as.factor(meltedevent[,i]) }
colnames(meltedevent)[1] = "id"
colnames(meltedevent) = gsub(' ' , '', colnames(meltedevent))


meltedtrain = dcast(train , id ~ location,  value.var = "location" )
meltedtrain = cbind( meltedtrain$id, sapply(X = meltedtrain[2:length(meltedtrain)], FUN = function(x) ifelse(is.na(x) , 0, 1) ) ) 
meltedtrain = as.data.frame(meltedtrain)
for (i in 2:ncol(meltedtrain)) { meltedtrain[,i] = as.factor(meltedtrain[,i]) }
colnames(meltedtrain)[1] = "id"
colnames(meltedtrain) = gsub(' ' , '', colnames(meltedtrain))


train$location = NULL
finaltrain = merge( x = meltedtrain, y= train , by = "id" )
finaltrain$fault_severity = as.factor(finaltrain$fault_severity)
finaltrain = merge( x = finaltrain, y= meltedres , by = "id" )
finaltrain = merge( x = finaltrain, y= meltedlog , by = "id" )
finaltrain = merge( x = finaltrain, y= meltedevent , by = "id" )

library(h2o)
water = h2o.init(ip = "10.92.131.108", startH2O = F, nthreads = -1)
h2o.removeAll()
train.hex = as.h2o( finaltrain,  destination_frame = "train.hex")





#Load Required Packages
library('caTools')
library('caret')
library('glmnet')
library('ipred')
library('e1071')

colnames(train) = tolower(colnames(train))
colnames(test) = tolower(colnames(test))
train$src = "train"
test$src = "test"

target = "response"
ignoredcols = c("src",target,'id')
features <- colnames(train)[!(colnames(train) %in% ignoredcols)]

train[,target] = as.factor(train[,target])

flist ="product_info_1,product_info_2,product_info_3,product_info_5,product_info_6,product_info_7,employment_info_2,employment_info_3,employment_info_5,insuredinfo_1,insuredinfo_2,insuredinfo_3,insuredinfo_4,insuredinfo_5,insuredinfo_6,insuredinfo_7,insurance_history_1,insurance_history_2,insurance_history_3,insurance_history_4,insurance_history_7,insurance_history_8,insurance_history_9,family_hist_1,medical_history_2,medical_history_3,medical_history_4,medical_history_5,medical_history_6,medical_history_7,medical_history_8,medical_history_9,medical_history_11,medical_history_12,medical_history_13,medical_history_14,medical_history_16,medical_history_17,medical_history_18,medical_history_19,medical_history_20,medical_history_21,medical_history_22,medical_history_23,medical_history_25,medical_history_26,medical_history_27,medical_history_28,medical_history_29,medical_history_30,medical_history_31,medical_history_33,medical_history_34,medical_history_35,medical_history_36,medical_history_37,medical_history_38,medical_history_39,medical_history_40,medical_history_41,medical_history_1,medical_history_10,medical_history_15,medical_history_24,medical_history_32"
for (f in strsplit(flist,',')[[1]]) {
    print(f) 
    train[,f] = as.factor(train[,f])
    test[,f] = as.factor(test[,f])
}

clean.create.vars  <- function(tmp.x) {
    xnames = colnames(tmp.x)
    for (f in xnames) {
        if (any(is.na(tmp.x[,f])) ) {
                if ( is.factor(tmp.x[,f]) ) {
                    print(paste(f,' is a factor'))                
                    tmp.x[,f] = ifelse( is.na(tmp.x[,f]) , 'MISSING', tmp.x[,f])  
                    }
                else{
                    print(paste(f,' is numeric'))
                    # Set Null to High Value
                    colnbr = NCOL(tmp.x) + 1
                    # tmp.x[,colnbr] = ifelse( is.na(tmp.x[,f]) , abs(max(tmp.x[,f], na.rm = T)) * 100  ,  tmp.x[,f])  
                    print(paste('Std is',abs(sd(tmp.x[,f],na.rm = T)*5)))
                    tmp.x[,colnbr] = ifelse( is.na(tmp.x[,f]) , abs(sd(tmp.x[,f],na.rm = T)*5) ,  tmp.x[,f])  
                    colnames(tmp.x)[colnbr] = paste0("na.",f)             
                    # Create Indicator for Null
                    colnbr = NCOL(tmp.x) + 1
                    tmp.x[,colnbr] = ifelse( is.na(tmp.x[,f]) , 'T' ,  'F')
                    tmp.x[,colnbr] = as.factor(tmp.x[,colnbr])
                    colnames(tmp.x)[colnbr] = paste0("naf.",f)   
                    tmp.x[,f] = NULL
                }
             }
    }
tmp.x = mutate(tmp.x
	,bmi2 = bmi^2	,bmi3 = bmi^3	,bmi.log = log10(bmi)	,bmi.sq2 = sqrt(bmi)
	,ht2 = ht^2	,ht3 = ht^3	,ht.log = log10(ht)	,ht.sq2 = sqrt(ht)	,wt2 = wt^2
	,wt3 = wt^3	,wt.log = log10(wt)	,wt.sq2 = sqrt(wt)	,ins_age2 = ins_age^2
	,ins_age3 = ins_age^3	,ins_age.sq2 = sqrt(ins_age)	,ins_age.log = log10(ins_age)
	,bmi.wt = bmi*wt ,bmi.ht = bmi*ht	,wt.ht = wt*ht
	,bmi.wt.ht = bmi*wt*ht
	,bmi.wt.ht.ins_age = bmi*wt*ht*ins_age
	)	
	return(tmp.x)
}

train=train.bkp
test=test.bkp
train = clean.create.vars (train)
test = clean.create.vars (test)
features <- colnames(train)[!(colnames(train) %in% ignoredcols)]
train$response = as.factor(train$response)


# form <- as.formula(paste("response ~ ", paste(features,collapse="+")))
# mat2 = model.matrix(form,data=train)[,-1]
# hlmn = glmnet( x = mat2, y = train$response, family="multinomial", alpha=1 )

# mat2 = sparse.model.matrix(form,data=train)[,-1]
# mat3 = sparse.model.matrix(form,data=test)[,-1]


library(h2o)
water = h2o.init(ip = "10.92.131.110", startH2O = F, nthreads = -1)
h2o.removeAll()
train.hex = as.h2o(train[,c(features,"response")],  destination_frame = "train.hex")
test.hex = as.h2o(test,  destination_frame = "test.hex")

gbm.grid <- h2o.grid("gbm", x = features, y="response", training_frame = train.hex, hyper_params = list(ntrees = c(50,75,100), max_depth=c(5,10), min_rows=c(60,30) ), distribution = "AUTO", tweedie_power = 1.5, learn_rate = 0.1, sample_rate = 1.0, col_sample_rate = 1.0, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 5, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3)
summary(gbm.grid)
model_ids <- gbm.grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})
for ( i in 1:length(models)) { print(paste('MSE ', model_ids[[i]], ' = ', models[[i]]@model$cross_validation_metrics@metrics$MSE)) }
for ( i in 1:length(models)) { print(paste('r2 ', model_ids[[i]], ' = ', models[[i]]@model$cross_validation_metrics@metrics$r2)) }
pred = h2o.predict(object = models[[11]] , newdata = test.hex )
pred = as.data.frame(pred)
write.table(x = cbind(test$id, pred$predict), file = "grid_gbm.csv", sep = ",", row.names = F, col.names = T)


gbm.grid <- h2o.grid("gbm", x = features, y="response", training_frame = train.hex, hyper_params = list(ntrees = c(100,150,200), max_depth=c(10,20), min_rows=c(30) ), distribution = "AUTO", tweedie_power = 1.5, learn_rate = 0.05, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 3, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3)

gbm.200.20.30 = h2o.gbm(x = features, y= target ,training_frame = "train.hex", model_id = "gbm.200.20.30", validation_frame = NULL, distribution = "AUTO", tweedie_power = 1.5, ntrees = 200, max_depth = 20, min_rows = 30, learn_rate = 0.05, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3, offset_column = NULL, weights_column = NULL )


singlecolmutation = function (tmp, vs) {
	for (v in vs ) {
		print(v)
		if ( !is.factor(tmp[,v]) ) { 
		colnbr = NCOL(tmp) + 1
		tmp[,colnbr]  = ifelse( is.na(tmp[,v]) , -999, log(abs(tmp[,v])))
		colnames(tmp)[colnbr] = paste0("log_",v)
		colnbr = NCOL(tmp) + 1
		tmp[,colnbr]  = ifelse( is.na(tmp[,v]) , -999, sqrt(tmp[,v] * tmp[,v]))
		colnames(tmp)[colnbr] = paste0("sqrt_",v)   
		colnbr = NCOL(tmp) + 1
		tmp[,colnbr]  = ifelse( is.na(tmp[,v]) , -999, tmp[,v]^2 )
		colnames(tmp)[colnbr] = paste0("exp2_",v)                   
		colnbr = NCOL(tmp) + 1
		tmp[,colnbr]  = ifelse( is.na(tmp[,v]) , -999, tmp[,v]^3 )
		colnames(tmp)[colnbr] = paste0("exp3_",v)                   
		colnbr = NCOL(tmp) + 1
		tmp[,colnbr]  = ifelse( is.na(tmp[,v]) , -999, exp(tmp[,v] * tmp[,v]))
		colnames(tmp)[colnbr] = paste0("exp_",v)                   
		return(tmp)
	}}
}
	
doublecolmutation = function (tmp, vs) {
	for ( v1 in vs) {
		for ( v2 in vs) {
			if ( !is.factor(tmp[,v1]) & !is.factor(tmp[,v2]) & v1!=v2  ) {	
				print(v1) ; print(v2);
				colnbr = NCOL(tmp) + 1
				tmp[,colnbr]  = ifelse( is.na(tmp[,v1]) || is.na(tmp[,v2]) , -999,  tmp[,v1] * tmp[,v2] )
				colnames(tmp)[colnbr] = paste0(v1,"X",v2)
				colnbr = NCOL(tmp) + 1
				tmp[,colnbr]  = ifelse( (is.na(tmp[,v1]) || is.na(tmp[,v2])) & is.na(tmp[,v2] != 0 ) , -999,  tmp[,v1] / tmp[,v2] )
				colnames(tmp)[colnbr] = paste0(v1,".by.",v2)
				}
			}
		}
	return(tmp)
}


varimp = h2o.varimp(object = gbm.200.20.30)
varimp = cbind(varimp, cumsum(varimp$percentage))
colnames(varimp)[5] = "cper"
selvar = varimp[varimp$cper <= 0.75 ,1 ]
train1 = train[,c(selvar,target)]
test1 = test[,selvar]
train1 = singlecolmutation(train1, selvar)
test1 = singlecolmutation(test1, selvar)
train1 = doublecolmutation(train1, selvar)
test1 = doublecolmutation(test1, selvar)

features <- colnames(train1)[!(colnames(train1) %in% ignoredcols)]
train1.hex = as.h2o(train1[,c(features,"response")],  destination_frame = "train1.hex")
test1.hex = as.h2o(test1,  destination_frame = "test1.hex")

gbm1.grid <- h2o.grid("gbm", x = features, y="response", training_frame = train1.hex, hyper_params = list(ntrees = c(100,150,200), max_depth=c(10,20), min_rows=c(30) ), distribution = "AUTO", tweedie_power = 1.5, learn_rate = 0.05, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 3, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3)


train1_gbm.200.20.30 = h2o.gbm(x = features, y= target ,training_frame = "train1.hex", model_id = "train1_gbm.200.20.30", validation_frame = NULL, distribution = "AUTO", tweedie_power = 1.5, ntrees = 200, max_depth = 20, min_rows = 30, learn_rate = 0.05, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3, offset_column = NULL, weights_column = NULL )




selvars = varimp[ ! varimp$variable %in% colnames(train.bkp), ]

gbm.pred = as.data.frame(h2o.predict(gbm.200.20.30 , test.hex))
write.table(x = cbind(test$id, gbm.pred$predict), file = "grid_gbm.200.20.30.csv", sep = ",", row.names = F, col.names = T)

train.hex = as.h2o(train[,c(features,"response")],  destination_frame = "train.hex")
test.hex = as.h2o(test,  destination_frame = "test.hex")


clus.train = h2o.kmeans(training_frame = "train.hex",  k = 8, model_id = "clus.train", max_iterations = 1000, standardize = TRUE, init = "Furthest", 
# init = c("Furthest", "Random", "PlusPlus"), 
nfolds = 10, fold_column = NULL, fold_assignment = "AUTO", keep_cross_validation_predictions = FALSE)


trainc = as.data.frame(h2o.predict(clus.train, train.hex))
colnames(trainc)[1] = "clusnum"
trainf = cbind(train, trainc)
trainf$response = as.factor(trainf$response)
splitt = split(trainf, f = trainf$clusnum)

testc = as.data.frame(h2o.predict(clus.train, test.hex))
colnames(testc)[1] = "clusnum"
testf = cbind(test, testc)
split.test = split(testf, f = testf$clusnum)
ignoredcols = c(ignoredcols,"clusnum")
features <- colnames(trainf)[!(colnames(trainf) %in% ignoredcols)]

cmodels = list()
cpred = list()
for ( i in 1:length(splitt) ) { 
	# hist(splitt[[i]]$response)$counts
	train.wip = as.h2o(  splitt[[i]][,c(features,"response")],  destination_frame = paste0("train",i,".wip"))
	test.wip = as.h2o( split.test[[i]][,c(features)],  destination_frame = paste0("test",i,".wip"))
	cmodels[[i]] = h2o.gbm(x = features, y=target ,training_frame = train.wip , model_id = paste0("gbm.200.20.30_",i) , validation_frame = NULL, distribution = "AUTO", tweedie_power = 1.5, ntrees = 200, max_depth = 20, min_rows = 30, learn_rate = 0.05, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3, offset_column = NULL, weights_column = NULL )
	cpred[[i]] = as.data.frame(h2o.predict(cmodels[[i]]  , test.wip))
	cpred[[i]] = cbind( split.test[[i]]$id, cpred[[i]] )
	
}
out = NULL
for (i in 1:length(cpred)) { out = rbind(out,cpred[[i]]) }
out1 = out[,1:2]
colnames(out1) = c("Id","Response")
	




summary(gbm.grid)
model_ids <- gbm.grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})
for ( i in 1:length(models)) { print(paste('MSE ', model_ids[[i]], ' = ', models[[i]]@model$cross_validation_metrics@metrics$MSE)) }
for ( i in 1:length(models)) { print(paste('r2 ', model_ids[[i]], ' = ', models[[i]]@model$cross_validation_metrics@metrics$r2)) }
pred = h2o.predict(object = models[[11]] , newdata = test.hex )
pred = as.data.frame(pred)
write.table(x = cbind(test$id, pred$predict), file = "grid_gbm.csv", sep = ",", row.names = F, col.names = T)



gbm.grid <- h2o.grid("gbm", x = features, y="response", training_frame = train.hex, hyper_params = list(ntrees = c(100,150,200), max_depth=c(10,20), min_rows=c(30) ), distribution = "AUTO", tweedie_power = 1.5, learn_rate = 0.05, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 3, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3)


rf.grid <- h2o.grid("randomForest", x = features, y="response", training_frame = train.hex, hyper_params = list(ntrees = c(50,75,100), max_depth=c(5,10), min_rows=c(60,30) ), nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 5, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3)
summary(rf.grid)
model_ids <- rf.grid@model_ids
models <- lapply(model_ids, function(id) { h2o.getModel(id)})
for ( i in 1:length(models)) { print(paste('MSE', model_ids[[i]], '=', models[[i]]@model$cross_validation_metrics@metrics$MSE)) }
for ( i in 1:length(models)) { print(paste('R2', model_ids[[i]], '=', models[[i]]@model$cross_validation_metrics@metrics$r2)) }
pred.rf = h2o.predict(object = models[[11]] , newdata = test.hex )
pred.rf = as.data.frame(pred)
write.table(x = cbind(test$id, pred$predict), file = "grid_rf.csv", sep = ",", row.names = F, col.names = T)


deep.grid <- h2o.grid("deeplearning", x = features, y=target, training_frame = train.hex, 
hyper_params = list(hidden = c( c(64),c(128),c(256),c(512),c(1024),c(64,64),c(128,128),c(256,256),c(512,512),c(1024,1024),c(64,64,64),c(128,128,128),c(256,256,256),c(512,512,512),c(1024,1024,1024) ), epochs=c(50,100), activation = c("Rectifier", "Tanh", "TanhWithDropout","RectifierWithDropout", "Maxout", "MaxoutWithDropout")) 
,overwrite_with_best_model = TRUE     # ,n_folds = 5
,autoencoder = FALSE                  ,l1 = 0
,use_all_factor_levels = TRUE         ,l2 = 0
,train_samples_per_iteration = -2     ,max_w2 = Inf
,adaptive_rate = TRUE                 ,initial_weight_distribution = c("UniformAdaptive")
,rho = 0.99                           ,initial_weight_scale = 1
,epsilon = 1e-08                      ,loss = c("Automatic")
,rate = 0.005                         ,score_interval = 5
,rate_annealing = 1e-06               ,score_training_samples = 0
,rate_decay = 1                       ,score_validation_samples = 0
,momentum_start = 0                   ,score_duty_cycle = 0.01
,momentum_ramp = 1e+06                ,classification_stop = 0
,momentum_stable = 0                  ,quiet_mode = FALSE
,nesterov_accelerated_gradient = TRUE ,diagnostics = TRUE
,input_dropout_ratio = 0              ,variable_importances = TRUE
,shuffle_training_data = TRUE         ,fast_mode = FALSE
,sparse  = TRUE                       ,ignore_const_cols = TRUE
,reproducible = FALSE                 ,export_weights_and_biases = TRUE
)

predict(cvfit, newx = x[1:5,], s = "lambda.min")

ll = as.character(h2o.ls()[,1])
ll = grep(pattern = "hex$|modelmetrics|cv",x = ll ,value = T, invert = T)

for ( i in ll) 

library(doParallel)
cl <- makeCluster(min(detectCores(),16), type='PSOCK' , timeout = 60*60*60*60 )
registerDoParallel(cl)
    
# svm
tc.svm = trainControl(method = "cv", number = 3, verboseIter = TRUE,returnData = TRUE,returnResamp = "final", savePredictions ="final" , p = 0.75, selectionFunction = "oneSE", allowParallel = TRUE )    
grid.svm <- expand.grid(.C = 2^(-2:2))     
model.svm = train( y~., data=data.frame(x = tmp.x, y = tmp.y) , method = "svmLinear", metric = "misclassification", trControl = tc.svm,tuneGrid = grid.svm)    
    

clf <- xgboost(data = data.matrix(train[,features]), missing = NaN, label = train$response, nrounds  = 100,objective  = "reg:linear",eval_metric = "rmse")

cat("training a XGBoost classifier\n")
clf <- xgboost(data        = data.matrix(t1[,features]),
               label       = train$Response,
               nrounds     = 100,
               objective   = "reg:linear",
               eval_metric = "rmse")

library(sqldf)



gbm.grid <- h2o.grid("gbm", x = features, y="response", training_frame = train.hex, hyper_params = list(ntrees = c(50,75,100), max_depth=c(5,10), min_rows=c(60,30) ), distribution = "AUTO", tweedie_power = 1.5, learn_rate = 0.1, sample_rate = 1.0, col_sample_rate = 1.0, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 5, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="misclassification", stopping_tolerance=1e-3)



				 
gbm = h2o.gbm(x = features, y= target ,training_frame = "train.hex", model_id = "gbm", validation_frame = NULL, distribution = "AUTO", tweedie_power = 1.5, ntrees = 50, max_depth = 5, min_rows = 10, learn_rate = 0.1, sample_rate = 1.0, col_sample_rate = 1.0, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 10, fold_column = NULL, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="AUC", stopping_tolerance=1e-3, offset_column = NULL, weights_column = NULL )





hrf = h2o.randomForest(x = features, y= target ,training_frame = "train.hex", model_id = "hrf",validation_frame = NULL,  mtries = -1,  sample_rate = 0.632,  build_tree_one_node = FALSE,  ntrees = 50,  max_depth = 20,  min_rows = 10,  nbins = 20,  nbins_cats = 1024,  binomial_double_trees = T,  balance_classes = FALSE,  max_after_balance_size = 5,  offset_column = NULL,  weights_column = NULL,  nfolds = 10,  fold_column = NULL,  fold_assignment = "Random",  keep_cross_validation_predictions = FALSE,  score_each_iteration = FALSE,  stopping_rounds=0,  stopping_metric="AUC",  stopping_tolerance=1e-3)

# distribution = c("AUTO","gaussian", "bernoulli", "multinomial", "poisson", "gamma", "tweedie")
# stopping_metric=c("AUTO", "deviance", "logloss", "MSE", "AUC", "r2", "misclassification")
gbm = h2o.gbm(x = features, y= target ,training_frame = "train.hex", model_id = "gbm", validation_frame = NULL, distribution = "AUTO", tweedie_power = 1.5, ntrees = 50, max_depth = 5, min_rows = 10, learn_rate = 0.1, sample_rate = 1.0, col_sample_rate = 1.0, nbins = 50, nbins_cats = 1024, balance_classes = FALSE, max_after_balance_size = 1, build_tree_one_node = FALSE, nfolds = 10, fold_column = NULL, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, score_each_iteration = FALSE, stopping_rounds=0, stopping_metric="AUC", stopping_tolerance=1e-3, offset_column = NULL, weights_column = NULL )

# activation = c("Rectifier", "Tanh", "TanhWithDropout", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
# initial_weight_distribution = c("UniformAdaptive", "Uniform", "Normal")
# loss = c("Automatic", "CrossEntropy", "Quadratic", "Absolute", "Huber")
# distribution = c("AUTO","gaussian", "bernoulli", "multinomial", "poisson", "gamma", "tweedie", "laplace", "huber")
# stopping_metric=c("AUTO", "deviance", "logloss", "MSE", "AUC", "r2", "misclassification")

deep = h2o.deeplearning(x = features, y= target ,training_frame = "train.hex", model_id = "deep", validation_frame = NULL, autoencoder = FALSE, use_all_factor_levels = TRUE, activation = "Rectifier", hidden= c(200, 200), epochs = 10, train_samples_per_iteration = -2, target_ratio_comm_to_comp = 0.05, adaptive_rate = TRUE, rho = 0.99, epsilon = 1e-8, rate = 0.005, rate_annealing = 1e-6, rate_decay = 1.0, momentum_start = 0, momentum_ramp = 1e6, momentum_stable = 0, nesterov_accelerated_gradient = TRUE, input_dropout_ratio = 0, l1 = 0, l2 = 0, max_w2 = Inf, initial_weight_distribution = "UniformAdaptive", initial_weight_scale = 1, loss = "Automatic", distribution = "AUTO", tweedie_power = 1.5, score_interval = 5, stopping_rounds=5, stopping_metric="misclassification", stopping_tolerance=0, balance_classes = FALSE, variable_importances = T, ignore_const_cols = T, shuffle_training_data = T, reproducible=FALSE, export_weights_and_biases=FALSE, offset_column = NULL, weights_column = NULL, nfolds = 10, fold_column = NULL, fold_assignment ="Random", keep_cross_validation_predictions = FALSE)

# alpha <- c(0.95, 0.97, 0.98, 0.99, 1)
# vars  <- c(25, 100, 150, 200, 250, 300, 350, 400)
# models <- list()

# for (i in 1:length(alpha)) {
  # models <- c(models, lapply(vars, function(x) {
    # h2o.glm(y = 'target', x = incl_vars,
            # training_frame = def.hex, validation_frame = def_test.hex, 
            # lambda_search = TRUE, nlambdas = 100, max_active_predictors = x, alpha = alpha[i],
            # family = 'binomial', link = 'logit', solver = 'IRLSM', standardize = FALSE, 
            # model_id = paste0('p_', x, 'a_', alpha[i]))
  # }))
# }

# solver = c("IRLSM", "L_BFGS")
# family = c("gaussian", "binomial", "poisson", "gamma", "tweedie")
# link = c("family_default", "identity", "logit", "log", "inverse", "tweedie")
hglm = h2o.glm(x = features, y= target ,training_frame = "train.hex", model_id = "hglm",  validation_frame = NULL, max_iterations = 50, beta_epsilon = 0, solver = "IRLSM", standardize = TRUE, family = "binomial",link = "logit", alpha = 0.5, prior = 0.0, lambda = 1e-05, lambda_search = F, nlambdas = -1, lambda_min_ratio = -1.0, nfolds = 5, fold_column = NULL, fold_assignment = "Random", keep_cross_validation_predictions = FALSE, beta_constraints = NULL, offset_column = NULL, weights_column = NULL, intercept = FALSE, max_active_predictors = -1) 


# nb = h2o.naiveBayes(x = features, y= "QuoteConversion_Flag" ,training_frame = "train.hex", model_id = "nb", laplace = 0, threshold = 0.001,eps = 0, compute_metrics = TRUE)


# hrf.pred = h2o.predict(hrf, train.hex)
# gbm.pred = h2o.predict(gbm, train.hex)
# deep.pred = h2o.predict(deep, train.hex)    
# hglm.pred = h2o.predict(hglm, train.hex) 

hrf.pred = h2o.predict(hrf, test.hex)
gbm.pred = h2o.predict(gbm, test.hex)
deep.pred = h2o.predict(deep, test.hex)    
hglm.pred = h2o.predict(hglm, test.hex) 




## Train a random forest using all default parameters
rfHex <- h2o.randomForest(x=features,
                          y="logSales", 
                          ntrees = 100,
                          max_depth = 30,
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame=trainHex)


closeAllConnections()


#Choose Target
train$quoteconversion_flag <- as.factor(ifelse(train$quoteconversion_flag == 1,'X1','X0'))

####################################
# RFE parameters
####################################
library(ipred)
library(e1071)

#Custom Functions
glmnetFuncs <- caretFuncs #Default caret functions

glmnetFuncs$summary <-  twoClassSummary

glmnetFuncs$rank <- function (object, x, y) {
	vimp <- sort(object$finalModel$beta[, 1])
	vimp <- as.data.frame(vimp)
	vimp$var <- row.names(vimp)
	vimp$'Overall' <- seq(nrow(vimp),1)
	vimp
}

MyRFEcontrol <- rfeControl(
		functions = glmnetFuncs,
		method = "boot",
		number = 25,
		rerank = FALSE,
		returnResamp = "final",
		saveDetails = FALSE,
		verbose = TRUE)
        
MyTrainControl=trainControl(
		method = "boot",
		number=25,
		returnResamp = "all",
		classProbs = TRUE,
		summaryFunction=twoClassSummary
		)


if ( require("multicore", quietly = TRUE, warn.conflicts = FALSE) ) {
	MyRFEcontrol$workers <- multicore:::detectCores()
	MyRFEcontrol$computeFunction <- mclapply
	MyRFEcontrol$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)

	MyTrainControl$workers <- multicore:::detectCores()
	MyTrainControl$computeFunction <- mclapply
	MyTrainControl$computeArgs <- list(mc.preschedule = FALSE, mc.set.seed = FALSE)
}


x <- train[,features]
y <- train$quoteconversion_flag

RFE <- rfe(x,y,sizes = seq(50,200,by=10),
		metric = "ROC",maximize=TRUE,rfeControl = MyRFEcontrol,
			method='glmnet',
			tuneGrid = expand.grid(.alpha=0,.lambda=c(0.01,0.02)),
			trControl = MyTrainControl)

NewVars <- RFE$optVariables
RFE
plot(RFE)

FL <- as.formula(paste("Target ~ ", paste(NewVars, collapse= "+"))) #RFE


####################################
# Fit a GLMNET Model
####################################

model <- train(FL,data=trainset,method='glmnet',
	metric = "ROC",
	tuneGrid = expand.grid(.alpha=c(0,1),.lambda=seq(0,.25,by=0.005)),
	trControl=MyTrainControl)
model
plot(model, metric='ROC')
test <- predict(model, newdata=testset, type  = "prob")
colAUC(test, testset$Target)

predictions <- test

########################################
#Generate a file for submission
########################################
testID  <- testset$case_id
submit_file = data.frame('Zach'=predictions[,1])
write.csv(submit_file, file="AUC_ZACH.txt", row.names = FALSE)


        
		
bmi	39761.1250	1.0	0.3246
medical_history_2	20912.8184	0.5260	0.1707
medical_history_4	12124.0938	0.3049	0.0990
medical_history_23	9793.9424	0.2463	0.0800
product_info_4	9442.7998	0.2375	0.0771
medical_keyword_3	5108.7188	0.1285	0.0417
medical_keyword_15	3153.5825	0.0793	0.0257
product_info_2	2939.8271	0.0739	0.0240
ins_age	2895.9268	0.0728	0.0236
wt	2712.4465	0.0682	0.0221