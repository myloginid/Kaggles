# for ( obj in ls() ) { if(class(get(obj)) == "H2OBinomialModel" ) { h2o.saveModel(object = get(obj) , path = paste0("/home/manishm/homes/", obj, ".rds" ), force = T)  } }

rm(list = ls())
# options(stringsAsFactors = FALSE) 
setwd("/home/manishm/homes")
rm(list = ls())
library(Metrics)
library(Hmisc)
library(xgboost)
library(checkmate)
library(mlr) 
library(dplyr)
library(parallelMap)

mit = 5
ccvv = socs = 5
set.seed(123)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train.bkp <- train
test.bkp <- test

train = train.bkp
test = test.bkp

colnames(train) = tolower(colnames(train))
colnames(test) = tolower(colnames(test))
train$src = "train"
test$src = "test"

target = "quoteconversion_flag"
ignoredcols = c("src","quotenumber",target)


prep_data	<- function( tmp, ignoredcols ) {
    # tmp$QuoteNumber  = NULL    

	xnames = colnames(tmp)[!colnames(tmp) %in% ignoredcols]
	tmp$original_quote_date = as.Date(tmp$original_quote_date,format = "%Y-%m-%d")
	tmp = mutate(tmp 
		,qcurrweek = as.integer(format(original_quote_date,'%w'))
		,qdayofmonth = as.integer(format(original_quote_date,'%d'))
		,qdayofyear = as.integer(format(original_quote_date,'%j'))
		,qmonth = as.integer(format(original_quote_date, "%m"))
		,qyear = as.integer(format(original_quote_date, "%Y"))
		)
	tmp$original_quote_date = NULL
	
	xnames = colnames(tmp)[!colnames(tmp) %in% ignoredcols]
	for (f in xnames) {
	  if (class(tmp[[f]])=="character") {
		print(paste0(f,' converted to factor'))
		levels <- unique(tmp[[f]])
		tmp[[f]] <- as.integer(factor(tmp[[f]], levels=levels))
	  }
	}
	xnames = colnames(tmp)[!colnames(tmp) %in% ignoredcols]
    for (f in xnames) {
        if ( is.integer(tmp[ !is.na(tmp[,f]) ,f] ) ) {
			# print(paste(f,' is converted to factor'))
			tmp[,f] = ifelse( is.na(tmp[,f]) , -1, tmp[,f] )
			# tmp[,f] = as.factor(tmp[,f])
        }
    }
    
	for (name in c('coveragefield','personalfield','salesfield','propertyfield','geographicfield'))
	{
		print(name)
		cols = grep(name, colnames(tmp), value = T)
		ints = sapply(X = alldata[,cols], FUN = is.integer)
		ints = names(ints[ints==T])
		# minus1s = rowSums(sapply(X = alldata[,ints], FUN = function(x) {ifelse(x==-1,1,0)} ))
		minus1s = rowSums(ifelse(tmp[,ints] == -1,1,0))
		tmp = cbind(tmp,minus1s)
		colnames(tmp)[ncol(tmp)] = paste0(name,'cntminus1s')
	}
	xnames = colnames(tmp)[!colnames(tmp) %in% ignoredcols]
	xnames = xnames[ ! xnames %in% paste0(c('coveragefield','personalfield','salesfield','propertyfield','geographicfield'),'cntminus1s')  ]	
    for (f in xnames) {
        if ( is.integer(tmp[ !is.na(tmp[,f]) ,f] ) ) {
			print(paste(f,' is converted to factor'))
			# tmp[,f] = ifelse( is.na(tmp[,f]) , -1, tmp[,f] )
			tmp[,f] = as.factor(tmp[,f])
        }
    }
    return(tmp)
}

train$quoteconversion_flag=NULL
y = train.bkp$QuoteConversion_Flag

alldata = rbind(train,test)
alldata = prep_data(alldata, ignoredcols)

train1 = alldata[ alldata$src == "train" ,]
test1 = alldata[ alldata$src == "test" ,]
target = "quoteconversion_flag"
features <- colnames(train1)[!(colnames(train1) %in% ignoredcols)]
train1 = train1[,c(features)]
test1 = test1[,c(features)]

# h = sample(nrow(train1), round(nrow(train1)/2))
h = sample(nrow(train1), round(nrow(train1)))
dtrain = xgb.DMatrix(data=data.matrix(train1[h,]),label=y[h])
peta= 0.1   
pcolsample_bytree= 0.6   
psubsample= 0.9   
pmin_child_weight= 10
# clf = xgb.cv(   
	# data 				= dtrain, 
	# nround				= 500, 
	# nthread 			= 7,
	# nfold 				= 5,
	# objective           = "binary:logistic", 
	# booster				= "gbtree",
	# eval_metric 		= "auc",
	# max_depth           = 8, #changed from default of 8
	# eta 				= peta,
	# colsample_bytree 	= pcolsample_bytree,
	# subsample			= psubsample,
	# min_child_weight	= pmin_child_weight,
	# verbose             = 1,  #1
	# print.every.n		= 10,
	# early.stop.round	= 50
# )

clf1 = xgb.cv(   
	data 				= dtrain, 
	nround				= 500, 
	nthread 			= 7,
	nfold 				= 5,
	objective           = "binary:logistic", 
	booster				= "gbtree",
	eval_metric 		= "auc",
	max_depth           = 8, #changed from default of 8
	eta 				= peta,
	colsample_bytree 	= pcolsample_bytree,
	subsample			= psubsample,
	min_child_weight	= 15,
	verbose             = 1,  #1
	print.every.n		= 10,
	early.stop.round	= 50
)

# clfs = list()
# i = 0
# # for ( peta in 2^-rev(seq(1:10)) )                 {
# # h = sample(nrow(train1), round(nrow(train1)/2))
# for ( peta in c(0.01, 0.1, 0.3, 0.5) )                 {
    # for ( pcolsample_bytree in seq(0.5, 1, 0.1) )     {
        # for ( psubsample in seq(0.5, 0.9, 0.1) )          {
            # for ( pmin_child_weight in c(10,30) )          {
                # i = i+1
                # print(paste('i=',i,'eta=',peta,'  colsample_bytree=',pcolsample_bytree,'  subsample=',psubsample,'  min_child_weight=',pmin_child_weight))
                # h = sample(nrow(train1), round(nrow(train1)/2))
                # dtrain<-xgb.DMatrix(data=data.matrix(train1[h,]),label=y[h])
                # clf = xgb.cv(   
                    # data 				= dtrain, 
                    # nround				= 50, 
                    # nthread 			= 7,
                    # nfold 				= 5,
                    # objective           = "binary:logistic", 
                    # booster				= "gbtree",
                    # eval_metric 		= "auc",
                    # max_depth           = 8, #changed from default of 8
                    # eta 				= peta,
                    # colsample_bytree 	= pcolsample_bytree,
                    # subsample			= psubsample,
                    # min_child_weight	= pmin_child_weight,
                    # verbose             = 1,  #1
                    # print.every.n		= 10,
                    # early.stop.round	= 50
                # )
                # clfs[[i]] = clf
            # }
        # }
    # }
# }


peta= 0.1   
pcolsample_bytree= 0.6   
psubsample= 0.9   
pmin_child_weight= 10
dtrain<-xgb.DMatrix(data=data.matrix(train1),label=y)

final = xgb.train(   
	data 				= dtrain, 
	nround				= 500,
	nthread 			= 7,
	objective           = "binary:logistic", 
	booster				= "gbtree",
	eval_metric 		= "auc",
	max_depth           = 12, #changed from default of 8
	eta 				= peta,
	colsample_bytree 	= pcolsample_bytree,
	subsample			= psubsample,
	min_child_weight	= pmin_child_weight,
	verbose             = 1,  #1
	print.every.n		= 10
)


# base = NULL; base = as.data.frame(base); for (i in 1:length(clfs)) { base = rbind(base,clfs[[i]][50])}
# base$id = seq(1:nrow(base))
# base = base[order(-base$test.auc.mean),]
# best = base[1,id]

# predtrain <- predict(final, data.matrix(train1))
# library(ROCR)

# predtrain1 = prediction(predictions = predtrain, labels = y)
# perftrain <- performance(predtrain1, measure = "tpr", x.measure = "fpr") 
# plot(perftrain, col=rainbow(10))

predtest <- predict(final, data.matrix(test1))
submission1 = as.data.frame(cbind(test.bkp$QuoteNumber, predtest))
colnames(submission1) = c("QuoteNumber","QuoteConversion_Flag")
write.table(x = submission1, file = "submission_ihopeitworked_v11.csv", sep = ",",quote = T, row.names = F, col.names = T)

save.image(file = "homesv9.image", compress = T)

# submission = mlr.xgboost(train1, test1)
# submission1 = as.data.frame(cbind(test.bkp$QuoteNumber, submission$quoteconversion_flag))
# colnames(submission1) = c("QuoteNumber","QuoteConversion_Flag")
# write.table(x = submission1, file = "submission_ihopeitworkd.csv", sep = ",",quote = T, row.names = F, col.names = T)



  