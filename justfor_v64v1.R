
sink("justfor_v64v1.log",split = T)
outfile = "justfor_v64v1.csv"

rm(list = ls(all = TRUE)) #CLEAR WORKSPACE

library(data.table) #Faster reading
library(xgboost)
library(Matrix)
library(ggplot2)

setwd("/home/manishm/bnp")

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),ncol = cols, nrow = ceiling(numPlots/cols))
  }
 if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,layout.pos.col = matchidx$col))
    }
  }
}

is.int = function(x) { return ( is.integer(x) & !is.numeric(x) ) }
getfacts = function(df) { facts = sapply(X = df, FUN = is.factor) ; names(facts[facts==T]) }
getints = function(df) { ints = sapply(X = df, FUN = is.int) ; names(ints[ints==T]) }
getchars = function(df) { chars = sapply(X = df, FUN = is.character) ; names(chars[chars==T]) }
getnbrs = function(df) { nbrs = sapply(X = df, FUN = is.numeric) ; names(nbrs[nbrs==T]) }
getnas = function(df) { nas = sapply(X = df, FUN = function(x) {any(is.na(x))}) ; names(nas[nas==T]) }

isna = function(x, value = T) { if (value==T) return(x[!is.na(x)]) else return(!is.na(x)) }

normalize = function(x) { if ( min(x) == max(x) &  max(x) == 0 ) {return(x) } ;   x = (x-min(x))/(max(x)-min(x)) ; return(x) }
cats = function(...) { cat(...); cat("\n") }
prints = function(...) { print(paste(...)) }

na.roughfix2 <- function (object, ...) {
    res <- lapply(object, roughfix)
    structure(res, class = "data.frame", row.names = seq_len(nrow(object)))
}

roughfix <- function(x) {
    missing <- is.na(x)
    if (!any(missing)) return(x)
    
    if (is.numeric(x)) {
        x[missing] <- median.default(x[!missing])
        # x[missing] <- -999
    } else if ( is.factor(x) | is.character(x) ) {
        freq <- table(x)
        x[missing] <- names(freq)[which.max(freq)]
        # x[missing] <- 'MISSING'
    } else {
        stop("na.roughfix only works for numeric or factor")
    }
    x
}



# Start the clock!
start_time <- Sys.time()


# Set a seed for reproducibility
set.seed(2016)

cats("reading the train and test data")
# Read train and test
train_raw <- fread("train.csv", stringsAsFactors=TRUE) 
print(dim(train_raw))
print(sapply(train_raw, class))

y <- as.factor(train_raw$target)
train_raw$target <- NULL
train_raw$ID <- NULL
n <- nrow(train_raw)

test_raw <- fread("test.csv", stringsAsFactors=TRUE) 
test_id <- test_raw$ID
test_raw$ID <- NULL
print(dim(test_raw))
print(sapply(test_raw, class))
cats("Data read ")
print(difftime( Sys.time(), start_time, units = 'sec'))

submission <- read.csv("sample_submission.csv")


# Preprocess data
# Find factor variables and translate to numeric
cats("Preprocess data")
all_data <- rbind(train_raw,test_raw)
all_data <- as.data.frame(all_data) # Convert data table to data frame


processdata = T
plots = F
# process my data
feature.names <- names(all_data)
factnacounts = rep(0,nrow(all_data))
nbrnacounts = rep(0,nrow(all_data))

for (f in feature.names) {
  cats("-----------------------------------------------------")
  cats("Variable ",f , " is a ",class(all_data[[f]]))
	if (  ( class(all_data[[f]]) =="character" | class(all_data[[f]]) =="factor" )) {
		cats("Fixing spaces in",f)
		all_data[[f]][ all_data[[f]]==""] = NA
		all_data[[f]] = as.factor(as.character(all_data[[f]]))		
		cats("Levels of ",f , " is a ",sort(levels(all_data[[f]])))
		cats("Unique of ",f , " is a ",length(levels(all_data[[f]])))
		cats("Count of NA in ",f," is ",sum(is.na(table(all_data[,f])))," ")
		if ( length(levels(all_data[[f]])) < 100 ) {
			# print(table(all_data[,f]))
			# print(table(all_data[1:n,f],y))
			if(plots == T) {
				# par(mfrow=c(1,2)) 
				# plot(all_data[,f], xlab = f)
				# plot(table(all_data[1:n,f],y), xlab = f, col = c("red","green"))
			}
		}
		else { cats( "WARNING : TOO MANY LEVELS", "") } 

		# standard processing of character data
		if ( processdata == T) {
			cats("Processing ",f)
			if (any(is.na(all_data[[f]]))) { 
				factnacounts = factnacounts + as.integer(is.na(all_data[[f]]))
				all_data[ncol(all_data)+1] = as.integer(is.na(all_data[[f]]))
				names(all_data)[ncol(all_data)] = paste0('factisna.',f)
				# impute nulls
				all_data[[f]] = as.factor(roughfix(as.character(all_data[[f]])))
			}
		}		
	}
	else if (  is.integer(all_data[[f]]) & unique(all_data[[f]]) < 100  ) {
		cats("Count of NA in ",f," is ",sum(is.na(all_data[,f]))," ")
		cats("Unique ", f, " is ",sort(unique(all_data[,f]))," ")
		cats("Unique of ",f , " is a ",length(unique(all_data[[f]])))		
		if ( length(levels(all_data[[f]])) < 100 ) {
			# print(table(all_data[,f]))
			# print(table(all_data[1:n,f],y))
			if(plots == T) {
				# par(mfrow=c(1,2)) 
				# hist(all_data[,f], xlab = f)
				# plot(table(all_data[1:n,f],y), xlab = f, col = c("red","green"))
			}
		}
		else { cats( "WARNING : TOO MANY LEVELS", "") } 

		# standard processing of integer factor data
		if ( processdata == T) {
			cats("Processing ",f)
			cats("Convert to factor", f)
			all_data[[f]] = as.factor(all_data[[f]])		
			if (any(is.na(all_data[[f]]))) { 
				factnacounts = factnacounts + as.integer(is.na(all_data[[f]]))
				all_data[ncol(all_data)+1] = as.integer(is.na(all_data[[f]])) 
				names(all_data)[ncol(all_data)] = paste0('factisna.',f)				
				# impute nulls
				all_data[[f]] = as.factor(roughfix(as.character(all_data[[f]])))
			}
		}
	}
	else if ( is.numeric(all_data[[f]]) |  (  is.integer(all_data[[f]])  &  unique(all_data[[f]]) > 100 ) ) {
		cats("Count of NA in ",f," is ",sum(is.na(all_data[,f]))," ")
		# print(summary(all_data[,f]))		
		tmpdf = na.omit(data.frame(x = all_data[1:n,f], y = y))
		if(plots == T) {
			# p1 = ggplot(tmpdf, aes(x)) + geom_histogram(col = "black", fill="green")
			# p2 = ggplot(tmpdf, aes(y,x, fill=y)) + geom_boxplot()
			# p3 = ggplot(tmpdf, aes(x, fill=y), binwidth = 0.5) + geom_histogram(col = "black")
			# multiplot(p1, p2, p3, cols=2)
			# multiplot(p2, p3, cols=2)
			# par(mfrow=c(3,1)) 
			# hist(all_data[,f], xlab = f, breaks=50)
			# boxplot( all_data[1:n,f] ~ y, varwidth=TRUE, range = 0, notch = T , col = c("red","green"), horizontal = F, na.action = na.omit)	
		}
		# standard processing of numeric data
		if ( processdata == T) {
			cats("Processing ",f)
			cats("Scaled First.. Then Impute ",f)
			all_data[[f]] = scale( all_data[[f]], T, T)
			if (any(is.na(all_data[[f]]))) { 
				nbrnacounts = nbrnacounts + as.integer(is.na(all_data[[f]]))
				all_data[ncol(all_data)+1] = as.integer(is.na(all_data[[f]])) 
				names(all_data)[ncol(all_data)] = paste0('nbrisna.',f)				
				# impute nulls
				all_data[[f]] = roughfix(all_data[[f]])
			}
		}
	}
}
gc(FALSE)
all_data$nbrnacounts = nbrnacounts
all_data$factnacounts = factnacounts

feats = names(all_data)[ !names(all_data) %in% c('v22','v8','v23','v25','v36','v37','v46','v51','v53','v54','v63','v73','v75','v79','v81','v82','v89','v92','v95','v105','v107','v108','v109','v110','v116','v117','v118','v119','v123','v124','v128')]

feats

train <- all_data[1:n,feats]
test <- all_data[(n+1):nrow(all_data),feats] 

mtrain = model.matrix( ~ . -1, data = train, sparse = T)
mtest = model.matrix( ~ . -1, data = test, sparse = T)

xgtrain = xgb.DMatrix(mtrain, label = as.integer(y)-1)
xgtest = xgb.DMatrix(mtest)


iter = 3000
clfs = list()
i = 0
for ( peta in c(0.05) )                 {
    for ( pcolsample_bytree in seq(0.5, 1, 0.1) )     {
        for ( psubsample in seq(0.5, 0.9, 0.1) )          {
            for ( pmin_child_weight in c(1,5,10) )          {
      				for ( pmax_depth in c(8,10,15) )          {
                cats("-----------------------------------------------------")
      				  i = i+1
                print(paste('i=',i,'eta=',peta,'  colsample_bytree=',pcolsample_bytree,'  subsample=',psubsample,'  min_child_weight=', pmin_child_weight, 'max_depth=', pmax_depth ))
                
                model_cv = xgb.cv(data = xgtrain,  nround = iter,  nthread = 12, nfold = 5, objective = "binary:logistic", eval_metric = "logloss", max_depth = pmax_depth, eta = peta, colsample_bytree = pcolsample_bytree, subsample = psubsample, min_child_weight = pmin_child_weight, verbose = 1, print.every.n = round(iter/5), early.stop.round = 20 )
                best <- min(model_cv$test.logloss.mean) 
                bestIter <- which(model_cv$test.logloss.mean==best)[1]    
                cats("\n i = ", i, "|test.logloss.mean = ", best, "|bestIter = ", bestIter)
                print(model_cv[bestIter])
                model_cv$i = i
                clfs[[i]] = model_cv
				     }
            }
        }
    }
}


# [1] "i= 119 eta= 0.05   colsample_bytree= 0.7   subsample= 0.8   min_child_weight= 1 max_depth= 10"
peta= 0.005 ;  pcolsample_bytree= 0.7 ;  psubsample= 0.8 ;  pmin_child_weight= 1 ; pmax_depth= 10;

print(paste('i=',i,'eta=',peta,'  colsample_bytree=',pcolsample_bytree,'  subsample=',psubsample,'  min_child_weight=', pmin_child_weight, 'max_depth=', pmax_depth ))
model_cv = xgb.cv(data = xgtrain,  nround = iter,  nthread = 12, nfold = 5, objective = "binary:logistic", eval_metric = "logloss", max_depth = pmax_depth, eta = peta, colsample_bytree = pcolsample_bytree, subsample = psubsample, min_child_weight = pmin_child_weight, verbose = 1, print.every.n = round(iter/5), early.stop.round = 20 )
best <- min(model_cv$test.logloss.mean) 
bestIter <- which(model_cv$test.logloss.mean==best)[1]    
cats("\n i = ", i, "|test.logloss.mean = ", best, "|bestIter = ", bestIter)
print(model_cv[bestIter])

bestIter = bestIter*(1+1/5)
	# Bagging of single xgboost for ensembling
	# change to e.g. 1:10 to get quite good results
ensemble <- rep(0, nrow(test))
for (i in 1:19) {
	cats("Making predictions", i)
	set.seed(i + 2015)
		xgmod = xgb.train( data = xgtrain,  nround = bestIter,  nthread = 12, objective = "binary:logistic", eval_metric = "logloss", max_depth = pmax_depth, eta = peta, colsample_bytree = pcolsample_bytree, subsample = psubsample, min_child_weight = pmin_child_weight, verbose = 1, print.every.n = round(iter/5))
	# if ( i==1) {
		# varimp = xgb.importance(feature_names = feats, model = xgmod); print(varimp)
		# print(xgboost::xgb.plot.importance( importance_matrix = varimp))
	# }
	p <- predict(xgmod, xgtest)
	rm(xgmod); gc()
	ensemble <- ensemble + p
}
ensemble = ensemble/i


submission$PredictedProb <- ensemble
summary(submission$PredictedProb)
write.csv(submission, file = outfile, row.names=F, quote=F)
savehistory(file = "justfor_63.log")
save.image(file = "justfor_63.image", compress = T, safe = T)



savehistory(file = "justfor_v64v1.log")
save.image(file = "justfor_v64v1.image", compress = T, safe = T)

sink()

