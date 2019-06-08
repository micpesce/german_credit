
library(gdata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(readtext)
library(graphics)
library(caret)
library(miscset)
library(tm)
#Retrieving the row original dataset 
credit_original <- read.csv("german.data", header=FALSE, sep = " ")
#Metadata definition and assignment
colNames = c("checking_account", "credit_duration", "Credit_history", "purpose", "credit_amount","savings_account","employment_since","percentage_income","personal_status","other_guarantors","residence","property","age","other_plans","housing","existing_credits","job","house_manteinant","telephone","foreign_worker","credit_response")
names(credit_original)   <-  colNames
# a quick view
glimpse(credit_original)

#exploring NA'S
sum(is.na(credit_original))
sum(complete.cases(credit_original))

#@@@@@@@@@@@@@@@@@ DATA CLEANING @@@@@@@@@@@@@@@@@@@@@

####@@@@REPLACING coded values with meaningful data 

#file.rename("german.doc","german.txt") #renames file to get it readable
key_map <- readLines("german.txt")
key_map <- key_map[grepl("A[0-9]", key_map)] #filter rows with coded variable
key_map <- as.data.frame(key_map)
key_map <- key_map %>% separate(key_map, into= c("KEY","VALUE"), sep=":" ) 
#in the next line of code the key_map DF is cleaned
key_map$KEY <- gsub(" ", "",key_map$KEY, fixed = TRUE) %>% gsub("\t", "",., fixed = TRUE)
key_map$VALUE <- gsub(" ", "",key_map$VALUE, fixed = TRUE) %>% gsub("\t", "",., fixed = TRUE)

credit_clear <- credit_original #copy original dataset in a new for replacing coded values
for (i in 1:length(names(credit_clear))) { #iterates all columns
  if (is.factor(credit_clear[,i]))#selects only columns with AXXX factor format
    credit_clear[,i] <- key_map$VALUE[match(credit_clear[,i], key_map$KEY)] #replace key with values
}

credit_clear <- credit_clear %>% mutate(credit_response=ifelse(.$credit_response==1,"good", "bad")) #making credit_response variable as categorical






#@@@@@@@@@@@@@@@@@@@@@@@@DATA ANALISYS @@@@@@@@@@@@@@@@@@@@@@

#The following code chunk creates a table showing factors and respective unique values
#This is for a simple view to help to understand the kind and types of graphics to develop analysis

unique_values <- data_frame(VARIABLE=colNames[1],UNIQUE_VAL=nunique(credit_original[,1])) #Creating first row oa tibble
for (i in 2:length(names(credit_original))) { #calculate unique values for each variable and store them in the tibble adding rows
  unique_values <- bind_rows(unique_values,data_frame(VARIABLE=colNames[i],UNIQUE_VAL=nunique(credit_original[,i])))
}
unique_values %>% knitr::kable()


#<<<<<<<<<<<<<<credit admission plot>>>>>>>>>>>>>>>>>>>
qplot(credit_clear$credit_response, geom="bar",
      fill=I('gray'), col=I('black'),
      xlab = "Credit response" , ylab = "Count" )

#<<<<<<<<<<<<<<credit response plot>>>>>>>>>>>>>>>>>>>




#<<<<<<<<<<<<<< credit amount analysis vs credit response (start block) >>>>>>>>>>>>>>>>>>

#Two different plots each for good and bad response, both for credit amount distribution
p1 <- credit_clear  %>% filter(credit_response=="good") %>% ggplot(aes(x=.$credit_amount)) +
  ggtitle("Good credit")+ geom_histogram(fill="green", bins=30)  +
  scale_y_continuous(limits=c(0,150))+xlab("Credit amount")+ ylab("Count") #limits chosen empirically to show real proportion

p2 <- credit_clear %>% filter(credit_response=="bad") %>%  ggplot(aes(x=.$credit_amount)) +
  ggtitle("Bad credit")+ geom_histogram(fill="red", bins=30)  +
  scale_y_continuous(limits=c(0,150))+ xlab("Credit amount")+ ylab("Count") 

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
#<<<<<<<<<<<<<< credit amount distribution vs credit response (end block)>>>>>>>>>>>>>>>>>>>


#<<<<<<<<<<<<<< credit history distribution (start block) >>>>>>>>>>>>>
credit_clear  %>% ggplot(aes(Credit_history)) +
  ggtitle(" Credit history distribution")+ geom_bar(fill="blue") + 
  xlab("Credit history") + ylab("Count") + theme(axis.text.x = element_text(angle = 90))
#the next code computes a table showing summarized categories and proportions inside Credit_history
as.data.frame(credit_clear %>% group_by(Credit_history) %>% summarize(amount=n())) %>% mutate(perc=amount/dim(credit_clear)[1]*100)%>% knitr::kable()
#<<<<<<<<<<<<<< credit history distribution (end block) >>>>>>>>>>>>>>>



#<<<<<<<<<<<<<< credit history distribution vs credit response (start block) >>>>>>>>>>>>>
p1 <- credit_clear  %>% filter(credit_response=="good") %>% ggplot(aes(x=.$Credit_history)) +
  ggtitle("Good Credit")+ geom_bar(fill="green") + 
  scale_y_continuous(limits=c(0,400))+xlab("Credit history")+ ylab("Count") + theme(axis.text.x = element_text(angle = 90))

p2 <- credit_clear %>% filter(credit_response=="bad") %>%  ggplot(aes(x=.$Credit_history)) +
  ggtitle("Bad credit")+ geom_bar(fill="red") + 
  scale_y_continuous(limits=c(0,400))+xlab("Credit history")+ ylab("Count") + theme(axis.text.x = element_text(angle = 90))

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
#<<<<<<<<<<<<<< credit history distribution vs credit response (end block) >>>>>>>>>>>>>



#<<<<<<<<<<<<<< Personal status vs Credit amount and age (start block) >>>>>>>>>>>>>
credit_clear  %>%  ggplot(aes(x=personal_status,y=credit_amount,fill=credit_response)) +
  ggtitle(" Credit amount by personal status")+ geom_boxplot(varwidth = TRUE) + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

#the next code computes a table showing summarized categories and proportions inside personal_status
as.data.frame(credit_clear %>% group_by(personal_status) %>% summarize(amount=n())) %>% 
  mutate(perc=amount/dim(credit_clear)[1]*100)%>% knitr::kable()
#extracting total amount of male_single with credit  >= 7500 approved
credit_clear %>% filter(credit_amount >=7500   & personal_status=="male_single" & credit_response=="good") %>% count(.)   %>% .$n

#age by personal status
credit_clear  %>%  ggplot(aes(x=personal_status,y=age,fill=credit_response)) +
  ggtitle(" age by personal status")+ geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#<<<<<<<<<<<<<< Personal status vs Credit amount and age (end block) >>>>>>>>>>>>>

credit_clear  %>%  ggplot(aes(x=personal_status,y=age,fill=credit_response)) +
  ggtitle(" age by personal status")+ geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#<<<<<<<<<<<<<< Employment and job Analysis (start block) >>>>>>>>>>>>>

#Job Proportion table
as.data.frame(credit_clear %>% group_by(job) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()

#Employment history proportion table
as.data.frame(credit_clear %>% group_by(employment_since) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()


#Emplyment history vs credit_amount on job as parameter
#Unemployed has been kept out, because, as a parameter, is quite irrelevant
credit_clear  %>%  filter(job!="unemployed/unskilled_non-resident") %>% ggplot(aes(x=employment_since,y=credit_amount,fill=credit_response)) +
  ggtitle("EmplOyment-history by credit amount on job parameter")+ geom_boxplot(varwidth = TRUE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ job)

#<<<<<<<<<<<<<< Emplyment and job Analysis (end block) >>>>>>>>>>>>>

#<<<<<<<<<<<<<< Cheking-account,duration and  credit_history analysis (start block) >>>>>>>>>>>>>

#Credit_history proportion table
as.data.frame(credit_clear %>% group_by(Credit_history) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()

#checking_account proportion table
as.data.frame(credit_clear %>% group_by(checking_account) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()

#Duration density plot 
ggplot(credit_clear, aes(credit_duration, fill=credit_response)) + 
  geom_density(alpha=.5)

#Cheking-account VS duration
credit_clear %>% 
  ggplot(aes(checking_account, credit_duration, fill=credit_response)) +
  ggtitle("Cheking-account by duration" )+
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Cheking-account VS duration on credit_history parameter 
credit_clear %>% 
  ggplot(aes(checking_account, credit_duration, fill=credit_response)) +
  ggtitle("Cheking-account by duration on credit_history parameter")+
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(. ~ Credit_history)

#Cheking-account VS credit_amount
credit_clear %>% 
  ggplot(aes(checking_account, credit_amount, fill=credit_response)) +
  ggtitle("Credit amount by Cheking-account") +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
#<<<<<<<<<<<<<< Cheking-account,duration and  credit_history anlysis (end block) >>>>>>>>>>>>>




#<<<<<<<<<<<<<< credit_amount vs other meaniningful attributes (start block) >>>>>>>>>>>>>

#savings_account proportion table
as.data.frame(credit_clear %>% group_by(savings_account) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()
#property proportion table
as.data.frame(credit_clear %>% group_by(property) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()
#purpose proportion table
as.data.frame(credit_clear %>% group_by(purpose) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()

#property VS credit_amount based on saving account
credit_clear %>% filter(savings_account!="excellent") %>%
  ggplot(aes(property, credit_amount, fill=credit_response)) +
  ggtitle("Credit amount vs Property on saving-account parameter") +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(. ~ savings_account)

#age VS credit_amount based on saving amount
credit_clear %>% 
  ggplot(aes(age, credit_amount, color=credit)) +
  ggtitle("Credit amount vs age on saving-account parameter") +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(. ~ savings_account)




#purpose VS credit_amount
credit_clear %>% 
  ggplot(aes(purpose, credit_amount, fill=credit_response)) +
  ggtitle("purpose VS credit_amount") +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
  
#guarantors VS credit_amount 
credit_clear %>% 
  ggplot(aes(other_guarantors, credit_amount, fill=credit_response)) +
  ggtitle("guarantors VS credit_amount") +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
  
#housing VS credit_amount 
credit_clear %>% 
  ggplot(aes(housing, credit_amount, fill=credit_response)) +
  ggtitle("housing VS credit_amount") +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
   
#<<<<<<<<<<<<<< credit_amount vs other meaniningful atributes (end block) >>>>>>>>>>>>>

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@PREPROCESSING @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#<<<<<<<<<<<<<< correlation and variables selection (start block) >>>>>>>>>>>>>
credit_num<- credit_original%>% mutate_if(is.factor, as.numeric) #converting to a numeric df
glimpse(credit_num)

library(corrplot)
#create a visual map showing the correlation values between all variables
#and is ordered by clustering
cor(credit_num) %>% corrplot(., type="upper", order="hclust", tl.col="black")
#The couple of variables most correlated
cor(credit_num$credit_duration,credit_num$credit_amount)
#the most correlated variables scatter plot and smoothing
credit_clear  %>% ggplot(aes(credit_amount,credit_duration, color=credit)) +geom_point() + geom_smooth()


library(caret)
nzv <- NULL #initialize variable
#the nzv function looks for variables that could be cut out from predictors
nzv <- nearZeroVar(credit_num, names=TRUE, saveMetrics = TRUE) #returns a detailed table
nzv <- nearZeroVar(credit_num, names=FALSE) #returns the column index
if(!is.null(nzv)) #if nzv is consistent ( calculated by function)..
  credit_clear <- credit_clear[,-nzv]#..cut nrz variable from df 
  credit_num <- credit_num[,-nzv]#..cut nrz variable from df
#<<<<<<<<<<<<<< correlation and variables selection (end block) >>>>>>>>>>>>>


#<<<<<<<<<<<<<< Normalization (start block) >>>>>>>>>>>>>

credit_norm <- credit_clear # a df copy to be normalized 
col_num <- credit_norm[, sapply(credit_norm,is.numeric)] %>% names(.) #extracts numeric column names
col_cat <- credit_norm[, sapply(credit_norm,is.character)] %>% names(.) #extracts categorical column names

for (n in col_num) { #scales all numeric variables
  credit_norm[,n] <- scale(credit_norm[,n])
  
}
for (f in col_cat) { #factorizes all categorical variables
  credit_norm[,f] <- as.factor(credit_norm[,f])
}
#<<<<<<<<<<<<<< Normalization (end block) >>>>>>>>>>>>>
#test
col_num_orig <- credit_original[, sapply(credit_original,is.numeric)] %>% names(.) #extracts numeric column names
col_num_orig <- col_num_orig[-(which(col_num_orig=="credit_response"))]
#col_num_orig <- col_num_orig[, "credit_response"]
for (n in col_num_orig) { #scales all numeric variables
  credit_original[,n] <- scale(credit_original[,n])
  
}
credit_original["credit_response"] <- as.factor(credit_original[,"credit_response"])
#!test



#<<<<<<<<<<<<<< Data partition (start block) >>>>>>>>>>>>>


set.seed(123)
test_index <- createDataPartition(y = credit_norm$credit_response, times = 1, p = 0.3, list = FALSE)
german_credit_train <- credit_norm[-test_index,] #70% of total
german_credit_test <- credit_norm[test_index,] #30% of total
x <- german_credit_train[,1:19] #predictor
y <- german_credit_train[,20] #outcome


#<<<<<<<<<<<<<< Data partition (end block) >>>>>>>>>>>>>>>
#@@@@test
library(caret)
set.seed(123)
test_index <- createDataPartition(y = credit_original$credit_response, times = 1, p = 0.3, list = FALSE)
gc_train <- credit_original[-test_index,] #70% of total
gc_test <- credit_original[test_index,] #70% of total
li <- which(names(gc_train)=="credit_response") #to get the label variable index
xtr <- gc_train[,-li] # The predictors train data set
ytr <- gc_train$credit_response
ytr <- factor(ytr)

#!test
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@ THE ML MODELING APPROACH @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#models to try: rf, glm, rpart,svg/pca, cart
fit_knn <- knn3(xtr, ytr,  k = 5)
train_nb <- train(x, y, method = 'nb', data = german_credit_train)

#@@@@@@@@@@@@@@@ MODEL GLM (start block)##########

# Fit the model on the german_credit_train training set.
#No cross validation, for GLM no tuning parameters
library(stats)
formula <-as.formula((names(x) %>% paste(.,collapse="+") %>% c("y",.) %>% paste0(. ,collapse="~")))
set.seed(123)
fit_glm <- glm(formula, data=german_credit_train, family="binomial")
glm_pred <- predict(fit_glm,german_credit_test, type="response") #Prediction as probability

glm_pred <- round(glm_pred)
y_hat_glm <- factor(ifelse(glm_pred > 0.5, "good", "bad"))#Prediction as factor
cm_glm <- confusionMatrix(data=y_hat_glm, reference=german_credit_test$credit_response,positive = "good")
#Saving the model result
data_frame(method = "GLM", Accuracy = cm_glm$overall["Accuracy"], Sensitivity=data.frame(cm_glm$byClass["Sensitivity"])[1,1],
           Specificity=data.frame(cm_glm$byClass["Specificity"])[1,1]) %>% knitr::kable()
library(pROC)
par(pty="s")
plot.roc(y,fit_glm$fitted.values,print.auc=TRUE, percent=TRUE,col="#4daf4a")
plot.roc(as.ordered(german_credit_test$credit_response),as.ordered(y_hat_glm),print.auc=TRUE, percent=TRUE,print.auc.y=40,col="#377eb8", add=TRUE)
legend("bottomright", legend=c("GLM TEST", "GLM TRAIN"), col=c("#377eb8", "#4daf4a"), lwd=8)

#######################################################

#@@@@@@@@@@@@@@@ RANDOM TREES (start block)##########




# RF Algorithm Cross Validation to look for mtry best parameter
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(7)
train_RF <- train(x,y, data=german_credit_train, method="rf", metric=metric, tuneLength=15, trControl=control)
print(train_RF) #print results
plot(train_RF) #plots the accuracy/mtree graphic
best_mtry <- rtrain_RF$bestTune$mtry #model best tune




rf_fits <- randomForest(x, y,  ntree = 500, mtry=best_mtry ) #fits using best tuning
rf_pred <- predict(rf_fits, german_credit_test)
cm_rf <- confusionMatrix(rf_pred,german_credit_test$credit_response,positive = "good") #saving confusion matrix result
#Creating a metrics table
prediction_results <- data_frame(method = "RF", Accuracy = cm_rf$overall["Accuracy"], Sensitivity=data.frame(cm_rf$byClass["Sensitivity"])[1,1],
                                 Specificity=data.frame(cm_rf$byClass["Specificity"])[1,1])
prediction_results%>% knitr::kable()

library(pROC)
par(pty="s")
plot.roc(y,rf_fits$votes[,1],print.auc=TRUE, percent=TRUE,col="#4daf4a")
plot.roc(as.ordered(german_credit_test$credit_response),as.ordered(rf_pred),print.auc=TRUE, percent=TRUE,print.auc.y=40,col="#377eb8", add=TRUE)
legend("bottomright", legend=c("RF TEST", "RF TRAIN"), col=c("#377eb8", "#4daf4a"), lwd=8)
imp_RF <- as.data.frame(importance(rf))
imp_RF <- imp_RF%>% mutate(variable=row.names(imp_RF))
imp_RF[order(imp_RF$MeanDecreaseGini, decreasing = TRUE), ]

rf_importance <-varImp(train_RF)




#select the first n factors( ten in this case) concat names by "+" and "~" 
#and finally transform it to use as first pred/ouput parameter in train function  
in_out <-as.formula(row.names(rf_importance[["importance"]][1])[1:10] %>% paste(.,collapse="+") %>% c("x",.) %>% paste0(. ,collapse="~"))

#varImp
library(randomForest)
rf <- randomForest(x, y,  ntree = 50)
imp_RF <- as.data.frame(importance(rf))
imp_RF <- imp_RF%>% mutate(variable=row.names(imp_RF))
imp_RF[order(imp_RF$MeanDecreaseGini, decreasing = TRUE), ]

#@@@@@@@@@@@@@@@ MODEL RPART (start block)##########
library(rpart)
rp_fit <- rpart(formula=formula,
      method="class",data=german_credit_train,control =
        rpart.control(minsplit=20, cp=0.05))
set.seed(7)
train_rpart <- train(x, y, 
                     method = 'rpart',
                     metric = 'Accuracy',
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25))
                     )
ggplot(train_rpart)

#@@@@@@@@@@@@@@@ MODEL RPART (end block)##########
##################ensembles
#training and predicting different models
models_ensemble <- c("glm","ranger",  "naive_bayes",  "adaboost", "gbm", "kknn","gam", "rf", "avNNet")
fits_ensemble <- lapply(models_ensemble , function(model){ 
  print(model)
  train(x,y, method = model)
}) 
names(fits_ensemble) <- models_ensemble
pred <- sapply(fits_ensemble, function(object) 
  predict(object, newdata = german_credit_test))



acc_ensemble <- colMeans(pred == german_credit_test$credit_response) #accuracy for each model
print(acc_ensemble)
avg_acc <- mean(acc_ensemble) #and mean

votes <- rowMeans(pred_ensemble == "good") #ensemble prediction  
y_hat <- ifelse(votes > 0.5, "good", "bad")
ens_acc <- mean(y_hat == german_credit_test$credit_response) #accuracy of the ensemble

#let's compare individual metods and ensemble

ind <- acc > ens_acc
sum(ind)
models2[ind]
######################

#####################################KNN TEST NUMERIC


credit_original["credit_response"] <- as.factor(credit_original[,"credit_response"])

credit_num <- credit_num %>% mutate(credit_response=ifelse((credit_response==1),"good","bad"))
credit_num <- credit_num %>% mutate(credit_response=as.factor(credit_response))
set.seed(123)

col_ <- credit_num[-(which(credit_num=="credit_response"))]

#col_num_orig <- col_num_orig[, "credit_response"]
for (n in 1:19) { #scales all numeric variables
  credit_num[,n] <- scale(credit_num[,n])
  
}
test_index <- createDataPartition(y = credit_num$credit_response, times = 1, p = 0.3, list = FALSE)
credit_num_train <- credit_num[-test_index,] #70% of total
credit_num_test <- credit_num[test_index,] #30% of total

x_num <- credit_num_train[,1:19]
y_num <- credit_num_train[,20]

preProcess(x_num, method = c("center", "scale"))
control <- trainControl(method = "cv", number = 10, p = .9)
model_knn <- train( x_num, y_num, method = "knn", trControl = control, 
                   tuneGrid = data.frame(k = c(3,5,7)))


#form=y_num ~ .
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@#@@@@@@@@@@@@@@@@@@@@@@@@@@@@