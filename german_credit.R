
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
#@@@@@@@@@@@@@@@ DATA RETRIVING AND CLEANING ( start block) ##########
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
#@@@@@@@@@@@@@@@ DATA RETRIVING AND CLEANING ( end block) ##########





#@@@@@@@@@@@@@@@ DATA ANALISYS ( start block) ##########

#The following code chunk creates a table showing factors and respective unique values
#This is for a simple view to help to understand the kind and types of graphics to develop analysis

unique_values <- data_frame(VARIABLE=colNames[1],UNIQUE_VAL=nunique(credit_original[,1])) #Creating first row oa tibble
for (i in 2:length(names(credit_original))) { #calculate unique values for each variable and store them in the tibble adding rows
  unique_values <- bind_rows(unique_values,data_frame(VARIABLE=colNames[i],UNIQUE_VAL=nunique(credit_original[,i])))
}
unique_values %>% knitr::kable()







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

#@@@@@@@@@@@@@@@ DATA ANALISYS (end block) ##########

#@@@@@@@@@@@@@@@ PREPROCESSING (start block) ##########

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


#<<<<<<<<<<<<<< Data partition (start block) >>>>>>>>>>>>>


set.seed(123)
test_index <- createDataPartition(y = credit_norm$credit_response, times = 1, p = 0.3, list = FALSE)
german_credit_train <- credit_norm[-test_index,] #70% of total
german_credit_test <- credit_norm[test_index,] #30% of total
x <- german_credit_train[,1:19] #predictor
y <- german_credit_train[,20] #outcome


#<<<<<<<<<<<<<< Data partition (end block) >>>>>>>>>>>>>>>

#@@@@@@@@@@@@@@@ PREPROCESSING (end block) ##########

#@@@@@@@@@@@@@@@ MODEL GLM (start block)##########

# Fit the model on the german_credit_train training set.
#No cross validation, for GLM no tuning parameters
library(stats)
formula <-as.formula((names(x) %>% paste(.,collapse="+") %>% c("y",.) %>% paste0(. ,collapse="~")))
set.seed(1234)
fit_glm <- glm(formula, data=german_credit_train, family="binomial")
glm_pred_prob <- predict(fit_glm,german_credit_test, type="response") #Prediction as probability

glm_pred_prob <- round(glm_pred_prob)
glm_pred <- factor(ifelse(glm_pred_prob > 0.5, "good", "bad"))#Prediction as factor
cm_glm <- confusionMatrix(data=glm_pred, reference=german_credit_test$credit_response,positive = "good")
#Saving the model result
tab.glm <- data_frame(method = "GLM", Accuracy = cm_glm$overall["Accuracy"], Sensitivity=data.frame(cm_glm$byClass["Sensitivity"])[1,1],
           Specificity=data.frame(cm_glm$byClass["Specificity"])[1,1])

print( knitr::kable(tab.glm))


#@@@@@@@@@@@@@@@ MODEL GLM (end block)##########

#@@@@@@@@@@@@@@@ MODEL RANDOM FOREST (start block)##########

# RF Algorithm Cross Validation to look for mtry best parameter
metric <- "Accuracy"
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(1234)
rf.train <- train(x,y, data=german_credit_train, method="rf",
                  metric=metric, tuneLength=15, trControl=control)
print(rf.train) #print results
plot(rf.train, main="Random forest cross-validation") #plots the accuracy/mtree graphic
rf.best_mtry <- rf.train$bestTune$mtry #model best tune

rf.fits.all <- randomForest(x, y,  ntree = 500, mtry=rf.best_mtry ) #fits using best tuning
rf.pred.all <- predict(rf.fits.all, german_credit_test) #predict with all factors
rf.cm <- confusionMatrix(rf.pred.all,german_credit_test$credit_response,positive = "good") #saving confusion matrix result
#Creating a metrics table
tab.rf <- data_frame(method = "RF", Accuracy = rf.cm$overall["Accuracy"], Sensitivity=data.frame(rf.cm$byClass["Sensitivity"])[1,1],
                                 Specificity=data.frame(rf.cm$byClass["Specificity"])[1,1])  
print(knitr::kable(tab.rf))


#fitting using the most important variable for the rf model
rf.importance <- varImp(rf.train) %>% .$importance 
rf.importance <- rf.importance%>% mutate(variable=row.names(rf.importance)) %>%  filter(., Overall >25) %>% .$variable
plot(varImp(rf.train), main="Random forest model: variable importance")

#select the first n factors( ten in this case) concat names by "+" and "~" 
#and finally transform it to use as first pred/ouput parameter in train function  
rf.formula <-as.formula(rf.importance %>% paste(.,collapse="+") %>% c("y",.) %>% paste0(. ,collapse="~"))

rf.fits.imp <- randomForest(rf.formula, data=german_credit_train, ntree = 500, mtry=rf.best_mtry )  #training model with the subset importance variable
rf.pred.imp <- predict(rf.fits.imp,german_credit_test)   #test data prediction 
rf.cm.imp <- confusionMatrix(rf.pred.imp, reference=german_credit_test$credit_response,positive = "good")
#Saving the model result
tab.rf_imp <- data_frame(method = "RF_IMP", Accuracy = rf.cm.imp$overall["Accuracy"], Sensitivity=data.frame(rf.cm.imp$byClass["Sensitivity"])[1,1],
           Specificity=data.frame(rf.cm.imp$byClass["Specificity"])[1,1]) 

print(knitr::kable(tab.rf_imp))
#@@@@@@@@@@@@@@@ MODEL RANDOM TREES (end block)##########

#@@@@@@@@@@@@@@@ MODEL RPART (start block)##########


set.seed(1234)
rpart.train <- train(x, y, 
                     method = 'rpart',
                     metric = 'Accuracy',
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25))
)
title(main = "RPart cross validation")
plot(rpart.train, main="Rpart cross-validation")

rpart.best_mtry <- rpart.train$bestTune$cp #model best tune for rpart (cp parameter)


rpart.pred.all<- predict(rpart.train,german_credit_test)   #test data prediction 

rpart.cm <- confusionMatrix(rpart.pred.all, reference=german_credit_test$credit_response,positive = "good")
#Saving the model result
tab.rpart <- data_frame(method = "RPART", Accuracy = rpart.cm$overall["Accuracy"], Sensitivity=data.frame(rpart.cm$byClass["Sensitivity"])[1,1],
           Specificity=data.frame(rpart.cm$byClass["Specificity"])[1,1]) 
#rpart_pred <- predict(train_rpart,german_credit_test, type = "prob")  %>% .$good #test data prediction for ROC curve

print(knitr::kable(tab.rpart))

#varable importance analysis
plot(varImp(rpart.train), main="Rpart model: variable importance")
#select variables whose importance is greater than zero and arrange them in a formula
rpart.imp <- varImp(rpart.train) %>% .$importance 
rpart.imp <- rpart.imp%>% mutate(variable=row.names(rpart.imp)) %>%  filter(., Overall >0) %>% .$variable

rpart.impIndexes <- names(x) %in% rpart.imp
rpart.dataImp <- x[rpart.impIndexes]

set.seed(1234)
rpart.fits.imp <- train(rpart.dataImp, y, 
                     method = 'rpart',
                     metric = 'Accuracy',
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25))
)


rpart.pred.imp <- predict(rpart.fits.imp,german_credit_test)   #test data prediction 
rpart.cm.imp <- confusionMatrix(rpart.pred.imp, reference=german_credit_test$credit_response,positive = "good")
#Saving the model result
tab.rpart_imp <- data_frame(method = "RPART_IMP", Accuracy = rpart.cm.imp$overall["Accuracy"], Sensitivity=data.frame(rpart.cm.imp$byClass["Sensitivity"])[1,1],
           Specificity=data.frame(rpart.cm.imp$byClass["Specificity"])[1,1])
print(knitr::kable(tab.rpart_imp))

#@@@@@@@@@@@@@@@ MODEL RPART (end block)##########


#@@@@@@@@@@@@@@@ MODEL NAIVE_BAYES(start block)##########
# set up tuning grid
train_control <- trainControl(
  method = "cv", 
  number = 10
)
search_grid <- expand.grid( #tuning parameters
  usekernel = c(TRUE, FALSE),
  laplace = 0:5,
  adjust = seq(0, 5, by = 1)
)

# train model
nb.fit.all <- train(  #fits with all variables
  x = x,
  y = y,
  method = "naive_bayes",
  trControl = train_control,
  tuneGrid = search_grid
  )

#prediction and metrics using all variables
nb.pred.all <- predict(nb.fit.all, newdata = german_credit_test) #predicts with all vatiables
nb.cm <- confusionMatrix(nb.pred.all, german_credit_test$credit_response,positive = "good")

tab.nb <- data_frame(method = "NB", Accuracy =nb.cm$overall["Accuracy"], Sensitivity=data.frame(nb.cm$byClass["Sensitivity"])[1,1],
           Specificity=data.frame(nb.cm$byClass["Specificity"])[1,1]) 

print(knitr::kable(tab.nb))

plot(varImp(nb.fit.all),  main="Naive bayes: variable importance")



#the next code chunk selects the predictors whose importance is >30
#and creates a new df whose variables are only the five most important
nb.importance <- varImp(nb.fit.all) %>% .$importance 
nb.importance <- nb.importance%>% mutate(variable=row.names(nb.importance)) %>%  filter(., good >30) %>% .$variable
nb.impIndexes <- names(x) %in% nb.importance 
nb.dataImp <- x[nb.impIndexes]

###re-fitting using variable importance
nb.fit.varimp <- train( # train a model using only the "varimp>30" df
  nb.dataImp,y,
  method = "naive_bayes",
  trControl = train_control,
  tuneGrid = search_grid
  )

#prediction and metrics using all the five most important variables
nb.pred.imp <- predict(nb.fit.varimp, newdata = german_credit_test)
nb.cm.imp <- confusionMatrix(nb.pred.imp, german_credit_test$credit_response,positive = "good")

tab.nb_imp <- data_frame(method = "NB-IMP", Accuracy =nb.cm.imp$overall["Accuracy"], Sensitivity=data.frame(nb.cm.imp$byClass["Sensitivity"])[1,1],
                     Specificity=data.frame(nb.cm.imp$byClass["Specificity"])[1,1]) 

print(knitr::kable(tab.nb_imp))
#@@@@@@@@@@@@@@@ MODEL NAIVE_BAYES(end block)##########

#@@@@@@@@@@@@@@@ PCA EVALUATION(start block)##########

# first of all, the matrix must be normalized 
z <- apply(as.matrix(credit_num[,1:19]),2, scale) 

#then apply the PCA function
pca <- prcomp(z) #pc of numeric version of german_credit dataset
pca.summary <- summary(pca)
print(pca.summary)
proportion.variance <- pca.summary$importance[2,] # select vector of  variance proportion by PCx
proportion.cumulative <- pca.summary$importance[3,] #select vector of cumulative proportion  by PCx

#plot the proportions
plot(proportion.cumulative, type="l",xlab="PCs",ylab="Cumulative Proportion",main = "PCA: Cumulative trend")
plot(proportion.variance, type="b",  xlab="PCs",ylab="Variance Proportion",main = "PCA: Variance distribution")

#the three principal components ( PC1,PC2,PC3)
pr_comp <- round(sum(proportion.variance[1:3])*100)


#@@@@@@@@@@@@@@@ PCA EVALUATION(end block)##########

#@@@@@@@@@@@@@@@ RESULTS(start block)##########

#the cumulative auc curve for each model
library(pROC)
par(pty="s")
plot.roc(as.ordered(german_credit_test$credit_response),as.ordered(glm_pred),print.auc=TRUE, percent=TRUE,print.auc.y=50,col="red")
plot.roc(as.ordered(german_credit_test$credit_response),as.ordered(rf.pred.all),print.auc=TRUE, percent=TRUE,print.auc.y=45,col="blue", add=TRUE)
plot.roc(as.ordered(german_credit_test$credit_response),as.ordered(rpart.pred.all),print.auc=TRUE, percent=TRUE,print.auc.y=40,col="green", add=TRUE)
plot.roc(as.ordered(german_credit_test$credit_response),as.ordered(nb.pred.all),print.auc=TRUE, percent=TRUE,print.auc.y=35,col="brown", add=TRUE)
legend("bottomright",y.intersp=0.6, legend=c("GLM","RF", "RPART","NB"), col=c("red", "blue","green", "brown"), lwd=2)

#Next code chunk binds all the models results in a one-shot table, and then prints output
tab.complete <- tab.glm
tab.entry <- list(tab.rf,tab.rf_imp,tab.rpart,tab.rpart_imp,tab.nb,tab.nb_imp)
for(tab.record in tab.entry){
  
  tab.complete <- bind_rows(tab.complete,tab.record)
}

print(knitr::kable(tab.complete))
#@@@@@@@@@@@@@@@ RESULTS(end block)##########

