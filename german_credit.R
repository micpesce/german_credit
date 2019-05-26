
library(gdata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(readtext)
library(graphics)
#Retrieving the row original dataset 
credit_original <- read.csv("german.data", header=FALSE, sep = " ")
#Metadata definition and assignment
colNames = c("checking_account", "duration", "Credit_history", "purpose", "credit_amount","savings_account","employment_since","percentage_income","personal_status","other_guarantors","residence","property","age","other_plans","housing","other_credits","job","house_manteinant","telephone","foreign_worker","credit")
names(credit_original)   <-  colNames
# a quick view
glimpse(credit_original)

#exploring NA'S
sum(is.na(credit_original))
sum(complete.cases(credit_original))

#@@@@@@@@@@@@@@@@@ DATA CLEANING @@@@@@@@@@@@@@@@@@@@@

####@@@@REPLACING coded values with meaningful data 
library(tm)
file.rename("german.doc","german.txt") #renames file to get it readable
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

credit_clear <- credit_clear %>% mutate(credit=ifelse(.$credit==1,"pass", "fail")) #making credit variable as categorical
credit_num<- credit_original%>% mutate_if(is.factor, as.numeric) #converting to a numeric df





#@@@@@@@@@@@@@@@@@@@@@@@@DATA ANALISYS @@@@@@@@@@@@@@@@@@@@@@

#The following code chunk creates a table showing factors and respective unique values
#This is for a simple view to help to understand the kind and types of graphics to develop analysis
library(miscset)
unique_values <- data_frame(VARIABLE=colNames[1],UNIQUE_VAL=nunique(credit_original[,1])) #Creating first row oa tibble
for (i in 2:length(names(credit_original))) { #calculate unique values for each variable and store them in the tibble adding rows
  unique_values <- bind_rows(unique_values,data_frame(VARIABLE=colNames[i],UNIQUE_VAL=nunique(credit_original[,i])))
}
unique_values %>% knitr::kable()


#<<<<<<<<<<<<<<credit admission plot>>>>>>>>>>>>>>>>>>>
qplot(credit_clear$credit, geom="bar",
      fill=I('gray'), col=I('black'),
      xlab = "Credit admission" , ylab = "Count" )
pass_amount <- length(which(credit_clear$credit=="pass")) #good credit
fail_amount <- length(which(credit_clear$credit=="fail")) #bad credit
#<<<<<<<<<<<<<<credit admission plot>>>>>>>>>>>>>>>>>>>




#<<<<<<<<<<<<<< credit amount analysis vs credit admission (start block) >>>>>>>>>>>>>>>>>>

#Two different plots each for good and bad credit, both for credit amount distribution
p1 <- credit_clear  %>% filter(credit=="pass") %>% ggplot(aes(x=.$credit_amount)) +
  ggtitle(" Credit pass")+ geom_histogram(fill="green", bins=30)  +
  scale_y_continuous(limits=c(0,150))+xlab("Credit amount")+ ylab("Count") #limits chosen empirically to show real proportion

p2 <- credit_clear %>% filter(credit=="fail") %>%  ggplot(aes(x=.$credit_amount)) +
  ggtitle(" Credit fail")+ geom_histogram(fill="red", bins=30)  +
  scale_y_continuous(limits=c(0,150))+ xlab("Credit amount")+ ylab("Count") 

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
#<<<<<<<<<<<<<< credit amount distribution vs credit admission (end block)>>>>>>>>>>>>>>>>>>>


#<<<<<<<<<<<<<< credit history distribution (start block) >>>>>>>>>>>>>
credit_clear  %>% ggplot(aes(Credit_history)) +
  ggtitle(" Credit history distribution")+ geom_bar(fill="blue") + 
  xlab("Credit history") + ylab("Count") + theme(axis.text.x = element_text(angle = 90))
#the next code computes a table showing summarized categories and proportions inside Credit_history
as.data.frame(credit_clear %>% group_by(Credit_history) %>% summarize(amount=n())) %>% mutate(perc=amount/dim(credit_clear)[1]*100)%>% knitr::kable()
#<<<<<<<<<<<<<< credit history distribution (end block) >>>>>>>>>>>>>>>



#<<<<<<<<<<<<<< credit history distribution vs credit admission (start block) >>>>>>>>>>>>>
p1 <- credit_clear  %>% filter(credit=="pass") %>% ggplot(aes(x=.$Credit_history)) +
  ggtitle(" Credit pass")+ geom_bar(fill="green") + 
  scale_y_continuous(limits=c(0,400))+xlab("Credit history")+ ylab("Count") + theme(axis.text.x = element_text(angle = 90))

p2 <- credit_clear %>% filter(credit=="fail") %>%  ggplot(aes(x=.$Credit_history)) +
  ggtitle(" Credit fail")+ geom_bar(fill="red") + 
  scale_y_continuous(limits=c(0,400))+xlab("Credit history")+ ylab("Count") + theme(axis.text.x = element_text(angle = 90))

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
#<<<<<<<<<<<<<< credit history distribution vs credit admission (end block) >>>>>>>>>>>>>



#<<<<<<<<<<<<<< Personal status vs Credit amount and age (start block) >>>>>>>>>>>>>
credit_clear  %>%  ggplot(aes(x=personal_status,y=credit_amount,fill=credit)) +
  ggtitle(" Credit amount by personal status")+ geom_boxplot(varwidth = TRUE) + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

#the next code computes a table showing summarized categories and proportions inside personal_status
as.data.frame(credit_clear %>% group_by(personal_status) %>% summarize(amount=n())) %>% 
  mutate(perc=amount/dim(credit_clear)[1]*100)%>% knitr::kable()
#extracting total amount of male_single with credit  >= 7500 approved
credit_clear %>% filter(credit_amount >=7500   & personal_status=="male_single" & credit=="pass") %>% count(.)   %>% .$n

#age by personal status
credit_clear  %>%  ggplot(aes(x=personal_status,y=age,fill=credit)) +
  ggtitle(" age by personal status")+ geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#<<<<<<<<<<<<<< Personal status vs Credit amount and age (end block) >>>>>>>>>>>>>





#<<<<<<<<<<<<<< Emplyment and job Analysis (start block) >>>>>>>>>>>>>



#Job Proportion table
as.data.frame(credit_clear %>% group_by(job) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()

#Employment history table
as.data.frame(credit_clear %>% group_by(employment_since) %>% summarize(Total=n())) %>% 
  mutate(perc=Total/dim(credit_clear)[1]*100)%>% knitr::kable()


#Emplyment history vs credit_amount on job as parameter
#Unemployed has been kept out, because, as a parameter, is quite irrelevant
credit_clear  %>%  filter(job!="unemployed/unskilled_non-resident") %>% ggplot(aes(x=employment_since,y=credit_amount,fill=credit)) +
  ggtitle("emplyment-history by credit amount on job parameter")+ geom_boxplot(varwidth = TRUE) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ job)

#<<<<<<<<<<<<<< Emplyment and job Analysis (end block) >>>>>>>>>>>>>

#Cheking-account VS duration based on history

credit_clear %>% 
  ggplot(aes(checking_account, duration, fill=credit)) +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Cheking-account") +
  facet_wrap(. ~ Credit_history)

#Cheking-account VS credit_amount

credit_clear %>% 
  ggplot(aes(checking_account, credit_amount, fill=credit)) +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Cheking-account") 
  

#property VS credit_amount based on saving amount

credit_clear %>% 
  ggplot(aes(property, credit_amount, fill=credit)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Property") +
  facet_wrap(. ~ savings_account)

#age VS credit_amount based on saving amount

credit_clear %>% 
  ggplot(aes(age, credit_amount, color=credit)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("age") +
  facet_wrap(. ~ savings_account)


#purpose VS credit_amount 
credit_clear %>% 
  ggplot(aes(purpose, credit_amount, fill=credit)) +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("purpose") 
  
#guarantors VS credit_amount 
credit_clear %>% 
  ggplot(aes(guarantors, credit_amount, fill=credit)) +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("guarantors") 

#housing VS credit_amount 
credit_clear %>% 
  ggplot(aes(housing, credit_amount, fill=credit)) +
  geom_boxplot(varwidth = TRUE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("housing") 

library(corrplot)
cor(credit_num) %>% corrplot(., type="upper", order="hclust", tl.col="black")
cor(credit_num$duration,credit_num$credit_amount)
qplot(credit_num$credit_amount,credit_num$duration, fill=credit_num$credit)

for_free <- length(which(credit_clear$housing=="for_free")) #good credit
rent <- length(which(credit_clear$housing=="rent")) #good credit
own <- length(which(credit_clear$housing=="own")) #good credit
