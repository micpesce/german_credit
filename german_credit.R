
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


#credit admission plot
qplot(credit_clear$credit, geom="bar",
      fill=I('gray'), col=I('black'),
      xlab = "Credit admission" , ylab = "Count" )
pass_amount <- length(which(credit_clear$credit=="pass")) #good credit
fail_amount <- length(which(credit_clear$credit=="fail")) #bad credit

#credit amount analysis vs credit admission
#Two different plots each for good and bad credit, both for credit amount distribution

p1 <- credit_clear  %>% filter(credit=="pass") %>% ggplot(aes(x=.$credit_amount)) +
  ggtitle(" Credit pass")+ geom_histogram(fill="green", bins=30)  +
  scale_y_continuous(limits=c(0,150))+xlab("Credit amount")+ ylab("Count") 

p2 <- credit_clear %>% filter(credit=="fail") %>%  ggplot(aes(x=.$credit_amount)) +
  ggtitle(" Credit fail")+ geom_histogram(fill="red", bins=30)  +
  scale_y_continuous(limits=c(0,150))+ xlab("Credit amount")+ ylab("Count") 

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

#The credit history distribution
credit_clear  %>% ggplot(aes(Credit_history)) +
  ggtitle(" Credit history distribution")+ geom_bar(fill="blue") + 
  xlab("Credit history") + ylab("Count") + theme(axis.text.x = element_text(angle = 90))


#The credit history density
credit_clear  %>% ggplot(aes(Credit_history)) +
  ggtitle(" Credit history distribution")+ geom_density(fill="blue") + 
  xlab("Credit history") + ylab("Count") + theme(axis.text.x = element_text(angle = 90))

#credit history analysis vs credit admission
p1 <- credit_clear  %>% filter(credit=="pass") %>% ggplot(aes(x=.$Credit_history)) +
  ggtitle(" Credit pass")+ geom_bar(fill="green") + 
  scale_y_continuous(limits=c(0,400))+xlab("Credit history")+ ylab("Count") + theme(axis.text.x = element_text(angle = 90))

p2 <- credit_clear %>% filter(credit=="fail") %>%  ggplot(aes(x=.$Credit_history)) +
  ggtitle(" Credit fail")+ geom_bar(fill="red") + 
  scale_y_continuous(limits=c(0,400))+xlab("Credit history")+ ylab("Count") + theme(axis.text.x = element_text(angle = 90))

library(gridExtra)
grid.arrange(p1, p2, ncol = 2)

#Credit amount by personal status
credit_clear  %>%  ggplot(aes(x=personal_status,y=credit_amount,fill=credit)) +
  ggtitle(" Credit amount by personal status")+ geom_boxplot() + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))
 
#age by personal status
credit_clear  %>%  ggplot(aes(x=personal_status,y=age,fill=credit)) +
  ggtitle(" age by personal status")+ geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#emplyment-since by credit amount
credit_clear  %>%  ggplot(aes(x=employment_since,y=credit_amount,fill=credit)) +
  ggtitle(" mplyment-since by credit amount")+ geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#emplyment-since by credit amount based on Job
credit_clear  %>%  ggplot(aes(x=employment_since,y=credit_amount,fill=credit)) +
  ggtitle(" emplyment-since by credit amount")+ geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_grid(. ~ job)

#Cheking-account VS duration based on history

credit_clear %>% 
  ggplot(aes(checking_account, duration, fill=credit)) +
  geom_boxplot() +
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

for_free <- length(which(credit_clear$housing=="for_free")) #good credit
rent <- length(which(credit_clear$housing=="rent")) #good credit
own <- length(which(credit_clear$housing=="own")) #good credit
