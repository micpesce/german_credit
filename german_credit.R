
library(gdata)
library(dplyr)
library(stringr)
library(tidyr)
german_credit <- read.csv("german_credit_data.csv", header=TRUE, sep = ";")
#exploring NA'S
sum(is.na(german_credit))
sum(complete.cases(german_credit))

glimpse(german_credit) #exploring data types
german_credit_num <- german_credit%>% mutate_if(is.factor, as.numeric) #converting to a numeric df
glimpse(german_credit_num)
#exploring NA'S on numerics
sum(is.na(german_credit_num))
sum(complete.cases(german_credit_num))
summary(german_credit)
credit_df <- read.csv("german.data", header=FALSE, sep = " ")
colNames = c("checking_account", "duration", "Credit_history", "purpose", "credit_amount","savings_account","employment_since","percentage_income","personal_status","guarantors","residence","property","age","other_plans","housing","other_credits","job","house_manteinant","telephone","foreign_worker","credit")
names(credit_df)   <-  colNames         
glimpse(credit_df)

#exploring NA'S
sum(is.na(credit_df))
sum(complete.cases(credit_df))
