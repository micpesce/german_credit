
library(gdata)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(readtext)
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
credit_original <- read.csv("german.data", header=FALSE, sep = " ")
colNames = c("checking_account", "duration", "Credit_history", "purpose", "credit_amount","savings_account","employment_since","percentage_income","personal_status","guarantors","residence","property","age","other_plans","housing","other_credits","job","house_manteinant","telephone","foreign_worker","credit")
names(credit_original)   <-  colNames         
glimpse(credit_original)

#exploring NA'S
sum(is.na(credit_original))
sum(complete.cases(credit_original))

library(miscset)
unique_values <- data_frame(VARIABLE=colNames[1],UNIQUE_VAL=nunique(credit_original[,1])) #Creating first row oa tibble
for (i in 2:length(names(credit_original))) { #calculate unique values for each variable and store them in the tibble adding rows
  unique_values <- bind_rows(unique_values,data_frame(VARIABLE=colNames[i],UNIQUE_VAL=nunique(credit_original[,i])))
}
unique_values %>% knitr::kable()
#credit_original  mutate_at()
ggplot(credit_original,aes(x=credit_original$credit,y=credit_original$telephone))
####@@@@REPLACING coded values with meaningful data 
library(tm)
file.rename("german.doc","german.txt") #renames file to get it readable
key_map <- readLines("german.txt")
key_map <- key_map[grepl("A[0-9]", key_map)] #filter rows with coded variable
extra_char <- which(grepl(":.*:", key_map))

key_map <- as.data.frame(key_map)
key_map <- key_map %>% separate(key_map, into= c("KEY","VALUE"), sep=":" ) 
#in the next line of code the KEY column is cleaned
key_map$KEY <- gsub(" ", "",key_map$KEY, fixed = TRUE) %>% gsub("\t", "",., fixed = TRUE)


credit_clear <- credit_original #copy original dataset in a new for replacing coded values
for (i in 1:length(names(credit_clear))) { #iterates all columns
  if (is.factor(credit_clear[,i]))#selects only columns with AXXX factor format
    credit_clear[,i] <- key_map$VALUE[match(credit_clear[,i], key_map$KEY)] #replace key with values
}

i <- 2
is.factor(credit_clear[,i])

