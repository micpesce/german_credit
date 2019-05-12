
library(gdata)
library(dplyr)
library(stringr)
library(tidyr)
germam_credit <- read.csv("german_credit_data.csv", header=TRUE, sep = ";")
#exploring NA'S
sum(is.na(germam_credit))
sum(complete.cases(germam_credit))

glimpse(germam_credit) #exploring data types
germam_credit_num <- germam_credit%>% mutate_if(is.factor, as.numeric) #converting to a numeric df

#exploring NA'S on numerics
sum(is.na(germam_credit_num))
sum(complete.cases(germam_credit_num))
