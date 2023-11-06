library(tidyverse)
library(broom)
library(gtsummary)
library(stargazer)
setwd("~/Documents/GitHub/Political_Party_Turkey")
data <- read_csv("Party_data.csv")

dataset <- data %>% mutate(Openness= 5-rowSums(is.na(.)),
                           Age=as.numeric(Sys.Date()-Founded)) %>% 
                separate(Chair,c("Chair","Co_Chair"),sep = "-") %>% 
  mutate(Have_cochair=as.factor(ifelse(is.na(Co_Chair),FALSE,TRUE)))

#Descriptive Statistics
dataset %>% select(Age,Member_Count,Openness,Have_cochair) %>%
  tbl_summary(missing = "no",
              statistic = list(all_continuous() ~ "{median} ({sd})")) %>% 
  add_n() %>% 
  bold_labels()

#Regressions
model1 <- dataset %>% lm(Member_Count~Age+Openness+Have_cochair,data = .) 
model2 <- dataset %>% filter(Member_Count<10000000,
                             Member_Count>0) %>%
  lm(Member_Count~Age+Openness+Have_cochair,data = .) 



stargazer(model1,model2,style="qje")

