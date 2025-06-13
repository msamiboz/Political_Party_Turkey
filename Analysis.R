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
model2 <- dataset %>% filter(Member_Count<1000000,
                             Member_Count>0) %>%
  lm(Member_Count~Age+Openness+Have_cochair,data = .) 
dataset_mod <- dataset %>%  filter(!is.na(Member_Count),!is.na(Age))


model3 <- dataset_mod %>% 
  glm(formula=Member_Count~Age+I(Age^2)+I(Age^3)+
        Age*Openness+Age*Have_cochair,data = .,
      family="quasipoisson")
plot(fitted.values(model3),residuals.glm(model3))

indices_remove2 <- which(cooks.distance(model3) >0.01) %>% unname

dataset_mod2 <- dataset_mod[-(indices_remove2),]

model4 <-  dataset_mod2 %>% 
  glm(formula=Member_Count~Age+I(Age^2)+
        Openness+Have_cochair,data = .,
      family="poisson")

plot(fitted.values(model4),residuals.glm(model4))

summary(model4)

stargazer(model1,model3,model4,style="qje",type="html")

####
data2 <- read_csv("Party_data1.csv") %>% mutate(Founded = as.Date(Founded,"%d.%m.%Y"),
                                                Member_Count=as.double(gsub("\\.", "",Member_Count)))

dataset2 <- data2 %>% mutate(Openness= 5-rowSums(is.na(.)),
                Age=as.numeric(Sys.Date()-Founded)) %>% 
  separate(Chair,c("Chair","Co_Chair"),sep = "-") %>% 
  mutate(Have_cochair=as.factor(ifelse(is.na(Co_Chair),FALSE,TRUE)))
