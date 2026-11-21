library(readstata13)
library(tidyverse)
library(DescTools)
library(expss)
library(here)
library(lmtest)

exp(0.34)

data <- read.dta13(here("data","hw7","Homework 7 data.dta"))

# create table
make_a_summary_table <- function(x, y){
  rep <- cbind(table(data[, x], y),
               round(prop.table(table(data[which(y=="No CHD"), x]))*100, digits=2),
               round(prop.table(table(data[which(y=="Has CHD"), x]))*100, digits=2))
  colnames(rep) <-  c("No CHD", "Has CHD", "No CHD %", "Has CHD %")   
  print(rep[,c("No CHD", "No CHD %", "Has CHD", "Has CHD %")])
  print(chisq.test(table(data[, x], y),correct=FALSE))
}

make_a_summary_table(x="smoker3", y=data$chd)
make_a_summary_table(x="sex", y=data$chd)
make_a_summary_table(x="diabetes", y=data$chd)


data |> 
  group_by(chd) |>
  summarize(mean_bmi = mean(bmi))
t.test(data[which(data$chd=="No CHD"), "bmi"], data[which(data$chd=="Has CHD"), "bmi"])
round(sd(data[which(data$chd=="No CHD"), "bmi"]), digits=2)
round(sd(data[which(data$chd=="Has CHD"), "bmi"]), digits=2)

# logistic regression

# ordinal coding
mylogit <- glm(data$chd ~ data$smoker3, family = "binomial")
cbind(Beta_coefficient = coef(mylogit), confint(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# categorical coding
mylogit <- glm(data$chd ~ as.factor(data$smoker3), family = "binomial")
cbind(Beta_coefficient = coef(mylogit), confint(mylogit))
exp(cbind(OR = coef(mylogit), confint(mylogit)))


# assess if sex is an effect measure modifier

mylogit <- glm(data$chd ~ as.factor(data$smoker3) + data$sex + as.factor(data$smoker3)*data$sex, family = "binomial")
summary(mylogit)

# likelihood ratio test

a <- glm(data$chd ~ as.factor(data$smoker3) + data$sex + as.factor(data$smoker3)*data$sex, family = "binomial")
b <- glm(data$chd ~ as.factor(data$smoker3) + data$sex, family = "binomial")
lrtest(a, b) 

# assess if sex, diabete, and bmi are confounders

mylogit <- glm(data$chd ~ as.factor(data$smoker3), family = "binomial")
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# sex
mylogit <- glm(data$chd ~ as.factor(data$smoker3) + data$sex, family = "binomial")
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# diabete
mylogit <- glm(data$chd ~ as.factor(data$smoker3) + data$diabetes, family = "binomial")
exp(cbind(OR = coef(mylogit), confint(mylogit)))

# bmi
mylogit <- glm(data$chd ~ as.factor(data$smoker3) + data$bmi, family = "binomial")
exp(cbind(OR = coef(mylogit), confint(mylogit)))
  
# final interpretation

# sex
mylogit <- glm(data$chd ~ as.factor(data$smoker3) + data$sex, family = "binomial")
exp(cbind(OR = coef(mylogit), confint(mylogit)))

