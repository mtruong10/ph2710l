library(readstata13)
library(tidyverse)
library(DescTools)
library(expss)
library(here)

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

t.test(dat[which(dat$chd=="No CHD"), "bmi"], dat[which(dat$chd=="Has CHD"), "bmi"])
round(sd(dat[which(dat$chd=="No CHD"), "bmi"]), digits=2)
round(sd(dat[which(dat$chd=="Has CHD"), "bmi"]), digits=2)


