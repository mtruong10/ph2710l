# load packages
library(tidyverse)
library(here)
library(metafor)
library(meta)
library(readxl)

# read data
angina <- read_excel(here("data","hw8","dat4Meta.xlsx"), sheet = "Data_Angina")
dat.IHD = read_excel(here("data","hw8","dat4Meta.xlsx"), sheet="Data.IHD")

# fit model

FE_RE.angina = metacont(nE, 
                        meanE, 
                        sqrt(varE), 
                        nC, 
                        meanC, 
                        sqrt(varC), 
                        data=angina, 
                        studlab=Protocol, 
                        common=TRUE, 
                        random=TRUE)

# print model

FE_RE.angina

# forest plot

forest(FE_RE.angina) 

funnel(FE_RE.angina)  #Call funnel to make funnel plot

metabias(FE_RE.angina, k.min=5) # Call metabias for statistical test

# cannot calculate crude OR with 0's
dat.IHD$cpos <- ifelse(dat.IHD$cpos==0, 0.5, dat.IHD$cpos)

# calculate log odds-ratio
ES.IHD = escalc(measure="OR",ai=tpos,bi=tneg,ci=cpos, di = cneg, data = dat.IHD, append = TRUE)

print(ES.IHD, row.names = FALSE)

meta.FE = rma(yi, vi, method="FE", data = ES.IHD)

meta.FE

meta.RE = rma(yi, vi, data = ES.IHD)

meta.RE

forest(meta.RE, cex=0.75)

funnel(meta.RE)

ranktest(meta.RE)

regtest(meta.RE)

metaReg.RE.REML = rma(yi, vi, mods = ~chol, data = ES.IHD)

metaReg.RE.REML

newx = seq(0,1.5, length=100)

preds = predict(metaReg.RE.REML, newmods = newx, transf=exp)

wi = 1/sqrt(ES.IHD$vi+metaReg.RE.REML$tau2)

size = 1 + 2*(wi - min(wi))/(max(wi) - min(wi))

plot(ES.IHD$chol, exp(ES.IHD$yi),pch = 1,cex = size,xlim=c(0, 1.6), ylim=c(0,2), las = 1, bty = "l",ylab = "Odds Ratio", xlab = "Absolute Reduction in Cholesterol(mmol/l)")

lines(newx, preds$pred, lwd=3)

lines(newx, preds$ci.lb, lty = "dashed")

lines(newx, preds$ci.ub, lty = "dashed")

abline(h = 1, lwd=3,lty = "dotted")
