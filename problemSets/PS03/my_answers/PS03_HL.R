#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
library(tidyverse)
library(ggplot2)
library(stargazer)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")

#Q1.1  Run a regression between voteshare and difflog
# Check data information and missed value
summary(inc.sub)
sum(is.na(inc.sub[, c("voteshare", "difflog")]))
# Build regression model
m1 <- lm(voteshare ~ difflog, data = inc.sub)
summary(m1)

#Q1.2 Plot the two variables and add the regression line
# Use ggplot2 
pdf("Q1_scatter_plot.pdf")
ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(size = 0.8, alpha = 0.8, color = "skyblue") +
  geom_smooth(method = "lm", se = FALSE , linewidth = 0.5 ) +
  labs(x = "spending difference between incumbent and challenger", y = "incumbent's vote share") +
  theme_bw()
dev.off()

#Q1.3 Save the residuals of the model in a separate object
res1 <- resid(m1)
res1

#Q2.1 Run a regression between presvote and difflog
sum(is.na(inc.sub[, c("presvote")]))
m2 <- lm(presvote ~ difflog, data = inc.sub)
summary(m2)

#Q2.2 Plot the two variables and add the regression line
pdf("Q2_scatter_plot.pdf")
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(size = 0.8, alpha = 0.8, color = "lightcoral") +
  geom_smooth(method = "lm", se = FALSE ,linewidth = 0.5 , color = "red") +
  labs(x = "spending difference between incumbent and challenger", y = "president candidate's vote share") +
  theme_bw()
dev.off()

#Q2.3 Save the residuals of the model in a separate object
res2 <- resid(m2)
res2

#Q3.1 Run aregression where the outcome variable is voteshare and the explanatory variable is presvote.
m3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(m3)

#Q3.2 Plot the two variables and add the regression line
pdf("Q3_scatter_plot.pdf")
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(size = 0.8, alpha = 0.8, color = "seagreen3") +
  geom_smooth(method = "lm", se = FALSE ,linewidth = 0.5 , color = "darkgreen") +
  labs(x = "president candidate's vote share", y = "incumbent's vote share") +
  theme_bw()
dev.off()

#Q4.1 Run a regression between res1 and res2
df <- data.frame('m1_residuals'= res1, 'm2_residuals' = res2)
m_control <- lm(res1 ~ res2 , data = df)
summary(m_control)
# show real number of res2's coefficient(0.2569) and intercept(-0.000000000000000005934)
format(2.569e-01, scientific = FALSE)
format(-5.934e-18, scientific = FALSE)

#Q4.1 Plot the regression between res1 and res2
pdf("Q4_scatter_plot.pdf")
ggplot(df, aes(x = res2, y = res1)) +
  geom_point(size = 0.8, alpha = 0.8, color = "gold") +
  geom_smooth(method = "lm", se = FALSE ,linewidth = 0.5 , color = "goldenrod") +
  labs(x = "presvote residuals", y = "voteshare residuals") +
  theme_bw()
dev.off()

#Q5.1 Build a multivariate regression model
m5 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(m5)
stargazer(m5, type = "latex",
          title = "Multivariate Regression of Incumbent Vote Share",
          label = "tab:m5",
          dep.var.labels = "Incumbent Vote Share",
          covariate.labels = c("Log Spending Difference", "President Candidate's Vote Share"),
          digits = 4,
          out = "model5.tex") 