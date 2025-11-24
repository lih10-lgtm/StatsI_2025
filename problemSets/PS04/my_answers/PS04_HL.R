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
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Q1(a) Create a new variable professional
# Import data 
install.packages("car")
library(car)
data(Prestige)
head(Prestige)
help(Prestige)
# Recode type to a binary variable'professional'
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
head(Prestige)

# Q1(b) Build the required interaction model 
# Firstly set non-professional(wc and bc) as baseline
professional_dummy <- factor(Prestige$professional,
                                  levels = c(0,1),
                                  labels = c("Non-professional", "Professional"))
# Create interaction mode;
interat_reg <- lm(prestige ~ income + professional_dummy + income:professional_dummy,
                  data = Prestige)
summary(interat_reg)
install.packages("stargazer")
library(stargazer)
stargazer(interat_reg, type = "latex",
          title="Regression Results",
          label="tab:interaction",
          dep.var.labels=c("Prestige"),
          covariate.labels=c("Income",
                             "Professional",
                             "Income × Professional"),
          out="interact_reg.tex")

#Q2(a) Conduct a hypothesis test for β1
beta1 <- 0.042
se1   <- 0.016
# Calculate t-value
t_value1 <- beta1 / se1
t_value1

# Calculate craticial t-value under the significance level of 0.05
df <- 131 - 3
t_critical <- qt(1-0.05/2, df)
t_critical

# Calculate p-value
p_value1 <- 2 * (1 - pt(abs(t_value1), df = df))
p_value1

#Q2(b) Conduct a hypothesis test for β2
beta2 <- 0.042
se2   <- 0.013
# Calculate t-value
t_value2 <- beta2 / se2
t_value2

# Criticalvalue is same here for identical degree of freedom and significance level of 0.05
t_critical

# Calculate p-value
p_value2 <- 2 * (1 - pt(abs(t_value2), df = df))
p_value2