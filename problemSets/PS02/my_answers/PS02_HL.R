library(tidyverse)
# Q1(a): calculate x^2 by hand
# Build contingency table shown in Q1
study_data <- matrix(c(14, 6, 7,
                       7, 7, 1),
                       nrow = 2,byrow = TRUE)

colnames(study_data) <- c("Not stopped", "Bribe requested", "Stopped Warning")
rownames(study_data) <- c("Upper calss", "Lower calss")

# Check data 
study_data

# Calculate expected frequency for each cell
row_total <- rowSums(study_data)
col_total <- colSums(study_data)
grand_total <- sum(study_data)
expected_fre <- outer(row_total, col_total) / grand_total

# Calculate Chi-square t-statistic manually
chi_sq <- sum((study_data - expected_fre)^2 / expected_fre)
chi_sq

# Q1(b):calculate the p-value 
df = (nrow(study_data)-1)*(ncol(study_data)-1)
pchisq(chi_sq, df, lower.tail = FALSE)

#Q1(c):calculate the standardized residuals for each cell
z <- (study_data - expected_fre) / sqrt(expected_fre * (1 - row_total/grand_total) * (1 - col_total/grand_total))
z

#Q2(b):run a bivariate regression to test this hypothesis
# Read the data to be analyzed
data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
# view data and grasp column names
summary(data)
# Transform reserved to binary variable
data$reserved <- factor(data$reserved,
                        levels = c(0, 1),
                        labels = c("NotReserved", "Reserved"))

# Build regression model and check the outcome
model <- lm(data$water ~ data$reserved)
summary(model)

library(stargazer)
stargazer(model, type = "latex", title = "Regression Results", 
          dep.var.labels = "Number of Drinking Water Facilities",
          covariate.labels = c("Intercept", "Reserved (Female)"))