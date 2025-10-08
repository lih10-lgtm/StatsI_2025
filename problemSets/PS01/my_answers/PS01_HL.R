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

library(graphics)
library(ggplot2)
lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# calculate sample size
n<-length(y)
n 

# since sample size <30,using t score for small sample
t90 <- qt((1-0.9)/2, df=n-1, lower.tail = FALSE)

# calcluate sample mean and standard deviation
y_mean <- mean(y)
y_mean
y_sd <- sd(y)

# calculate the upper and lower side of confidence interval 
upper_90 <- y_mean + t90 * (y_sd/sqrt(n))
lower_90 <- y_mean - t90 * (y_sd/sqrt(n))

# build the 90% confidence interval
confint90 <- c(lower_90,upper_90)
confint90   
# 1.The 90% confidence interval is (93.95993,102.92007) ; sample mean(98.44)

# conduct one-sample and one-tailed t-test under the confidence level of 95%
# null hypothesis:y_mean <= 100
# alternative hypothesis:y_mean > 100
t.test(y, mu = 100, conf.level = 0.95, alternative = 'greater')
# the outcome shows : t = -0.59574, df = 24, p-value = 0.7215 
# alternative hypothesis: true mean is greater than 100
# t value is rather close to 0,indicating there is no apparent difference between the observed mean and 100
# and p-value is obviously greater than 0.05,which means the null hypothesis can't be rejected,in other words,the average student IQ in the school can't be seen as higher than the average IQ score (100) among all the schools in the country.


#####################
# Problem 2
#####################

#import the data frame to be analysed
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)

# view and grasp data structure
str(expenditure)

# display relationships among Y,X1,X2,X3 in a plot as a whole
pdf("plot_all.pdf")
pairs(expenditure[,c('Y','X1','X2','X3')])
dev.off()
# explore correlations between Y,X1,X2 and X3
cor(expenditure[, c("Y", "X1", "X2", "X3")])

# the plot_all and correlation outcome shows:
# a moderate and positive correlation between per capita expenditure on shelters/housing assistance (Y) and per capita personal income (X1), with a correlation coefficient of approximately 0.53.
# Y also shows moderate positive associations with both financial insecurity (X2, r ≈ 0.45) and urban population density (X3, r ≈ 0.46).
# Among the independent variables, per capita personal income (X1) and urban population density (X3) exhibited a strong positive correlation (r ≈ 0.60), while the relationships involving financial insecurity (X2) and the other predictors were found to be weaker.


#factor region becasuse region here is a categorical variable
class(expenditure$Region)
expenditure$Region <- factor(expenditure$Region,
                             levels = c(1:4),
                             labels = c("Northwest","North Central","South","West"))

#plot the relationship between Y and Region
pdf("plot_Y_RG.pdf")
boxplot(expenditure$Y ~ expenditure$Region,data = expenditure,
        xlab = "Region",
        ylab = "Expenditure On Assistance Per Capita")
dev.off()
# the boxplot(plot_Y_RG) shows averagely,west region has the highest per capita expenditure on housing assistance.


# create scatter plot of Y and X1
pdf("plot_Y_X1.pdf")
plot(expenditure$X1,expenditure$Y,
     xlab = "Personl Income Per Captia",
     ylab = "Expenditure On Assistance Per Capita")
     abline(lm(Y ~ X1, data = expenditure), col = "grey", lwd = 1)
dev.off()
# the scatter plot(plot_Y_X1) indicates that per capita expenditure (Y) is positively associated with per capita personal income (X1),which means as the state's per capita personal income increases, shelters/housing assistance spending per captia grows as well.

# plot relationship between Y and X1 based on Region 
# and display different regions with different types of symbols and colors
pdf("plot_Y_X1_byRG.pdf")
ggplot(expenditure, aes(x = X1, y = Y, color = Region, shape = Region)) +  
  geom_point()+
  geom_smooth(aes(group = 1),method = "lm", se = FALSE, linetype = "solid",col= "grey") +
  xlab("Personl Income Per Captia") +
  ylab("Expenditure On Assistance Per Capita")
dev.off()
