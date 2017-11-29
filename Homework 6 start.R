


setwd("~/Desktop/Data")
df <- read.csv("salary.txt",header=T)
names(df)


# Construct training and test datasets
# Use sample 
# Set seed to 0 
set.seed(0)
index <- sample(1:nrow(df),4965,replace = F)
train.data <- df[-index,]
data <- train.data 
test.data <- df[index,]


# Quality control check
sum(train.data$race=="black")/nrow(data)
sum(test.data$race=="black")/nrow(test.data)  


# Rough model 1 
r.model.1 <- lm(wage~edu+exp+city+reg+race+deg+com,data=train.data) 
summary(r.model.1)
qqnorm(rstudent(r.model.1))
qqline(rstudent(r.model.1))


# Rough model 2 
r.model.2 <- lm(log(wage)~edu+exp+city+reg+race+deg+com,data=train.data)
summary(r.model.2)
qqnorm(rstudent(r.model.2))
qqline(rstudent(r.model.2))


############################################
# EDA (Use smootehrs)
############################################

##### com
plot(data$com,log(data$wage))
lines(supsmu(data$com,log(data$wage)),col=2)

##### edu
plot(data$edu,log(data$wage))
lines(supsmu(data$edu,log(data$wage)),col=2)

##### edu
boxplot(log(data$wage)~data$edu)
lines(supsmu(data$edu,log(data$wage)),col=2)


############################################
#### EDA (interaction plots)
############################################


#################
# City vs. region
#################
city <- data$city
reg <- data$reg
wage <- data$wage
interaction.plot(city,reg,log(wage))

##############
# Race vs. edu
##############
plot(data$edu,log(wage),col=data$race)
plot(data$edu,log(wage),col="lightgrey")

black <- data$race=="black"
white <- data$race=="white"
other <- data$race=="other"

# smoother
plot(data$edu,log(wage),col="lightgrey")
lines(supsmu(data$edu[black],log(data$wage)[black]),col=2)
lines(supsmu(data$edu[white],log(data$wage)[white]),col=3)
lines(supsmu(data$edu[other],log(data$wage)[other]),col=4)
legend("topright",legend=c("Black","White","Other"),col=c(2,3,4),lty=c(1,1,1))

# lines
plot(data$edu,log(wage),col="lightgrey")
abline(lm(log(data$wage)[black]~data$edu[black]),col=2)
abline(lm(log(data$wage)[white]~data$edu[white]),col=3)
abline(lm(log(data$wage)[other]~data$edu[other]),col=4)
legend("topright",legend=c("Black","White","Other"),col=c(2,3,4),lty=c(1,1,1))

##################################
#  Is the interaction significant?
##################################

r.model.3 <- lm(log(wage)~edu+edu*race+exp+city+reg+race+deg+com,data=train.data)
summary(r.model.3)

# AIC 
AIC(r.model.2)
AIC(r.model.3)





