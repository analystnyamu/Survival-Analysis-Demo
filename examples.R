library(survival)
mydata<-lung
mydata
# Recording new variable
recordstatus <- function(x){
  if (x==1){rs=0} # no event /censored
  if (x==2){rs=1} # event happened
  return(rs)
}
for(i in 1:length(mydata$status)){
  mydata$recordstatus[i]<-recordstatus(mydata$status[i])
}

# creating the survival object
mysurv<-Surv(time=mydata$time, event=mydata$recordstatus)
mysurv

# fitting the survival curve
myfit<-survfit(mysurv~1)
plot(myfit)
plot(myfit,conf.int="none")
abline(h=0.5) # locating the median using horizontal and vertical line
abline(v=310)

# specify predictor variable in the formula
myfit1<-survfit(mysurv~mydata$sex)
myfit1
plot(myfit1)
table(mydata$sex)
plot(myfit1,col=c("red","blue")) # red=male, blue=female
plot(myfit1,conf.int="both",col=c("red","blue"))
plot(myfit1,col=c("red","blue"), mark=3) # mark.time=T markeed at each censoring time
legend("topright",c("male","female"), col=c("red","blue"), lty=1)
abline(h=0.5); abline(v=270, col="blue"); abline(v=426, col="red")

# now we see that survival of females is better
# Q: is it better by chance, or statistically significant?
survdiff(mysurv~mydata$sex)
# plot the inverse of a survival function
plot(myfit1, fun="event", col=c("red", "blue"), mark=3)

# cox proportional model
mymod1<-coxph(mysurv~mydata$sex+mydata$age)
summary(mymod1)
names(mymod1)
exp(mymod1$coefficients)




####################################################################
# Loglinear models
# using array to input data
# create a table with 2 rows, 2 columns, and 2 layers
seniors <- array(data = c(911, 44, 538, 456, 3, 2, 43, 279), 
                 dim = c(2,2,2), 
                 dimnames = list("cigarette" = c("yes","no"),
                                 "marijuana" = c("yes","no"),
                                 "alcohol" = c("yes","no")))
seniors
# flatten the table
ftable(seniors, row.vars = c("alcohol","cigarette"))
# The addmargins function provides marginal totals
addmargins(seniors)

#The prop.table function allows us to calculate cell proportions
#Below we calculate proportions across the columns along the rows for each layer
prop.table(seniors, margin = c(1,3))
prop.table(seniors, margin = c(2,3))

# converting data into a data.frame and setting a reference variable
seniors.df <- as.data.frame(as.table(seniors))
seniors.df[,-4] <- lapply(seniors.df[,-4], relevel, ref = "no")
seniors.df
# fitting model
mod0 <- glm(Freq ~ cigarette + marijuana + alcohol, 
            data = seniors.df, family = poisson)
summary(mod0)
# checking signficance of teh model
pchisq(deviance(mod0), df = df.residual(mod0), lower.tail = F)

# odds ratio of marijuana
exp(coef(mod0)[3])

# manual calcualtion of odds ratio
margin.table(seniors, margin = 2)/sum(margin.table(seniors, margin = 2))

# homogineous association
mod1 <- glm(Freq ~ (cigarette + marijuana + alcohol)^2, 
            data = seniors.df, family = poisson)
summary(mod1)

# model fit
pchisq(deviance(mod1), df = df.residual(mod1), lower.tail = F)

# compare 
cbind(mod1$data, fitted(mod1))

## odds ratio of marijuana:alcoho

exp(coef(mod1)["cigaretteyes:marijuanayes"])
exp(coef(mod1)["cigaretteyes:alcoholyes"])


# confidence intervals
exp(confint(mod1, parm = c("cigaretteyes:marijuanayes",
                           "cigaretteyes:alcoholyes",
                           "marijuanayes:alcoholyes")))
# saturated model
mod2 <- glm(Freq ~ cigarette * marijuana * alcohol, 
            data = seniors.df, family = poisson)

summary(mod2)

# deviance and fitted values
deviance(mod2)

cbind(mod2$data, fitted(mod2))

pchisq(deviance(mod2), df=df.residual(mod2), lower.tail=F)
# comparing between the two models
anova(mod1, mod2)
pchisq(0.37399, 1, lower.tail = F)







# Before we get started, let's think about why we call it "loglinear".
# Let's say I flip a quarter 40 times
set.seed(1)
qtr <- sample(c("H","T"),40,replace = T)
nik <- sample(c("H","T"),40,replace = T)
table(qtr,nik)
