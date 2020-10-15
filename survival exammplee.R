library(survival)
surTime<-c(3,4,4,6,7,9,15,21,23,25)
event<-c(1,1,0,0,1,0,1,1,0,1)
as.factor(event)
survtime<-data.frame(surTime, event)
mysurv1<-Surv(survtime$surTime, survtime$event)
mysurv1


myfit<-survfit(mysurv1~1)
summary(myfit)

{plot(myfit, conf.int="none", ylab = "Survival Probability", xlab = "Time (years)", main="Kaplan-Meier Survival Curve (Survival Function)")
  abline(h=0.5, lty=3, col = "red") # locating the median using horizontal and vertical line
  abline(v=15, lty=3, col="blue")}

tumor<-c(2,3,6,7,10,5,32,32,30,27,15,16,16)
event2<-c(1,0,1,0,1,1,1,1,1,1,0,1,1)
as.factor(event2)
tumord<-data.frame(tumor, event2)

mysurv2<-Surv(tumord$tumor, tumord$event2)
mysurv2

myfit2<-survfit(mysurv2~1)
summary(myfit2)

{plot(myfit2, conf.int="none", ylab = "Survival Probability", xlab = "Time (months)", main="Kaplan-Meier Survival Curve")
  abline(h=0.5, lty=3) # locating the median using horizontal and vertical line
  abline(v=16, lty=3, col="blue")
  abline(h=0.752, lty=3)
  abline(v=7, lty=3, col="red")}

