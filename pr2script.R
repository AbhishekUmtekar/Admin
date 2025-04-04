heights<-c(63 , 63, 66, 67, 68, 69, 70, 70, 71, 71)
t.test(x=heights,mu=66)
t.test(x=mtcars$mpg,mu=20)
da<-c(10,12,13,11,14)
db<-c(8,9,12,14,15,10,9)
t.test(da,db,paired=FALSE)
before<-c(53,28,31,48,50,42)
after<-c(58,29,30,55,56,45)
t.test(before,after,paired=TRUE)
library(MASS)
d1=table(survey$Smoke,survey$Exer)
chisq.test(d1)
d2=table(mtcars$cyl,mtcars$carb)
chisq.test(d2)
data=matrix(c(8,10,12,178,21,21),nrow = 2,ncol = 3,byrow=TRUE)
colnames(data)=c("0","1","2")
rownames(data)=c("Left","Right")
chisq.test(data)
