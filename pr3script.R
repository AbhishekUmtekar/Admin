#For FTest
data<-read.csv(file.choose(),sep=",",header = T)
var.test(data$A,data$B)

#For ANOVA
# Assuming `data` already has columns A, B, and C
data<-read.csv(file.choose(),sep=",",header = T)
d1 <- data.frame(
  satindex = c(data$A, data$B, data$C),  # Combine the satisfaction scores
  dept = factor(c(rep("A", length(data$A)),   # Repeat 'A' for the number of A's
                  rep("B", length(data$B)),   # Repeat 'B' for the number of B's
                  rep("C", length(data$C)))   # Repeat 'C' for the number of C's
  )
)
d1
data<-read.csv(file.choose(),sep=",",header = T)
model<-aov(data$C ~ data$A+data$B+data$A*data$B,data=data)
summary(model)
nd<-data.frame(d1$A="M",d1$B="School")
predict(model,data=nd)

d1<-read.csv(file.choose(),header=TRUE,sep=",")
var.test(d1$A,d1$B)
aov(d1$C~d1$A+d1$B,data=d1)
