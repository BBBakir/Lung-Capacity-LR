
library(readxl)
library(dplyr)
library(fma)
library(fpp)
library(qcc)
library(ggplot2)
library(nortest)
library(SciViews)

dataq<- read_excel("Data.xlsx")
dataq = data.frame(dataq)
names(dataq) = c("Participant","Smoke_Dur","Month_house","Gender","F_Mem_Smoke","Cf_Smoke","fWarn","Inform","Tv","Lung")




dataq$Gender <-as.numeric(as.factor(dataq$Gender))


family_friend_table <- table(dataq$F_Mem_Smoke, dataq$Cf_Smoke)

duration_gender_table <- table(dataq$Interval, dataq$sex)


chisq.test(family_friend_table)







dataq$Gender <-as.numeric(as.factor(dataq$Gender))


colors = c("#3AB0FF","#F24C4C","#000000","#F24C4C")
#### General model plotting and relations

#Fitting a linear regression model
Rmod<-lm(Lung ~Smoke_Dur+Month_house+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Inform+Tv,data=dataq)

#summary table
summary(Rmod)


anova(Rmod)
par(mfrow=c(2,2))
plot(Rmod)
anova(Rmod)
qcc(rstandard(Rmod), type = "xbar.one", plot = TRUE)




##Relations

plot(dataq$Smoke_Dur,dataq$Lung,
     xlab="Smoke Duration",ylab="Lung Capacity",main = "Smoke and Lung Capacity Plot",col="#3AB0FF",pch=17) 

plot(dataq$Month_house,dataq$Lung, main = "Relation Monthly Household Income and Lung Capacity ",xlab="Smoke Duration",ylab="Lung Capacity",col="#3AB0FF",pch=17)+abline(lm(formula=dataq$Lung~dataq$Month_house)) 

plot(dataq$Smoke_Dur,dataq$Month_house, xlab="Smoke Duration",ylab="Monthly Household Income",main = "Relation Monthly Household Income and Smoke Duration",col="#3AB0FF",pch=17)+abline(lm(formula=dataq$Lung~dataq$Smoke_Dur))+abline(lm(formula=dataq$Smoke_Dur~dataq$Month_house)) 


par(mfrow=c(2,2))
plot(dataq$Smoke_Dur,dataq$Lung,xlab="Smoke Duration",ylab="Lung Capacity",
     main = "Gender",
     col = ifelse(dataq$Gender==2,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$Gender==2, 17, 15))+
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$Gender==2 ,data=dataq), col = "#3AB0FF",lwd = 2) +
  abline(lm(formula=dataq$Lung~dataq$Smoke_Dur, subset =dataq$Gender==1,data=dataq), col = '#F24C4C',lwd = 2)+
  legend(6,1,legend=c("Male", "Female"),fill = c("#3AB0FF","#F24C4C"))

plot(dataq$Smoke_Dur,dataq$Lung,xlab="Smoke Duration",ylab="Lung Capacity",main = "Informed or not",
     col = ifelse(dataq$Inform==0,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$Inform==0, 17, 15))+
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$Inform==0 ,data=dataq), col = "#3AB0FF",lwd = 2) +
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$Inform==1 ,data=dataq), col = "#F24C4C",lwd = 2)+
  legend(6,1,legend=c("Yes", "No"),fill = c("#3AB0FF","#F24C4C"))

plot(dataq$Smoke_Dur,dataq$Lung,xlab="Smoke Duration",ylab="Lung Capacity",main = "Warned by family not",
     col = ifelse(dataq$fWarn==0,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$fWarn==0, 17, 15)) +
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$fWarn==0 ,data=dataq), col = "#3AB0FF",lwd = 2) +
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$fWarn==1 ,data=dataq), col = "#F24C4C",lwd = 2)+
  legend(6,1,legend=c("Yes", "No"),fill = c("#3AB0FF","#F24C4C"))

plot(dataq$Smoke_Dur,dataq$Lung,xlab="Smoke Duration",ylab="Lung Capacity",main = "Anti-tobacco Messages",
     col = ifelse(dataq$Tv==0,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$Tv==0, 17, 15))+
  
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$Tv==0 ,data=dataq), col = "#3AB0FF",lwd = 2) +
  abline(lm(dataq$Lung~dataq$Smoke_Dur, subset =dataq$Tv==1 ,data=dataq), col = "#F24C4C",lwd = 2)+
  legend(6,1,legend=c("Yes", "No"),fill = c("#3AB0FF","#F24C4C"))



ggplot(
  data = dataq,
  aes( x = Smoke_Dur,
       y =Lung,
       col=Cf_Smoke
  )
)+
  geom_point(
    
  )+
  geom_smooth(
    method = "lm"
  )+
  scale_color_manual(values = c("#3AB0FF","#F24C4C","#000000","#F24C4C"))+
  labs(x = "Smoke Duration",
       y = "Lung",
       color = " Number of Close friends smoked?")

ggplot(
  data = dataq,
  aes( x = Smoke_Dur,
       y =Lung,
       col=F_Mem_Smoke
  )
)+
  geom_point(
    
  )+
  geom_smooth(
    method = "lm"
  )+
  scale_color_manual(values = c("#3AB0FF","#F24C4C","#000000","#F24C4C"))+
  labs(x = "Smoke Duration",
       y = "Lung",
       color = " Number of Family members smoked?")

#####################################################################3333333333333
par(mfrow=c(2,2))
plot(dataq$Month_house,dataq$Lung,xlab="Montly Household Income",ylab="Lung Capacity",main = "Gender",
     col = ifelse(dataq$Gender==2,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$Gender==2, 17, 15)) +
  abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$Gender==2 ,data=dataq), col = "#3AB0FF",lwd = 2) +
  abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$Gender==1,data=dataq), col = '#F24C4C',lwd = 2)+
  legend(17500,1,legend=c("Male", "Female"),fill = c("#3AB0FF","#F24C4C"))

plot(dataq$Month_house,dataq$Lung,xlab="Montly Household Income",ylab="lung capacity",main = " Warned by family not",
     col = ifelse(dataq$Inform==0,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$Inform==0, 17, 15)) +
  abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$Inform==0 ,data=dataq), col = "#3AB0FF",lwd = 2)
abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$Inform==1 ,data=dataq), col = "#F24C4C",lwd = 2)+
  legend(17500,1,legend=c("Yes", "No"),fill = c("#3AB0FF","#F24C4C"))

plot(dataq$Month_house,dataq$Lung,xlab="Montly Household Income",ylab="lung capacity",main = "Warned by family not",
     col = ifelse(dataq$fWarn==0,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$fWarn==0, 17, 15)) +
  abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$fWarn==0 ,data=dataq), col = "#3AB0FF",lwd = 2)
abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$fWarn==1 ,data=dataq), col = "#F24C4C",lwd = 2)+
  legend(17500,1,legend=c("Yes", "No"),fill = c("#3AB0FF","#F24C4C"))

plot(dataq$Month_house,dataq$Lung,xlab="Montly Household Income",ylab="lung capacity",main = " Anti-tobacco Messages",
     col = ifelse(dataq$Tv==0,"#3AB0FF",'#F24C4C'),
     pch = ifelse(dataq$Tv==0, 17, 15)) +
  abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$Tv==0 ,data=dataq), col = "#3AB0FF",lwd = 2) +
  abline(lm(formula=dataq$Lung~dataq$Month_house, subset =dataq$Tv==1 ,data=dataq), col = "#F24C4C",lwd = 2)+
  legend(17500,1,legend=c("Yes", "No"),fill = c("#3AB0FF","#F24C4C"))


ggplot(
  data = dataq,
  aes( x = Month_house,
       y =Lung,
       col=Cf_Smoke
  )
)+
  geom_point(
    
  )+
  geom_smooth(
    method = "lm"
  )+
  theme_classic(
    
  )+
  scale_color_manual(values = c("#3AB0FF","#F24C4C","#000000","#F24C4C"))+
  labs(x = "Smoke Duration",
       y = "Lung",
       color = " Number of Close friends smoked?")


ggplot(
  data = dataq,
  aes( x = Month_house,
       y =Lung,
       col=F_Mem_Smoke
  )
)+
  geom_point(
    
  )+
  geom_smooth(
    method = "lm"
  )+
  theme_classic(
    
  )+
  scale_color_manual(values = c("#3AB0FF","#F24C4C","#000000","#F24C4C"))+
  labs(x = "Smoke Duration",
       y = "Lung",
       color = " Number of Family members smoked?")












dataq$Gender <-as.numeric(as.factor(dataq$Gender))


colors = c("#3AB0FF","#F24C4C","#000000","F24C4C")

#Fitting a linear regression model
Rmod<-lm(Lung ~Smoke_Dur+Month_house+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Inform+Tv,data=dataq)

#summary table
summary(Rmod)


anova(Rmod)
par(mfrow=c(2,2))
plot(Rmod)
anova(Rmod)
qcc(rstandard(Rmod), type = "xbar.one", plot = TRUE)



Rmod1<-lm(Lung ~Smoke_Dur+Month_house+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Inform+Tv+Gender*fWarn+Gender*Smoke_Dur,data=dataq)
summary(Rmod1)
anova(Rmod1)
res1 <- residuals(Rmod1)
res1



sres1 <- rstandard(Rmod1)
sres1
par(mfrow=c(2,2))
plot(Rmod1)
ad.test(rstandard(Rmod1))
hist(rstandard(Rmod1), main = "Histogram", xlab = "Residuals Standardized", ylab = "Frequency",col="#F24C4C")




## Backward Elimination

## 1
Rmod2<-lm(Lung ~Smoke_Dur+Month_house+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Inform+Tv+Gender*fWarn+Gender*Smoke_Dur,data=dataq)

summary(Rmod2)
## 2
Rmod3<-lm(Lung ~Smoke_Dur+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Inform+Tv+Gender*fWarn+Gender*Smoke_Dur,data=dataq)

summary(Rmod3)
## 3
Rmod4<-lm(Lung ~Smoke_Dur+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Tv+Gender*fWarn+Gender*Smoke_Dur,data=dataq)

summary(Rmod4)
## 4
Rmod5<-lm(Lung ~Smoke_Dur+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Gender*fWarn+Gender*Smoke_Dur,data=dataq)

summary(Rmod5)
## 5
Rmod6<-lm(Lung ~Smoke_Dur+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Gender*Smoke_Dur,data=dataq)

summary(Rmod6)




anova(Rmod6)
par(mfrow=c(2,2))
plot(Rmod6)
anova(Rmod6)



finalmod<-lm(sqrt(Lung) ~Smoke_Dur+Gender+F_Mem_Smoke+Cf_Smoke+fWarn+Gender*Smoke_Dur,data=dataq)
summary(finalmod)
par(mfrow=c(2,2))
plot(finalmod)
anova(finalmod)



plot(dataq$Participant,
     rstandard(finalmod),
     main = "Scatter Plot",
     xlab = "Observations",
     ylab = "Standardized Residuals",
     pch = 1)
hist(rstandard(finalmod), main = "Histogram", xlab = "Residuals Standardized", ylab = "Frequency",col="#3AB0FF")


 
dataforq3=data.frame(Smoke_Dur=1.65, Gender=1, F_Mem_Smoke="One", Cf_Smoke="Two", fWarn=1, Inform=0, Tv=1)
predict(finalmod, dataforq3, interval="confidence",level = 0.95) ^2

dataforq4=data.frame(Smoke_Dur=3.35, Gender=2, F_Mem_Smoke="Two", Cf_Smoke="More than 3", fWarn=0, Inform=1, Tv=1)
predict(finalmod, dataforq4, interval = 'prediction',level = 0.95) ^2

