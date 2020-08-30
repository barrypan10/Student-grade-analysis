d1=read.table("student-mat.csv",sep=",",header=TRUE)



x1<-lm(G1 ~ studytime+guardian+school+sex+age+address+famsize+Pstatus+Medu+Fedu+Mjob+Fjob+reason+traveltime++failures+schoolsup+famsup+paid+activities+nursery+higher+internet+romantic+famrel+freetime+goout+Dalc+Walc+health+absences, data=d1)
summary(x1)
plot(x1)
x<-(d1$G1+d1$G2+d1$G3)/3

summary(x2)
plot(x2)
barplot(table(d1$age))

x<-(d1$G1+d1$G2+d1$G3)/3

barplot(table(d1$G3>=15))
barplot(prop.table(table(d1$G3>=11,d1$studytime),2),ylab='percentage',xlab='Time spent studying',main='Amount of students getting above 11/20',col=1:2)

barplot(prop.table(table(d1$G3>=11,d1$schoolsup),2),ylab='percentage',xlab='Educational support from school',main='Amount of students getting above 11/20',col=1:2,legend.text = rownames(table(d1$G3>=11,d1$schoolsup)))

x3<-glm(x~studytime+sex+failures+schoolsup+goout,data=d1)