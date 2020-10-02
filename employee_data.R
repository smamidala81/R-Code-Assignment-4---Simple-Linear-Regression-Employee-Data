# Assignement 4, Q 3) Emp_data -> Build a prediction model for Churn_out_rate.
# Simple Linear Regression
employee_data<-read.csv(file.choose())
View(employee_data)

# Visualization
install.packages("lattice")
library(lattice)
dotplot(employee_data$Churn_out_rate, main="Dot Plot of Churn Out Rate",col="dodgerblue4")
dotplot(employee_data$Salary_hike, main="Dot Plot of Salary Hike", col="dodgerblue4")
boxplot(employee_data$Churn_out_rate,col="dodgerblue4",horizontal = T)
boxplot(employee_data$Salary_hike,col="red",horizontal = T)

attach(employee_data)
hist(Churn_out_rate,probability = T)
hist(Salary_hike,probability = T)

library(moments)
skewness(employee_data)
kurtosis(employee_data)

#scatter plot
plot(Salary_hike,Churn_out_rate)

qqnorm(Churn_out_rate)
qqline(Churn_out_rate)

qqnorm(Salary_hike)
qqline(Salary_hike)

cor(Salary_hike,Churn_out_rate)

#Regression equation

emp_model<- lm(Churn_out_rate~Salary_hike,data =employee_data)
summary(emp_model)

sum(emp_model$residuals^2)/nrow(employee_data) ## RMSE

emp_model$coefficients
emp_model$residuals

# Transformation
emp_model2<- lm(Churn_out_rate~sqrt(Salary_hike),data =employee_data)
summary(emp_model2)
sqrt(sum(emp_model2$residuals^2)/nrow(employee_data)) ## RMSE

emp_model3<- lm(Churn_out_rate~log(Salary_hike),data =employee_data)
summary(emp_model3)
log(sum(emp_model3$residuals^2)/nrow(employee_data)) ## RMSE

emp_model4<- lm(sqrt(Churn_out_rate)~Salary_hike,data =employee_data)
summary(emp_model4)
sqrt(sum(emp_model4$residuals^2)/nrow(employee_data)) ## RMSE

emp_model5<- lm(log(Churn_out_rate)~Salary_hike,data =employee_data)
summary(emp_model5)
log(sum(emp_model5$residuals^2)/nrow(employee_data)) ## RMSE

colnames(employee_data)

cor(log(Churn_out_rate),Salary_hike)
plot(log(Churn_out_rate),Salary_hike)

confint(emp_model5,level = 0.95)
predict(emp_model5,interval = 'predict')

emp_model5$coefficients

6.6383000+(-0.0013963*2500)

Salary_h_new=data.frame(Salary_hike=2500)
churnrate_new=predict(emp_model5,newdata = Salary_h_new)
churnrate_new

###########
pred<-predict(emp_model5)
pred
pred<-predict(emp_model5)
final_empldata<-data.frame(employee_data,pred,"Error"= employee_data$Churn_out_rate-pred)
View(final_empldata)
