library(kernlab)#for svm algorithms
library(dplyr)
library(mice)#for imputations
library(e1071)#for skewness
library(ggplot2)
getwd()
train=read.csv('DengAI_Predicting_Disease_Spread_-_Training_Data_Features.csv')
train$week_start_date = as.Date(train$week_start_date, "%Y-%m-%d");#coverting char to date format
str(train)
test=read.csv('DengAI_Predicting_Disease_Spread_-_Test_Data_Features.csv')
test$week_start_date = as.Date(test$week_start_date, "%Y-%m-%d");
train2=read.csv('DengAI_Predicting_Disease_Spread_-_Training_Data_Labels.csv')
train$city=as.factor(train$city)#converting city names as factors
test$city=as.factor(test$city)
train$weekofyear=as.factor(train$weekofyear)#converting city names as factors
test$weekofyear=as.factor(test$weekofyear)#converting city names as factors
train1=train#creating dummy data frame
train3=train

#around 21 columns are numerical and continous
#cleaning train
colnames(train)
sum(is.na(train))#there are 548 missing values

summary(train1)
#since there are two cities named sj and iq we are splitting into separate values using dplyr
train1.sj=train1%>%filter(city == 'sj')
train1.iq=train1%>%filter(city == 'iq')

##pre-processing##

#treating outliers 
boxplot(train1.sj[,-c(1,2,3,4)])#as 1st 4 columns are not continous so they are not present while plotting boxplot
boxplot(train1.iq[,-c(1,2,3,4)])
outlier.treat=function(y){ #substituting benchmark values for which they arre outliers using Inter quartile (user-defined)
  q1=summary(y)[2]
  q3=summary(y)[5]
  e1=bench=q1 - (1.5*IQR(y,na.rm = TRUE))
  e2=bench=q3 + (1.5*IQR(y,na.rm = TRUE))
  y[y<e1]=q1
  y[y>e2]=q3
  return(y)
}
i=5
while(i<25){#for all the rows expect rows(1,2,3,4) applying outlier.treat function
  if(is.factor(train1.sj) && is.factor(train1.iq)){}
  else{  
    train1.sj[,i] = outlier.treat(train1.sj[,i])
    train1.iq[,i] = outlier.treat(train1.iq[,i])
  }
  i=i+1
}
boxplot(train1.sj[,-c(1,2,3,4)])#outlier are replaced with benchmark values
boxplot(train1.iq[,-c(1,2,3,4)])
colSums(is.na(train1))

#verifying skewness for numerical columns
skewness(train1.sj$ndvi_ne,na.rm = TRUE)
skewness(train1.sj$ndvi_nw,na.rm = TRUE)
skewness(train1.sj$ndvi_se,na.rm = TRUE)
skewness(train1.sj$ndvi_sw,na.rm = TRUE)
skewness(train1.sj$precipitation_amt_mm,na.rm = TRUE)#right skewed
skewness(train1.sj$reanalysis_air_temp_k,na.rm = TRUE)
skewness(train1.sj$reanalysis_avg_temp_k,na.rm = TRUE)
skewness(train1.sj$reanalysis_dew_point_temp_k,na.rm = TRUE)#left skewed
skewness(train1.sj$reanalysis_max_air_temp_k,na.rm = TRUE)
skewness(train1.sj$reanalysis_min_air_temp_k,na.rm = TRUE)#left skewed
skewness(train1.sj$reanalysis_precip_amt_kg_per_m2,na.rm = TRUE)#right skewed
skewness(train1.sj$reanalysis_relative_humidity_percent,na.rm = TRUE)
skewness(train1.sj$reanalysis_sat_precip_amt_mm,na.rm = TRUE)#right skewed
skewness(train1.sj$reanalysis_specific_humidity_g_per_kg,na.rm = TRUE)#left skewed
skewness(train1.sj$reanalysis_tdtr_k,na.rm = TRUE)#right skewed
skewness(train1.sj$station_avg_temp_c,na.rm = TRUE)
skewness(train1.sj$station_diur_temp_rng_c,na.rm = TRUE)
skewness(train1.sj$station_max_temp_c,na.rm = TRUE)
skewness(train1.sj$station_min_temp_c,na.rm = TRUE)
skewness(train1.sj$station_precip_mm,na.rm = TRUE)#right skewed

#reducing skewness for city=sj
skewness(sqrt(train1.sj$precipitation_amt_mm),na.rm = TRUE)
train1.sj$precipitation_amt_mm=sqrt(train1.sj$precipitation_amt_mm)
skewness((train1.sj$reanalysis_dew_point_temp_k)^7,na.rm = TRUE)
#train1.sj$reanalysis_dew_point_temp_k=(train1.sj$reanalysis_dew_point_temp_k)^7
skewness(train1.sj$reanalysis_min_air_temp_k,na.rm = TRUE)
skewness((train1.sj$reanalysis_min_air_temp_k)^9,na.rm = TRUE)
#train1.sj$reanalysis_dew_point_temp_k=(train1.sj$reanalysis_min_air_temp_k)^9
skewness(sqrt(train1.sj$reanalysis_precip_amt_kg_per_m2),na.rm = TRUE)
train1.sj$reanalysis_precip_amt_kg_per_m2=sqrt(train1.sj$reanalysis_precip_amt_kg_per_m2)
skewness(train1.sj$reanalysis_sat_precip_amt_mm,na.rm = TRUE)
skewness(sqrt(train1.sj$reanalysis_sat_precip_amt_mm),na.rm = TRUE)
train1.sj$reanalysis_sat_precip_amt_mm=sqrt(train1.sj$reanalysis_sat_precip_amt_mm)
skewness(train1.sj$reanalysis_specific_humidity_g_per_kg,na.rm = TRUE)
skewness((train1.sj$reanalysis_specific_humidity_g_per_kg)^3,na.rm = TRUE)
train1.sj$reanalysis_specific_humidity_g_per_kg=(train1.sj$reanalysis_specific_humidity_g_per_kg)^3
skewness(train1.sj$reanalysis_tdtr_k,na.rm = TRUE)
skewness((train1.sj$reanalysis_tdtr_k)^(1/3),na.rm = TRUE)
train1.sj$reanalysis_tdtr_k=(train1.sj$reanalysis_tdtr_k)^(1/3)
skewness(train1.sj$station_precip_mm,na.rm = TRUE)
skewness(sqrt(train1.sj$station_precip_mm),na.rm = TRUE)
train1.sj$station_precip_mm=sqrt(train1.sj$station_precip_mm)

#city = iq

skewness(train1.iq$ndvi_ne,na.rm = TRUE)
skewness(train1.iq$ndvi_nw,na.rm = TRUE)
skewness(train1.iq$ndvi_se,na.rm = TRUE)
skewness(train1.iq$ndvi_sw,na.rm = TRUE)
skewness(train1.iq$precipitation_amt_mm,na.rm = TRUE)
skewness(train1.iq$reanalysis_air_temp_k,na.rm = TRUE)
skewness(train1.iq$reanalysis_avg_temp_k,na.rm = TRUE)
skewness(train1.iq$reanalysis_dew_point_temp_k,na.rm = TRUE)#left skewed
skewness(train1.iq$reanalysis_max_air_temp_k,na.rm = TRUE)
skewness(train1.iq$reanalysis_min_air_temp_k,na.rm = TRUE)#left skewed
skewness(train1.iq$reanalysis_precip_amt_kg_per_m2,na.rm = TRUE)#right skewed
skewness(train1.iq$reanalysis_relative_humidity_percent,na.rm = TRUE)#left skewed
skewness(train1.iq$reanalysis_sat_precip_amt_mm,na.rm = TRUE)
skewness(train1.iq$reanalysis_specific_humidity_g_per_kg,na.rm = TRUE)#left skewed
skewness(train1.iq$reanalysis_tdtr_k,na.rm = TRUE)
skewness(train1.iq$station_avg_temp_c,na.rm = TRUE)
skewness(train1.iq$station_diur_temp_rng_c,na.rm = TRUE)
skewness(train1.iq$station_max_temp_c,na.rm = TRUE)
skewness(train1.iq$station_min_temp_c,na.rm = TRUE)
skewness(train1.iq$station_precip_mm,na.rm = TRUE)#right skewed

#imputing

#sj

summary(train1.sj)
impute=mice(train1.sj[,1:24],m=4,seed=123)#using mice package we are imputing the data by 4 iterations
stripplot(impute)#these 4 iterations can be plotted using stripplot and selecting the suitable values by seeing how much data(redline) is fitted into original(blue line)
sj=complete(impute,4)#imputing 4th iteration because the missing values of ndvi_ne column are somewhat correctly fitted than others
colSums(is.na(sj))#after imputing there are still some NA values in 17th column 
while(i<dim(sj)[1]){#so we imputed the 17th column by mean of the column
  if(is.na(sj[i,17])){
    sj[i,17]=mean(sj[,17],na.rm = TRUE)
  }
  i=i+1
}
#above steps are also followed by city = iq
#iq

summary(train1.iq)
impute=mice(train1.iq[,1:24],m=4,seed=123)
stripplot(impute)
iq=complete(impute,4)
colSums(is.na(iq))#there are 4 values having NA still after imputing 
i=1
while(i<dim(iq)[1]){#imputing 17th column by mean of the column
  if(is.na(iq[i,17])){
    iq[i,17]=mean(iq[,17],na.rm = TRUE)
  }
  i=i+1
}
#combining iq and sj
imputedtrain=rbind(sj,iq)
imputedtrain$total_cases=train2$total_cases#appending the target column from the train_labels dataset
colSums(is.na(imputedtrain))#no missing values data is fully imputed


##ML Models and Predictions

##ML without preprocessing
train3$total_cases=train2$total_cases
train3$weekofyear=as.factor(train3$weekofyear)
train3[is.na(train3)]=0
#linear
model=lm(data = train3,total_cases~.)
options(max.print = 999999999)
summary(model)
#r-square value for this model is 0.24
pred=round(predict(model,train3))
mean_error=mean(abs(pred-train3$total_cases))
mean_error# mean_error=20.31731

#Support Vector Regression
model=ksvm(total_cases~.,data=train3, kernel="rbfdot",kpar="automatic",C=0.6,cross=3,prob.model=TRUE,seed=142)

pred=round(predict(model,train3))
mean_error=mean(abs(pred-train3$total_cases))
mean_error#mean_error=13.8
#ML1 : Linear Regression Model
model1=lm(data = imputedtrain,total_cases~.)
options(max.print = 999999999)#this allows as to print every column summary without omitting
summary(model1)
#r-square value for model1 is 0.2047

pred1=round(predict(model1,imputedtrain))
mean_error1=mean(abs(pred1-imputedtrain$total_cases))
mean_error1# mean_error1=19.317

#selecting columns for higher significance from the summary of model1
model2=lm(data = imputedtrain,total_cases~ndvi_ne+ndvi_nw+station_diur_temp_rng_c+station_max_temp_c+weekofyear)
options(max.print = 999999999)
summary(model2)
#r-square value for model1 is 0.1572

pred2=round(predict(model2,imputedtrain))
mean_error2=mean(abs(pred1-imputedtrain$total_cases))
mean_error2# mean_error2=19.31731

#linear regression didn't performed accurately its r-square is very low and the mean_error is very high

#ML2 : Support Vector Regression

model3=ksvm(total_cases~.,data=imputedtrain, kernel="rbfdot",kpar="automatic",C=0.6,cross=3,fit=TRUE,prob.model=TRUE)
pred3=round(predict(model3,imputedtrain))

mean_error3=mean(abs(pred3-imputedtrain$total_cases))
mean_error3#mean_error3=14.11  error is somewhat decreased
#fit=TRUE implies that to include the fitted values also
#we kept verifying this model by the changing values of C and cross parameters in the model
#cross implies the cross-validation tests performed on training data
#C implies a parameter for controlling the cross validation errors

model4=ksvm(total_cases~.,data=imputedtrain, kernel="vanilladot",kpar="automatic",C=1,cross=3,fit=TRUE,prob.model=TRUE)
pred4=round(predict(model4,imputedtrain))

mean_error4=mean(abs(pred4-imputedtrain$total_cases))
mean_error4#mean_error3=16.02 using vanilladot as function

#kernel="vanilladot" is a function for training and testing by using vanilladot as kernel we got error as 16.02
#but using rbfdot we got error in 13.2 so we are using rbfdot function rbf= Radial Basis Function and vanilladot is linear function

model5=ksvm(total_cases~.,data=imputedtrain, kernel="rbfdot",kpar="automatic",C=43,cross=43,fit=TRUE,prob.model=TRUE,seed=142)
pred5=round(predict(model5,imputedtrain))

mean_error5=mean(abs(pred5-imputedtrain$total_cases))
mean_error5#mean_error5=7.8


#model5 is best among the tested models, so it is chosen for predictions of test dataset

#######
#test
sum(is.na(test))#119 missing values in test dataset
summary(test)

#filtering data based on cities
test.sj=test%>%filter(city == 'sj')
test.iq=test%>%filter(city == 'iq')

#imputing
#sj

summary(test.sj)
impute=mice(test.sj[,1:24],m=4,seed=123)
stripplot(impute)
sjt=complete(impute,4)
colSums(is.na(sjt))
i=1
while(i<dim(sjt)[1]){#imputing left-over values through mean
  if(is.na(sjt[i,17])){
    sjt[i,17]=mean(sjt[,17],na.rm = TRUE)
  }
  i=i+1
}

#iq

summary(test.iq)
impute=mice(test.iq[,1:24],m=4,seed=123)
stripplot(impute)
iqt=complete(impute,4)
colSums(is.na(iqt))

#combining data sets
imputedtest=rbind(sjt,iqt)

#verifying whether both test and train having same data types
str(imputedtest)
str(imputedtrain)
pred=predict(model5,imputedtest)
imputedtest$total_cases=round(abs(pred))

#creating submission file as mentioned in submission_format data set
submission=cbind.data.frame(imputedtest$city, imputedtest$year, imputedtest$weekofyear,imputedtest$total_cases)
colnames(submission)=c('city','year','weekofyear','total_cases')#changing column names
print(submission)
path='C:\\Users\\Adarsha\\Desktop\\submission.csv'
write.csv(x=submission, file = path, row.names = FALSE)

##done by
#Name: Y.Adarsha