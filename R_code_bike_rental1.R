rm(list = ls())
library(ggplot2 )
library(grid)
library("tidyverse")
install.packages("randomForest")
library('randomForest')
install.packages("rpart")
library(rpart)
install.packages('rpart.plot')
library(rpart.plot)
library(randomForest)
install.packages("caret")
library("caret")
install.packages("DMwR")
library(DMwR)
setwd("D:\\edwisor\\bike_rental")
#######Reading CSV file#######################
day=read.csv("bikee.csv")
View(day)
dim(day)##(731,16)
summary(day)#from this function we can say that there are no NA values in this dataset
str(day)
######converting some numeric  variables into categorical variables.########
day$dteday=as.Date(day$dteday)
day$season=as.factor(day$season)
day$yr=as.factor(day$yr)
day$mnth=as.factor(day$mnth)
day$holiday=as.factor(day$holiday)
day$workingday=as.factor(day$workingday)
day$weathersit=as.factor(day$weathersit)
str(day)
unique(day$holiday)
#######################visualization###################################
##to visualize the distribution of the continuous variables
v1=ggplot(day,mapping=aes(day$temp))+geom_histogram(bins = 25)+geom_density()##bimodal
v2=ggplot(day,mapping=aes(day$atemp))+geom_histogram(bins = 25)+geom_density()
v3=ggplot(day,mapping=aes(day$hum))+geom_histogram(bins = 25)+geom_density()##left skewed
v4=ggplot(day,mapping=aes(day$windspeed))+geom_histogram(bins = 25)+geom_density()##right skewed
v1
v2
v3
v4
##In brief all the continuous variables selected are normally distributed.
##########Check the distribution of categorical Data using bar graph####################
bar1 = ggplot(data = day, aes(x = season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = day, aes(x = weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = day, aes(x = holiday)) + geom_bar() + ggtitle("Count of Holiday")##
bar4 = ggplot(data = day, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")##more no of bike counts on working day ,here 0-holiday 1-workingday
bar1
bar2
######################## ##BIVARIATE AND MULTIVARIATE ANALYSIS#######################

###################To check the relationship between dependent and independent variables using scatterplot
p1 = ggplot(data = day, aes(x =temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point() + xlab("Temperature") + ylab("Bike Count")##from this scatter plot we can say that temp and bike counts are positively correlated
p2 = ggplot(data = day, aes(x =hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike Count")##there is good amount of correlation between humidity and bike counts
p3 = ggplot(data = day, aes(x =atemp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point() + xlab("Feel Temperature") + ylab("Bike Count")##the variable atemp and bike counts are highly correlated
p4 = ggplot(data = day, aes(x =windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike Count")## windspeed and bike counts are correlated

      



#bar plot for season wise monthly distribution of counts:-
ggplot(day, aes(season,fill=mnth) ) +
  geom_bar(position = 'dodge') 
##From above we can say that there is decreasing trend of bike counts in season 1
##whereas in other three seasons bike counts are initially increasing and after that it is decreasing

ggplot(day,aes(x=yr,y=cnt,fill=yr))+geom_col()+theme_bw()+
  labs(x='Year',y='Total_Count',title='Year wise distribution of counts')
##We can say that there is highest bike counts in the year 2012 as compared to year 2011.

###Distribution of Bike Counts during Holiday################################
ggplot(day,aes(x=holiday,y=cnt,fill=season))+geom_col()+theme_bw()+
  labs(x='holiday',y='Total_Count',title='distribution of counts During Holidays')
##From the above plot we can say that when there is no holiday the bike counts are highest
##whereas when there is a holiday the bike counts are negligible as compared to the non holiday
##here 0- workingday,1-holiday


#####################Distribution of counts During Workingday:-######################################
ggplot(day,aes(x=workingday,y=cnt,fill=season))+geom_col()+theme_bw()+
  labs(x='workingday',y='Counts',title='Workingday wise distribution of counts')
##Here 1-workingday,0-holiday
##from the above graph we can say that there are highest no of bike counts on the working day as comapared to the holiday
##In the season of summer and fall on the workingady there is more demand of bikes


###############Distribution of counts During Weather conditions:-############################
  ggplot(day,aes(x=weathersit,y=cnt,fill=season))+geom_col()+theme_bw()+
  labs(x='Weather_condition',y='counts',title=' distribution of counts in Different Weather conditions')
##Here 1: Clear, Few clouds, Partly cloudy, Partly cloudy 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist 3: Light Snow, Light Rain +
##From the above plot we can clearly say that when the weather is clear, partly cloudy,the bike counts are highest
##In the same weather conditions when there is summer and fall season the bike counts are always high irrespective of the other factors.  


 
#############Select the continuous variables to check the outliers#########
cont_var=subset(day,select = c('hum','windspeed','temp'))
View(cont_var) 
boxplot(cont_var)
##From the above boxplot we can see that the outliers are present in humidity and windspeed  
 

 sum(is.na(day))
 ########## Replace and impute the outliers:-########
    #create subset for windspeed and humidity variable:-
    cont_var<-subset(cont_var,select=c('windspeed','hum'))
    View(cont_var)
    
    
  #column names of wind_hum:-
  cnames<-colnames(cont_var)
  for(i in cnames){
    val=cont_var[,i][cont_var[,i] %in% boxplot.stats(cont_var[,i])$out] #separating outlier values in val variable.
    cont_var[,i][cont_var[,i] %in% val]= NA  # Replacing  outliers with NA 
  }
  
  
 ########## ##crosschecking#########
  sum(is.na(cont_var))##15 NA values 
  
  
  ##################Imputating the missing values using mean imputation method############
  cont_var$windspeed[is.na(cont_var$windspeed)]=mean(cont_var$windspeed,na.rm=T) 
  cont_var$hum[is.na(cont_var$hum)]=mean(cont_var$hum,na.rm=T)
  
  
  
 ######### #Remove the windspeed and humidity variable in order to replace imputed data:-########
  df<-subset(day,select=-c(windspeed,hum))
  #Combined df and cont_var data frames:-
  df1=cbind(df,cont_var)
  head(df1,5)
  sum(is.na(df1))
  
  
##############################FEATURE SELECTION#########################################
  install.packages("corrgram")
library(corrgram)  
corrgram(day, order = F, upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")  
##From the above coorrplot we can temp and atemp are highly positively correlated,so we will skip atemp ,some variables are positively correlated with each other & some are nagatively correlated with each other
View(df1)



#####Selecting the  Required  Columns for Model Building##################
new_df1=subset(df1,select = c('season','yr','mnth','holiday','weekday','workingday','weathersit','temp','hum','windspeed','cnt'))


#########################Divide the data into train and test#############################
set.seed(123)
train_index = sample(1:nrow(new_df1), 0.8 * nrow(new_df1))
train_data = new_df1[train_index,]
test_data = new_df1[-train_index,]


###################Train the data using linear regression####################################
lr_model = lm(formula = cnt~., data = train_data)


######Check the summary of the model#####################
summary(lr_model)###Adjusted R-squared:  0.8355 

## R-squared:  0.842,F statistics:129.7

#######################Predict the test cases#############################
lr_predictions = predict(lr_model, test_data[,1:10])
##############################Create dataframe for actual and predicted values########################
df = data.frame("actual"=test_data[,11], "pred"=lr_predictions)
head(df)

rmse<-RMSE(lr_predictions, test_data$cnt)
print(rmse)##692.3745


#######################Mean squared error:-##########################################
mae<-MAE(lr_predictions, test_data$cnt)
print(mae)##511.43


#################################calculate MAPE######################################
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}
MAPE(test_data$cnt, dt_predictions)



####################################Residual plot:-###########################
y_test<-test_data$cnt
residuals<-y_test-lr_predictions
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)

###########From the above graph we can say that the error is scattered randomly which is said to be a good fit.




dt_model = rpart(cnt ~ ., data = train_data, method = "anova")
summary(dt_model)


###############Visualize the learned decision tree model:-########################
  rpart.plot(dt_model, box.palette="RdBu", shadow.col="gray", nn=TRUE,roundint=FALSE)

############################Predict the test cases#########################
dt_predictions = predict(dt_model, test_data[,-11])


##################Create dataframe for actual and predicted values########

df = data.frame("actual"=test_data[,11], "pred"=dt_predictions)
head(df)
rmse<-RMSE(dt_predictions, test_data[,11])
print(rmse)##852.8797
mae<-MAE(dt_predictions, test_data$cnt)
print(mae)
#calculate MAPE
MAPE = function(actual, pred){
  print(mean(abs((actual - pred)/actual)) * 100)
}

MAPE(test_data$cnt, dt_predictions)##27.8162

##############comparing the two models##############
##When we compare two models Linear Regression and Decision Tree
##RMSE(LINEAR REGRESSION)=692.37    RMSE(DECISION TREE)=852.87
##MSE(LINEAR REGRESSION)=511.43        MAE(DECISION TREE)=609
###When we compare the root mean squared error and mean absolute error of 2 models, the Linear Regression model has less root mean squared error and mean absolute error. So, the Linear Regression model is best for predicting the bike rental count on daily basis.


Bike_prediction=data.frame(y_test,lr_predictions)
write.csv(Bike_prediction,'Bike_Count_R.CSV',row.names=F)
Bike_prediction
str(day)
