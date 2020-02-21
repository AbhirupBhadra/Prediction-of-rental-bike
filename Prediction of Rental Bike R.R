# Cleaning the environment
rm(list = ls())

# Setting the working directory
setwd("E:/data science_edwisor/Project 1 bike prediction")
getwd()

#Reading the file
data1 = read.csv('day.csv', header = TRUE)

#Loading libraries
x = c("ggplot2","usdm","caret","corrgram","randomForest","plyr","dplyr","rpart","DataCombine")
lapply(x, require, character.only=TRUE)

#EDA
dim(data1)
str(data1)
names(data1)
summary(data1)

# Checking missing value
sum(is.na(data1))
sapply(data1, function(x){
  sum(is.na(x))
})

# Dropping the variable which are not required
data1 = subset(data1,select= -c(instant,dteday,casual,registered))

# Converting into proper data type
cat_var= c('season','yr','mnth','holiday','weekday','workingday','weathersit')
num_var= c('temp','atemp','hum','windspeed')
data_conv = function(data1,var,type){
  data1[var] = lapply(data1[var], type)
  return(data1)
}
data1= data_conv(data1,cat_var,factor)

# Data Visualization
# CNT according to Season
ggplot(data1, aes(fill=cnt, x=season)) +
  geom_bar(position="dodge") + labs(title="cnt ~ season")

# CNT according to holiday
ggplot(data1, aes(fill=cnt, x=holiday)) +
  geom_bar(position="dodge") + labs(title="cnt ~ holiday")

# CNT according to season by yr
ggplot(data1, aes(fill=cnt, x=season)) +
  geom_bar(position="dodge") + facet_wrap(~yr)+
  labs(title="CNT according to season by yr")

# CNT according to season by workingday
ggplot(data1, aes(fill=cnt, x=season)) +
  geom_bar(position="dodge") + facet_wrap(~workingday)+
  labs(title="CNT according to season by workingday")

# CNT according to season by weekday
ggplot(data1, aes(fill=cnt, x=workingday)) +
  geom_bar(position="dodge") + facet_wrap(~weekday)+
  labs(title="CNT according to workingday by weekday")

#Check the distribution of categorical Data using bar graph

bar1 = ggplot(data = data1, aes(x = season)) + geom_bar() + ggtitle("Count of Season")
bar2 = ggplot(data = data1, aes(x = weathersit)) + geom_bar() + ggtitle("Count of Weather")
bar3 = ggplot(data = data1, aes(x = holiday)) + geom_bar() + ggtitle("Count of Holiday")
bar4 = ggplot(data = data1, aes(x = workingday)) + geom_bar() + ggtitle("Count of Working day")
# ## Plotting plots together
gridExtra::grid.arrange(bar1,bar2,bar3,bar4,ncol=2)

#Check the distribution of numerical data using histogram
hist1 = ggplot(data = data1, aes(x =temp)) + ggtitle("Distribution of Temperature") + geom_histogram(bins = 25)
hist2 = ggplot(data = data1, aes(x =hum)) + ggtitle("Distribution of Humidity") + geom_histogram(bins = 25)
hist3 = ggplot(data = data1, aes(x =atemp)) + ggtitle("Distribution of Feel Temperature") + geom_histogram(bins = 25)
hist4 = ggplot(data = data1, aes(x =windspeed)) + ggtitle("Distribution of Windspeed") + geom_histogram(bins = 25)
gridExtra::grid.arrange(hist1,hist2,hist3,hist4,ncol=2)

#Check the distribution of numerical data using scatterplot
scat1 = ggplot(data = data1, aes(x =temp, y = cnt)) + ggtitle("Distribution of Temperature") + geom_point(color="blue") + xlab("Temperature") + ylab("Bike Count")
scat2 = ggplot(data = data1, aes(x =hum, y = cnt)) + ggtitle("Distribution of Humidity") + geom_point(color="red") + xlab("Humidity") + ylab("Bike Count")
scat3 = ggplot(data = data1, aes(x =atemp, y = cnt)) + ggtitle("Distribution of Feel Temperature") + geom_point(color="blue") + xlab("Feel Temperature") + ylab("Bike Count")
scat4 = ggplot(data = data1, aes(x =windspeed, y = cnt)) + ggtitle("Distribution of Windspeed") + geom_point(color="red") + xlab("Windspeed") + ylab("Bike Count")
gridExtra::grid.arrange(scat1,scat2,scat3,scat4,ncol=2)

#Check for outliers in data using boxplot

cnames = colnames(data1[,c("temp","atemp","windspeed","hum")])
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = cnames[i]), data = data1)+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot for",cnames[i])))
}
gridExtra::grid.arrange(gn1,gn3,gn2,gn4,ncol=2)

#Remove outliers in Windspeed
windoutlr = data1[,11][data1[,11] %in% boxplot.stats(data1[,11])$out]
data1 = data1[which(!data1[,11] %in% windoutlr),]
boxplot(data1$windspeed)

#Remove outliers in humidity
humoutlr = data1[,10][data1[,10] %in% boxplot.stats(data1[,10])$out]
data1 = data1[which(!data1[,10] %in% humoutlr),]
boxplot(data1$hum)

#correlation plot
data2 = subset(data1, select= -c(season,yr,mnth,holiday,weekday,workingday,weathersit))
corr = round(cor(data2),2)
ggcorrplot(corr,hc.order = T,
           type = "full",
           lab = T,
           lab_size = 3,
           method = "square",
           colors = c("blue","white","darkgreen"),
           title = "Correlation Plot",
           ggtheme = theme_bw)

#Removing atemp from the data1 dataset
data1 = subset(data1, select = -(atemp))

### Modeling
#Dividing into test and train seta
t_idx = sample(1:nrow(data1), 0.8*nrow(data1))
train = data1[t_idx,]
test = data1[-t_idx,]
# Removing all the custom variables from the memory
rmExcept(c("test","train","data1"))
# MAPE
mape = function(actual, predict){
  mean(abs((actual-predict)/actual))*100
}

##### Decission Tree

# rpart for regression. Train the model
dt_model = rpart(cnt~ ., data= train, method = "anova")
#Prediction of cnt from test data
dt_pred = predict(dt_model, test[,-11])
#Creating dataframe for actual and predicted value
dt_df = data.frame("actual" = test[,11], "pred"= dt_pred)
head(dt_df)
plot(test$cnt, dt_pred,
     xlab='Actual values',
     ylab= 'Prediction Values',
     main = 'DT model')
# Evaluation
postResample(dt_pred, test$cnt)
mape(test$cnt, dt_pred)
#MAPE: 18.54%
#MAE: 618.11
#RMSE: 796.51
#Accuracy: 81.46%

#### Random Forest

# Train the model
rf_model = randomForest(cnt~., data = train, ntree = 500)
#Prediction of cnt from test data
rf_pred = predict(rf_model, test[,-11])
#Creating dataframe for actual and predicted value
rf_df = data.frame("actual" = test[,11], "pred"= rf_pred)
head(rf_df)
plot(test$cnt, rf_pred,
     xlab='Actual values',
     ylab= 'Prediction Values',
     main = 'RF model')
# Evaluation
postResample(rf_pred, test$cnt)
mape(test$cnt, rf_pred)
#MAPE: 12.12%
#MAE: 384.28
#RMSE: 521.73
#Accuracy: 87.88%

#### Linear Regression 

#Train the data using linear regression
lr_model = lm(formula = cnt~., data = train)
#Check the summary of the model
summary(lr_model)
#Predict the test cases
lr_pred = predict(lr_model, test[,-11])
#Creating dataframe for actual and predicted value
lr_df = data.frame("actual" = test[,11], "pred"= lr_pred)
head(lr_df)
plot(test$cnt, lr_pred,
     xlab='Actual values',
     ylab= 'Prediction Values',
     main = 'LR model')
# Evaluation
postResample(lr_pred, test$cnt)
mape(test$cnt, lr_pred)
#MAPE: 13.36%
#MAE: 519.52
#RMSE: 701.62
#Accuracy: 86.64%

# saving the best model(Random Forest) output data
results = data.frame(test, pred_cnt = rf_pred)
write.csv(results, file = 'RF output R .csv', row.names = FALSE, quote=FALSE)
