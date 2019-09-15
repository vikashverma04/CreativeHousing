# Missing data imputation libraries 
library(VIM)
library(mice)
library(missForest)

# Factor Analysis lib
library(psych)
library(psy)
library(GPArotation)
library(stats)
library(FactoMineR)

# regression libs
library(Metrics)
library(lattice)
library(corpcor)
library(ggplot2)
library(car)
library(moments)

setwd('C:\\Users\\vikash\\Desktop\\DSA\\Hack')

fl_data <- read.csv("1522419498_DSA_Hackathon_Dataset.csv")


#Remove id 
# sqft living= sqft above + sqft base (remove sqft living)
# condition or grade --> for new houses no need of grade --> remove grade
#drops <- c("id", "zipcode", "yr_renovated", "sqft_basement")

drops <- c("id", "sqft_living", "grade")
fl_data <- fl_data[ , !(names(fl_data) %in% drops)]
nrow(fl_data)  #19999

# Bedroom, bathroom, Make 0 to  NA
fl_data$bathrooms <- ifelse(fl_data$bathrooms==0, NA, fl_data$bathrooms)
fl_data$bedrooms <- ifelse(fl_data$bedrooms==0, NA, fl_data$bedrooms)



# Create a temp csv file 
write.csv(fl_data, 'non_na.csv')


fl_data$yr_renovated <- ifelse(fl_data$yr_renovated==0, fl_data$yr_built, fl_data$yr_renovated)
fl_data$yr_built = 2019 - fl_data$yr_built
fl_data$yr_renovated = 2019 - fl_data$yr_renovated

fl_data = fl_data[complete.cases(fl_data[ , 3:4]),]
nrow(fl_data)
#fl_data_1 <- transform(fl_data, zip_freq = ave(seq(nrow(fl_data)), zipcode, FUN=length))


fl_data$zipcode <- factor(fl_data$zipcode)
levels(fl_data$zipcode)
summary(fl_data)
write.csv(fl_data, 'fl_data.csv')



fl_data$bedrooms <- ifelse(fl_data$bedrooms>7, NA, fl_data$bedrooms)
fl_data$bedrooms <- ifelse(fl_data$bedrooms==1 & fl_data$bathrooms>1, fl_data$bedrooms+1, fl_data$bedrooms)
write.csv(fl_data, 'fl_data.csv')
##boxplot(fl_data$bedrooms)
hist(fl_data$bedrooms)


#boxplot(fl_data$sqft_lot)
hist(fl_data$sqft_lot)



#Check missing value pattern



md.pattern(fl_data)
#plot missing value pattern
mice_plot <- aggr(fl_data, col=c('grey','red'),numbers=TRUE, sortVars=TRUE,labels=names(fl_data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Start MICE imputing
imputed_Data <- mice(fl_data, method = 'cart', m=3, maxit = 2)
summary(imputed_Data)

#Check complete data using one impute set
imputeData <- complete(imputed_Data,2)
summary(imputeData)
write.csv(imputeData, 'imputed_data.csv')

#plot(imputeData$sqft_lot[5:400], imputeData$price[5:400])

#md.pattern(imputeData)

# Treat Outliers

# CooksDistance

# mod <- lm(imputeData$price ~ ., data=imputeData)
# cooksd <- cooks.distance(mod)
# 
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels


skewness(imputeData$price)
skewness(imputeData$bedrooms)
skewness(imputeData$bathrooms)
skewness(imputeData$floors)
summary(imputeData$sqft_lot)
skewness(imputeData$sqft_above)
skewness(imputeData$sqft_basement)

hist(imputeData$price)
hist(imputeData$bedrooms)
hist(imputeData$bathrooms)
hist(imputeData$floors)
hist(imputeData$sqft_lot)

#boxplot(imputeData$price, horizontal = TRUE)
#boxplot(imputeData$bedrooms, horizontal = TRUE)
#boxplot(imputeData$bathrooms, horizontal = TRUE)
#boxplot(imputeData$floors, horizontal = TRUE)


# Treat skewness (logrithmic transform)

transformed_data <- imputeData

transformed_data$sqft_basement <- transformed_data$sqft_basement + 1
vars = c("price", "sqft_lot", "sqft_above", "sqft_basement")
for (i in vars){
  transformed_data[,i] <- log(transformed_data[,i])
}

skewness(transformed_data$price)
skewness(transformed_data$sqft_lot)
skewness(transformed_data$sqft_above)
skewness(transformed_data$sqft_basement)


summary(transformed_data)
write.csv(transformed_data, 'transformed_data.csv')


# Skipped Facor Data
#ToDo.... 


model_data <- transformed_data
set.seed(1221)
train = sample(1:nrow(model_data),nrow(model_data)/3)
test = -train
training_data = model_data[train,]
testing_data = model_data[test,]
testing_price = model_data$price[test]

model<-lm(price~ .,data=training_data)
predicted_price <- predict(model, testing_data)

summary(model)
# Residuals vs Regressors

par(mfrow=c(2,2))
plot(model)
#predicted_price = predict.lm(model,testing_data,type = 'response')
rmse(testing_price,predicted_price)
mape(testing_price,predicted_price)

# Confidence Interval for the slopes
confint(model,level=0.95)


influence.measures(model)
influenceIndexPlot(model, id.n=3) # Index Plots of the influence measures


# New model with omited observations
model_data1= model_data[c(-7102,-1412, -1133, -3829, -9053, -4513,-9051, -4313, -9049, -4312),]

set.seed(1221)
train1 = sample(1:nrow(model_data1),nrow(model_data1)/3)
test1 = -train1
training_data1 = model_data1[train1,]
testing_data1 = model_data[test1,]
testing_price1 = model_data1$price[test1]


model1<-lm(price~ .,data=model_data1)
summary(model1)
influenceIndexPlot(model1, id.n=3) # Index Plots of the influence measures


#predict values
new_predict = predict(model1,testing_data1,type = 'response')
par(mfrow=c(2,2))
plot(model1)
residualPlots(model1)
#Error estimation
rmse(testing_price1,new_predict)
mape(testing_price1,new_predict)
actuals_preds <- data.frame(cbind(actuals=testing_data1$price, predicteds=new_predict))
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy


######## Plot Dependent Vs Independent Var for polynomial check #############3

plot(transformed_data$bedrooms, transformed_data$price)
plot(transformed_data$bathrooms, transformed_data$price)
plot(transformed_data$sqft_lot, transformed_data$price)
plot(transformed_data$sqft_above, transformed_data$price)
plot(transformed_data$sqft_basement, transformed_data$price)
#pairs(transformed_data, pch = 19)

#model_data1= model_data[c(),]

qqnorm(model_data1$price)
qqline(model_data1$price, col= "red")
qqPlot(model_data1$price, distribution="norm")

# Try out factor analysis

factor_data <- transformed_data

# # try PCA
# 
# result <- PCA(factor_data)
# result

drops <- c("price", "zipcode")
factor_data <- factor_data[ , !(names(factor_data) %in% drops)]
cormat <- cor(factor_data)
cormat
n.factors <- 5


# FA with orthogonal rotation- Varimax
fit <- factanal(factor_data, 
                n.factors,              # number of factors to extract
                rotation="oblimin", scores="regression")     # 'varimax' is an ortho rotation
fit
fit$loadings
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(factor_data),cex=.7) # add variable names
load
fit$scores
#Confirm how many factors are desired
scree.plot(fit$correlation)
#parallel <- fa.parallel(factor_data, fm = 'minres', fa = 'fa')
factor_data2 = as.data.frame(cbind(transformed_data$price, transformed_data$zipcode, fit$scores))
names(factor_data2) <- c('price', 'zipcode', 'factor_1', 'factor_2', 'factor_3', 'factor_4', 'factor_5')
model_data_2 <- factor_data2
set.seed(1221)
train = sample(1:nrow(model_data_2),nrow(model_data_2)/3)
test = -train
training_data2 = model_data_2[train,]
testing_data2 = model_data_2[test,]
testing_price2 = model_data_2$price[test]

model2<-lm(price~ .,data=training_data2)
predicted_price2 <- predict(model2, testing_data2)

summary(model2)
residualPlots(model2)

rmse(testing_price2,new_predict)
mape(testing_price2,new_predict)
actuals_preds <- data.frame(cbind(actuals=testing_data2$price, predicteds=new_predict))
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
min_max_accuracy

########### Check correlation
install.packages('ggpubr')
library("ggpubr")
ggscatter(transformed_data, x = "bedrooms", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")



ggscatter(transformed_data, x = "sqft_above", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(transformed_data, x = "bathrooms", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(transformed_data, x = "sqft_lot", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(transformed_data, x = "sqft_basement", y = "price", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")



vars = c("bedrooms", "bathrooms", "sqft_lot", "sqft_above", "sqft_basement")
for (i in vars){
  ggscatter(transformed_data, x = i, y = "price", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson")
}

# Polynomial models:

transformed_data <- imputeData

transformed_data$sqft_basement <- transformed_data$sqft_basement + 1
vars = c("price", "sqft_lot", "sqft_above", "sqft_basement")
for (i in vars){
  transformed_data[,i] <- log(transformed_data[,i])
}

transformed_data$sqft_above_square <- transformed_data$sqft_above^2
transformed_data$sqft_basement_square <- transformed_data$sqft_basement^2



skewness(transformed_data$sqft_above_square)
skewness(transformed_data$sqft_basement_square)
skewness(transformed_data$sqft_above)
skewness(transformed_data$sqft_basement)

model_data3 <- transformed_data

train = sample(1:nrow(model_data3),nrow(model_data3)/3)
test = -train
training_data = model_data3[train,]
testing_data = model_data3[test,]
testing_price = model_data3$price[test]

model3<-lm(price~ .,data=training_data)
predicted_price <- predict(model3, testing_data)

summary(model3)
# Residuals vs Regressors

par(mfrow=c(2,2))
plot(model3)
#predicted_price = predict.lm(model,testing_data,type = 'response')
rmse(testing_price,predicted_price)
mape(testing_price,predicted_price)
residualPlots(model3)
