### IQR for outliears

removeOutliers = function(x) { 
  # Get Q1 and Q3
  qnt = quantile(x, probs=c(.25, .75))
  
  # Get the interquartile range time 1.5
  iqt = 2 * IQR(x)
  
  # Apply on a copy of the original data
  y = x 
  y[x < (qnt[1] - iqt)] = NA
  y[x > (qnt[2] + iqt)] = NA
  return(y)
  
  # Remove incomplete cases and return the resulted variable
  #return(y[complete.cases(y)])
}

vars <- c("sqft_lot")

for (i in vars){
  fl_data[,i] = removeOutliers(fl_data[,i])
  #print(i)
}

write.csv(fl_data, 'removed_outliers.csv')

#FA without rotation (still remains orthogonal in nature)
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
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(factor_data),cex=.7) # add variable names
load
fit$scores
#Confirm how many factors are desired
scree.plot(fit$correlation)
parallel <- fa.parallel(factor_data, fm = 'minres', fa = 'fa')

#To plot more than one factor at a time
pairs(fit$loadings)
pairs(fit$loadings, col=1:ncol(transformed_data), upper.panel=NULL, main="Factor loadings")
legend('topright', pch='o', col=1:ncol(transformed_data), attr(fit$loadings, 'dimnames')[[1]], title="Variables")


#Check Normal distribution
hist(transformed_data$Workex..months.,prob=TRUE)
curve(dnorm(x,mean(transformed_data$Workex..months.),sd(transformed_data$Workex..months.)),col="red",add=TRUE)

qqnorm(transformed_data$Workex)
qqline(transformed_data$Workex, col= "red")
qqPlot(transformed_data$Workex, distribution="norm")


dotplot(transformed_data$sqft_living, main="Dot Plot of CEO Salary")

#drops <- c("floors","yr_renovated","zipcode")
#model_data <- model_data[ , !(names(model_data) %in% drops)]


# hist(completeData$sqft_living,prob=TRUE)
# curve(dnorm(x,mean(completeData$sqft_living),sd(completeData$sqft_living)),col="red",add=TRUE)
# skewness(completeData$sqft_living)
# 
# qqnorm(completeData$bedroom)
# qqline(completeData$bedroom, col= "red")
# qqPlot(completeData$bedroom, distribution="norm")
