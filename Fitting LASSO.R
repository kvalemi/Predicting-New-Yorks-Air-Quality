library('dplyr')
library('Rmisc')
library(stringr)
library(glmnet)
library(MASS)


## FUNCTIONS ##

### We will regularly need to shuffle a vector. This function
### does that for us.
shuffle = function(X){
  set.seed(2928893)
  new.order = sample.int(length(X))
  new.X = X[new.order]
  return(new.X)
}

### We will also often need to calculate MSE using an observed
### and a prediction vector. This is another useful function.
get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

get.folds = function(n, K) {
  set.seed(2928893)
  ### Get the appropriate number of fold labels
  n.fold = ceiling(n / K) # Number of observations per fold (rounded up)
  fold.ids.raw = rep(1:K, times = n.fold) # Generate extra labels
  fold.ids = fold.ids.raw[1:n] # Keep only the correct number of labels
  
  ### Shuffle the fold labels
  folds.rand = fold.ids[sample.int(n)]
  
  return(folds.rand)
}

### Calculate RMSPEs
all.RMSPEs = apply(all.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})


##################################################################






# Setting up variables
# Load in the data
AQ = na.omit (  airquality[ , 1 : 4 ] )
AQ$TWcp = AQ$Temp*AQ$Wind
AQ$TWrat = AQ$Temp/AQ$Wind

# 1

# (a)

# Fit Ridge with multiple values of lambda
lambda.vals = seq(from = 0, to = 100, 0.05)
fit.ridge = lm.ridge(Ozone ~ ., lambda = lambda.vals, data = AQ)

# get the lambda
ind.min.GCV = which.min(fit.ridge$GCV)
lambda.min = lambda.vals[ind.min.GCV]

# get ridge coefficients
all.coefs.ridge = coef(fit.ridge)
coef.min = all.coefs.ridge[ind.min.GCV,]

# get the least squares coefficients to compare
summary(lm(Ozone~., data=AQ))



## 2

matrix.train.raw = model.matrix(Ozone ~., data=AQ)
matrix.train = matrix.train.raw[,-1]
head(matrix.train)

# The lambda that gets the smallest MSPE over the internal CV is called lambda min.
# The lambda that gets the simplest model over the internal CV is called the 1SE lambda.

# Run the lmnet to run the internal CV to pick the lambda
all.LASSOs = cv.glmnet(x=matrix.train, y=AQ$Ozone)

lambda.min = all.LASSOs$lambda.min
lambda.1se = all.LASSOs$lambda.1se
lambda.min
lambda.1se

# Now lets build the model from these model estimates
coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type = 'coef')
coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = 'coef')

included.LASSO.min = predict(all.LASSOs, s = lambda.min, 
                             type = "nonzero")
included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, 
                             type = "nonzero")



# 3


# Set up for the CV:
K = 10

n = nrow(AQ)
folds = get.folds(n, K)
all.models = c("LS", "stepwise.AIC", "stepwise.BIC", "Ridge", "LASSO-Min", "LASSO-1se")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models

# lets actually run the CV
for(i in 1:K) {
  
  data.train = AQ[folds != i,]
  data.valid = AQ[folds == i,]
  
  Y.train = data.train$Ozone
  Y.valid = data.valid$Ozone
  
  n.train = nrow(data.train)
  
  # LS:
  fit.ls = lm(Ozone ~ ., data=data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls
  
  
  # Step:
  fit.start = lm(Ozone ~ 1,   data = data.train)
  fit.end   = lm(Ozone ~ .,   data = data.train)
  
  step.AIC = step(fit.start, list(upper = fit.end), k=2, trace = 0)
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train), trace = 0)
  
  pred.step.AIC = predict(step.AIC, data.valid)
  pred.step.BIC = predict(step.BIC, data.valid)
  
  err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  
  ### Store errors in errs.CV, which has two dimensions, so 
  ### we need two indices
  all.MSPEs[i, "stepwise.AIC"] = err.step.AIC
  all.MSPEs[i, "stepwise.BIC"] = err.step.BIC  
  
  
  
  # Ridge Reg:
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  
  fit.ridge = lm.ridge(Ozone ~ ., lambda = lambda.vals, data = data.train)
  
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  
  matrix.valid.ridge = model.matrix(Ozone ~ ., data = data.valid)
  pred.ridge = matrix.valid.ridge %*% coef.min
  MSPE.ridge = get.MSPE(Y.valid, pred.ridge)
  all.MSPEs[i, "Ridge"] = MSPE.ridge
  
  
  # LASSO
  matrix.train.raw = model.matrix(Ozone ~ ., data = data.train)
  matrix.train = matrix.train.raw[, -1]
  
  all.LASSOs = cv.glmnet(x = matrix.train, y = Y.train)
  
  lambda.min = all.LASSOs$lambda.min
  lambda.1se = all.LASSOs$lambda.1se
  
  print(lambda.min)
  print(lambda.1se)
  
  coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type='coef')
  coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type='coef')
  
  included.LASSO.min = predict(all.LASSOs, s = lambda.min, 
                               type = "nonzero")
  included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, 
                               type = "nonzero")
  
  matrix.valid.LASSO.raw = model.matrix(Ozone ~ ., data = data.valid)
  matrix.valid.LASSO = matrix.valid.LASSO.raw[,-1]
  
  pred.LASSO.min = predict(all.LASSOs, newx = matrix.valid.LASSO,
                           s = lambda.min, type = "response")
  pred.LASSO.1se = predict(all.LASSOs, newx = matrix.valid.LASSO,
                           s = lambda.1se, type = "response")
  
  MSPE.LASSO.min = get.MSPE(Y.valid, pred.LASSO.min)
  all.MSPEs[i, "LASSO-Min"] = MSPE.LASSO.min
  
  MSPE.LASSO.1se = get.MSPE(Y.valid, pred.LASSO.1se)
  all.MSPEs[i, "LASSO-1se"] = MSPE.LASSO.1se  
  
}

# d
subset(all.MSPEs, select=c('Ridge', 'LASSO-Min', 'LASSO-1se'))

mean(subset(all.MSPEs, select=c('Ridge', 'LASSO-Min', 'LASSO-1se'))[,1])
mean(subset(all.MSPEs, select=c('Ridge', 'LASSO-Min', 'LASSO-1se'))[,2])
mean(subset(all.MSPEs, select=c('Ridge', 'LASSO-Min', 'LASSO-1se'))[,3])


# e and f
par( mfrow = c(1,1) )
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"), las = 2)
par(cex.axis=0.75)


### Calculate RMSPEs
all.RMSPEs = apply(all.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
all.RMSPEs = t(all.RMSPEs)

### Make a boxplot of RMSPEs
boxplot(all.RMSPEs, main = paste0("CV RMSPEs over ", K, " folds"), las = 2)
par(cex.axis=0.75)

### One model is much worse than the others. Let's zoom in on the
### good models.
boxplot(all.RMSPEs, ylim = c(1, 1.03),
        main = paste0("CV RMSPEs over ", K, 
                      " folds (enlarged to show texture)"),
)
