library('dplyr')
library('Rmisc')

## Lecture 4 code ##

# Load in the data
AQ = na.omit (  airquality[ , 1 : 4 ] )

# add in some new data
AQ$TWcp = AQ$Temp*AQ$Wind
AQ$TWrat = AQ$Temp/AQ$Wind


## Question 1 ##
summary(AQ$TWcp)
summary(AQ$TWrat)


## Question 2 ##
twcp.fit = lm(Ozone ~ Temp + Wind + TWcp, data=AQ)
twrat.fit = lm(Ozone ~ Temp + Wind + TWrat, data=AQ)

## (a)
summary(twcp.fit)
summary(twrat.fit)






## Question 3 ##

## Functions:

get.MSPE = function(Y, Y.hat) {
  residuals = Y - Y.hat
  resid.sq = residuals ^ 2
  SSPE = sum(resid.sq)
  MSPE = SSPE / length(Y)
  return(MSPE)
}

# Do a 75/25 split
n = nrow(AQ)
new.order = sample.int(n)
size.train = floor(n*0.75)

ind.train = new.order[1:size.train]
ind.valid = new.order[(size.train + 1): n]

data.train = AQ[ind.train,]
data.valid = AQ[ind.valid,]

# train the model
twcp.fit = lm(Ozone ~ Temp + Wind + TWcp, data=data.train)
twrat.fit = lm(Ozone ~ Temp + Wind + TWrat, data=data.train)

# run the models on the validation set
pred.twcp = predict(twcp.fit, data.valid)
pred.twrat = predict(twrat.fit, data.valid)

# Compute the MSPE's
Y.valid = data.valid$Ozone

MSPE.twcp  = get.MSPE(Y.valid, pred.twcp)
MSPE.twrat = get.MSPE(Y.valid, pred.twrat)

print("MSPE of twcp model:")
print(MSPE.twcp)
print("MSPE of twrat model:")
print(MSPE.twrat)






# 10 fold CV repeated 20 times

ave.CV.MSPEs = array(0, dim = c(20, 7))
colnames(ave.CV.MSPEs) = c('Solar.R','Wind','Temp','All Pred','Curved Interaction', 'TWcp', 'TWrat') 

for (j in 1:20) {
  
  n = nrow(AQ)
  num_folds = 10
  n.fold = n/num_folds 
  n.fold = ceiling(n.fold)
  
  # We can remove any excess later
  ordered.ids = rep(1:num_folds, times = n.fold)
  ordered.ids = ordered.ids[1:n] # Remove excess label(s)
  shuffle = sample.int(n) # Randomly permute the numbers 1 to n
  shuffled.ids = ordered.ids[shuffle] # Use shuffle to permute 
  
  # add the labels to the rows
  data.CV = AQ
  data.CV$fold = shuffled.ids
  
  CV.MSPEs = array(0, dim = c(num_folds, 7))
  colnames(CV.MSPEs) = c('Solar.R','Wind','Temp','All Pred','Curved Interaction', 'TWcp', 'TWrat') 
  
  
  for(i in 1:num_folds) {
    data.train = filter(data.CV, data.CV$fold != i)
    data.valid = filter(data.CV, data.CV$fold == i)
    
    data.train = select(data.train, -fold)
    data.valid = select(data.valid, -fold)
    
    # Fit some models
    Solar.R.lm  = lm(Ozone ~ Solar.R, data = data.train)
    Wind.lm     = lm(Ozone ~ Wind, data = data.train)
    Temp.lm     = lm(Ozone ~ Temp, data = data.train)
    all.pred.lm = lm(Ozone ~ Solar.R + Wind + Temp, data = data.train)
    curv.lm     = lm(Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)+Temp*Wind+Temp*Solar.R+Wind*Solar.R, data=data.train)
    twcp.fit    = lm(Ozone ~ Temp + Wind + TWcp, data=data.train)
    twrat.fit   = lm(Ozone ~ Temp + Wind + TWrat, data=data.train)    
    
    # run the models on the validation set
    pred.Solar.R = predict(Solar.R.lm, data.valid)
    pred.Wind    = predict(Wind.lm, data.valid)
    pred.Temp    = predict(Temp.lm, data.valid)
    pred.all     = predict(all.pred.lm, data.valid)
    pred.curv    = predict(curv.lm, data.valid)  
    pred.twcp    = predict(twcp.fit, data.valid)
    pred.twrat   = predict(twrat.fit, data.valid)    
    
    # Compute the MSPE's
    Y.valid = data.valid$Ozone
    
    MSPE.Solar.R = get.MSPE(Y.valid, pred.Solar.R)
    MSPE.Wind    = get.MSPE(Y.valid, pred.Wind)
    MSPE.Temp    = get.MSPE(Y.valid, pred.Temp)
    MSPE.all     = get.MSPE(Y.valid, pred.all)
    MSPE.curve   = get.MSPE(Y.valid, pred.curv)
    MSPE.twcp    = get.MSPE(Y.valid, pred.twcp)
    MSPE.twrat   = get.MSPE(Y.valid, pred.twrat)    
    
    ### Store MSPEs
    CV.MSPEs[i, 1] = MSPE.Solar.R
    CV.MSPEs[i, 2] = MSPE.Wind
    CV.MSPEs[i, 3] = MSPE.Temp
    CV.MSPEs[i, 4] = MSPE.all
    CV.MSPEs[i, 5] = MSPE.curve
    CV.MSPEs[i, 6] = MSPE.twcp
    CV.MSPEs[i, 7] = MSPE.twrat
  }
  
  this.ave.MSPEs = apply(CV.MSPEs, 2, mean)
  ave.CV.MSPEs[j,] = this.ave.MSPEs # We are replacing a whole 
  # row at once  
  
}


boxplot(ave.CV.MSPEs,
        main = "Boxplot of 20 Replicates of Average 10-Fold CV Error")


rel.ave.CV.MSPEs = apply(ave.CV.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})

rel.ave.CV.MSPEs = t(rel.ave.CV.MSPEs)

boxplot(rel.ave.CV.MSPEs,
        main = "Boxplot of 20 Replicates of Relative Average 10-Fold CV Error")



## Part B: Categorical Explanatories

ins = read.csv('Insurance.csv', header=TRUE)
ins$zone = as.factor(ins$zone)
ins$make = as.factor(ins$make)

ins = ins[ins$claims>0,]

all.var.fit = lm(per~km+zone+bonus+make+insured+claims, data=ins)
summary(all.var.fit)




## Lecture 5 ##

library('dplyr')
library('leaps')

# Load in the data
AQ = na.omit (  airquality[ , 1 : 4 ] )

# add in some new data
AQ$TWcp = AQ$Temp*AQ$Wind
AQ$TWrat = AQ$Temp/AQ$Wind



## FUNCTIONS: ##

shuffle = function(X){
  set.seed('2928893')
  new.order = sample.int(length(X))
  new.X = X[new.order]
  return(new.X)
}

### We will also often need to calculate MSE using an observed
### and a prediction vector. This will be another useful function.
get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

### The way that the lm() function calculates predicted values
### is somewhat limited. Specifically, the predict function only 
### works if our new data are in a data frame which contains 
### columns who's names match the variables in our model. Sometimes,
### we will want to format the new data as a matrix. This function
### lets us still get predicted values.
### Note: This function uses matrix-vector multiplication via the
### %*% operation. If you have taken a course in linear algebra, you
### will have seen how useful this tool can be. If not, don't
### worry, you don't need to understand the details of this function
predict.matrix = function(fit.lm, X.mat){
  coeffs = fit.lm$coefficients
  Y.hat = X.mat %*% coeffs
  return(Y.hat)
}


## ##


# create train and validation data sets
n = nrow(AQ)
n.train = floor(n * 0.75)
n.valid = n - n.train
groups = c(rep(1, times = n.train), rep(2, times = n.valid))
groups.shuffle = shuffle(groups)
data.train = AQ[groups.shuffle == 1,]
data.valid = AQ[groups.shuffle == 2,]

# Create the data matrix
data.matrix = model.matrix(Ozone ~ ., data = data.train)

# set the matrix for the response
Y.train = data.train$Ozone

# Run the algo
all.subsets = regsubsets(x = data.matrix, y = Y.train, nvmax = 30, intercept = F)
info.subsets = summary(all.subsets)
all.subsets.models = info.subsets$which

# Calculate the AIC and BIC

n.models = nrow(all.subsets.models) # Number of candidate models
all.AICs = rep(0, times = n.models) # Container to store AICs
all.BICs = all.AICs # Copy all.AICs to get a container for BICs

for(i in 1:n.models){
  ### We can actually supply a model matrix and response vector 
  ### to lm, without using a data frame. Remember that our model matrix
  ### already has an intercept, so we need to make sure lm doesn't
  ### include another one. We do this by including -1 in the right side
  ### of the model formula.
  this.data.matrix = data.matrix[,all.subsets.models[i,]]
  fit = lm(Y.train ~ this.data.matrix - 1) 
  
  ### NOTE: the -1 says not to add the intercept
  
  ### Get the AIC using extractAIC(). This function takes a regression
  ### model as input, as well as (optionally) an input called k, which
  ### specifies the penalty on the number of variables in our model.
  ### The AIC value is in the second component of the output object.
  this.AIC = extractAIC(fit)[2]
  all.AICs[i] = this.AIC
  
  ### Get the BIC using extractAIC(). This time, we need to set k equal
  ### to the log of the number of observations used to fit our model
  this.BIC = extractAIC(fit, k = log(n.train))[2] # explicitly define that you want a BIC
  all.BICs[i] = this.BIC
}

### Get the optimal model for AIC and BIC
AIC.ind = which.min(all.AICs)
AIC.model = all.subsets.models[AIC.ind,]

BIC.ind = which.min(all.BICs)
BIC.model = all.subsets.models[BIC.ind,]


## Stepwise algorithm (using the default step function:

fit.start = lm(Ozone ~ 1, data = data.train)
fit.end = lm(Ozone ~ ., data = data.train)

step.AIC = step(fit.start, list(upper = fit.end), k = 2)

step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train))

pred.step.AIC = predict(step.AIC, data.valid)
pred.step.BIC = predict(step.BIC, data.valid)

err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)








## Stepwise over 10 fold CV ##


### First we need to set the number of folds
K = 10

### Construct folds
### Don't attach fold labels to dataset because we would just have
### to remove this later
n = nrow(AQ)
n.fold = n/K # Approximate number of observations per fold
n.fold = ceiling(n.fold)
ordered.ids = rep(1:10, each = n.fold)
ordered.ids = ordered.ids[1:n]
fold.ids = shuffle(ordered.ids)

# Create the container
CV.models = c("stepwise.AIC", "stepwise.BIC")
errs.CV = array(0, dim = c(K,length(CV.models)))
colnames(errs.CV) = CV.models

# Copy the data and assign a fold id
data.CV = AQ
data.CV$fold = fold.ids 

for(i in 1:K){
  print(paste0(i, " of ", K))
  
  ### Construct training and validation sets by either removing
  ### or extracting the current fold. 
  ### Also, get the response vectors
  data.train = data.CV[fold.ids != i,]
  data.valid = data.CV[fold.ids == i,]
  
  n.train = nrow(data.train)
  
  ## Remove fold from training and validation sets
  data.train = select(data.train, -fold)
  data.valid = select(data.valid, -fold)  
  
  Y.train = data.train$Ozone 
  Y.valid = data.valid$Ozone
  
  ##########################################
  ### Stepwise selection via AIC and BIC ###
  ##########################################
  
  fit.start = lm(Ozone ~ 1, data = data.train)
  fit.end   = lm(Ozone ~ .,   data = data.train)
  
  ### These functions will run several times each. We don't need
  ### to print out all the details, so set trace = 0.
  step.AIC = step(fit.start, list(upper = fit.end), k=2, trace = 0)
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train), trace = 0)
  
  pred.step.AIC = predict(step.AIC, data.valid)
  pred.step.BIC = predict(step.BIC, data.valid)
  
  print(step.AIC)
  print(step.BIC)  
  
  err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  
  ### Store errors in errs.CV, which has two dimensions, so 
  ### we need two indices
  errs.CV[i, "stepwise.AIC"] = err.step.AIC
  errs.CV[i, "stepwise.BIC"] = err.step.BIC
}

# get the MSPE over all of the data
pred.all.data = predict(step.AIC, AQ)
all.data.MSPE = get.MSPE(AQ$Ozone, pred.all.data)

# get the mean:
mean(errs.CV[,"stepwise.AIC"])
mean(errs.CV[,"stepwise.BIC"])









