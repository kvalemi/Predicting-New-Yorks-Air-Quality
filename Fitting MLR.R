library('dplyr')
library('Rmisc')

# Temp is much simpler to measure in comparison to Solar.R
  # if we have to chose between these two then chose temp

# Remove outliers
AQ = na.omit (  airquality[ , 1 : 4 ] )
dim (AQ)

# set the seed
set.seed(4099183)

# Do a 75/25 split
n = nrow(AQ)
new.order = sample.int(n)
size.train = floor(n*0.75)

ind.train = new.order[1:size.train]
ind.valid = new.order[(size.train + 1): n]

data.train = AQ[ind.train,]
data.valid = AQ[ind.valid,]



# Fit some models
Solar.R.lm  = lm(Ozone ~ Solar.R, data = data.train)
Wind.lm     = lm(Ozone ~ Wind, data = data.train)
Temp.lm     = lm(Ozone ~ Temp, data = data.train)
all.pred.lm = lm(Ozone ~ Solar.R + Wind + Temp, data = data.train)
curv.lm     = lm(Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)+Temp*Wind+Temp*Solar.R+Wind*Solar.R, data=data.train)

# run the models on the validation set
pred.Solar.R = predict(Solar.R.lm, data.valid)
pred.Wind    = predict(Wind.lm, data.valid)
pred.Temp    = predict(Temp.lm, data.valid)
pred.all     = predict(all.pred.lm, data.valid)
pred.curv    = predict(curv.lm, data.valid)

get.MSPE = function(Y, Y.hat) {
  residuals = Y - Y.hat
  resid.sq = residuals ^ 2
  SSPE = sum(resid.sq)
  MSPE = SSPE / length(Y)
  return(MSPE)
}

# Compute the MSPE's
Y.valid = data.valid$Ozone

MSPE.Solar.R = get.MSPE(Y.valid, pred.Solar.R)
MSPE.Wind    = get.MSPE(Y.valid, pred.Wind)
MSPE.Temp    = get.MSPE(Y.valid, pred.Temp)
MSPE.all     = get.MSPE(Y.valid, pred.all)
MSPE.curve   = get.MSPE(Y.valid, pred.curv)

cat("Solar.R SLR Model Had an MSPE of:", MSPE.Solar.R) 
cat("Wind SLR Model Had an MSPE of:", MSPE.Wind) 
cat("Temp SLR Model Had an MSPE of:", MSPE.Temp) 
cat("All Predictors MLR Model Had an MSPE of:", MSPE.all) 
cat("Curved Model With Interaction Terms Had an MSPE of:", MSPE.curve) 



# 10 fold cross validation

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

CV.MSPEs = array(0, dim = c(num_folds, 5))
colnames(CV.MSPEs) = c('Solar.R','Wind','Temp','All Pred','Curved Interaction') 


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
  
  # run the models on the validation set
  pred.Solar.R = predict(Solar.R.lm, data.valid)
  pred.Wind    = predict(Wind.lm, data.valid)
  pred.Temp    = predict(Temp.lm, data.valid)
  pred.all     = predict(all.pred.lm, data.valid)
  pred.curv    = predict(curv.lm, data.valid)  
  
  # Compute the MSPE's
  Y.valid = data.valid$Ozone
  
  MSPE.Solar.R = get.MSPE(Y.valid, pred.Solar.R)
  MSPE.Wind    = get.MSPE(Y.valid, pred.Wind)
  MSPE.Temp    = get.MSPE(Y.valid, pred.Temp)
  MSPE.all     = get.MSPE(Y.valid, pred.all)
  MSPE.curve   = get.MSPE(Y.valid, pred.curv)
 
  ### Store MSPEs
  CV.MSPEs[i, 1] = MSPE.Solar.R
  CV.MSPEs[i, 2] = MSPE.Wind
  CV.MSPEs[i, 3] = MSPE.Temp
  CV.MSPEs[i, 4] = MSPE.all
  CV.MSPEs[i, 5] = MSPE.curve
}

for(i in 1:5) {
  
  print(colnames(CV.MSPEs[,])[i])
  
  #print('mean:')
  #mean = mean(CV.MSPEs[,i])
  #print(mean)
  
  print('Confidence Interval:')
  print(CI(CV.MSPEs[,i], ci=0.95))
  
}







# 10 fold CV repeated 20 times

ave.CV.MSPEs = array(0, dim = c(20, 5))
colnames(ave.CV.MSPEs) = colnames(CV.MSPEs)

for (j in 1:20) {

  
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
  
  CV.MSPEs = array(0, dim = c(num_folds, 5))
  colnames(CV.MSPEs) = c('Solar.R','Wind','Temp','All Pred','Curved Interaction') 
  
  
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
    
    # run the models on the validation set
    pred.Solar.R = predict(Solar.R.lm, data.valid)
    pred.Wind    = predict(Wind.lm, data.valid)
    pred.Temp    = predict(Temp.lm, data.valid)
    pred.all     = predict(all.pred.lm, data.valid)
    pred.curv    = predict(curv.lm, data.valid)  
    
    # Compute the MSPE's
    Y.valid = data.valid$Ozone
    
    MSPE.Solar.R = get.MSPE(Y.valid, pred.Solar.R)
    MSPE.Wind    = get.MSPE(Y.valid, pred.Wind)
    MSPE.Temp    = get.MSPE(Y.valid, pred.Temp)
    MSPE.all     = get.MSPE(Y.valid, pred.all)
    MSPE.curve   = get.MSPE(Y.valid, pred.curv)
    
    ### Store MSPEs
    CV.MSPEs[i, 1] = MSPE.Solar.R
    CV.MSPEs[i, 2] = MSPE.Wind
    CV.MSPEs[i, 3] = MSPE.Temp
    CV.MSPEs[i, 4] = MSPE.all
    CV.MSPEs[i, 5] = MSPE.curve
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













