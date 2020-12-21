source('../Function Template.R')

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}


# Load the data
AQ = na.omit (  airquality[ , 1 : 4 ] )

# add in some new data
AQ$TWcp = AQ$Temp*AQ$Wind
AQ$TWrat = AQ$Temp/AQ$Wind


Data = AQ

# 1
n = nrow(Data)
n.train = floor(n*0.75)
n.valid = n - n.train

inds = c(rep(1, times = n.train), rep(2, times = n.valid))
set.seed(2928893)
inds.rand = inds[sample.int(n)]

data.train = Data[inds.rand == 1,]
X.train.raw = subset(data.train, select = -c(Ozone))
Y.train = data.train$Ozone

data.valid = Data[inds.rand == 2,]
X.valid.raw = subset(data.valid, select = -c(Ozone))
Y.valid = data.valid$Ozone

# standardize
X.train = rescale(X.train.raw, X.train.raw)
X.valid = rescale(X.valid.raw, X.train.raw)







# 2

# call the function
X.train     = subset(X.train, select = c(Temp, Wind))
X.train.raw = subset(X.train.raw, select = c(Temp, Wind))

all.n.hidden = c(2,6)
all.shrink = c(0.001,1)

# Create the matrix for the different iterations
all.pars = expand.grid(n.hidden = all.n.hidden, shrink = all.shrink)
n.pars = nrow(all.pars) 

# Iterate through the pairs
for(j in 1:n.pars) {
  
  # Extract paired values
  this.n.hidden = all.pars[j,1]
  this.shrink = all.pars[j,2]
  
  # Create containers
  all.nnets = list(1:M)
  all.SSEs = rep(0, times = M)
  
  # Iterate through pairs
  for(l in 1:M){
    
    fit.nnet = nnet(X.train, Y.train, linout = TRUE, size = this.n.hidden,
                    decay = this.shrink, maxit = 5000, trace = FALSE)
    
    ### Get model SSE
    SSE.nnet = fit.nnet$value
    
    ### Store model and its SSE
    all.nnets[[l]] = fit.nnet
    all.SSEs[l] = SSE.nnet
  }
  
  # Get the best  fit
  min.sses = min(all.SSEs)
  ind.best = which.min(all.SSEs)
  fit.nnet.best = all.nnets[[ind.best]]
  
  # print(paste0("Hidden Nodes: ", this.n.hidden, ", Shrinkage: ", this.shrink, ", sMSE: ", min.sses))
  
  
  # Plot
  x1 <- seq(from = min(Data$Temp), to = max(Data$Temp), by=.05)
  x2 <- seq(from = min(Data$Wind), to = max(Data$Wind), by=.05)
  
  xy1 <- data.frame(expand.grid(Temp=x1, Wind=x2))
  
  pred2 <- predict(fit.nnet.best, newdata = rescale(xy1, X.train.raw) )
  surface2 = matrix(pred2)
  
  
  open3d()
  persp3d(x = x1, 
          y = x2, 
          z = surface2, 
          main = paste0("Hidden Nodes: ", this.n.hidden, ", Shrinkage: ", this.shrink),
          col = "orange", 
          xlab="Temp", 
          ylab="Wind", 
          zlab="Predicted Ozone")
  
  points3d(Data$Ozone ~ Data$Temp + Data$Wind, col="blue")  
  

}








# 3

Data = subset(Data, select = c(Temp, Wind, Ozone))

all.n.hidden = c(1,3,5,7,9)
all.shrink = c(0.001, 0.1, 0.5, 1, 2)
M = 10

all.pars = expand.grid(n.hidden = all.n.hidden, shrink = all.shrink)
n.pars = nrow(all.pars) # Number of parameter combinations

K = 5 # Number of folds

### Create folds
folds = get.folds(n, K)

### Create container for MSPEs
CV.MSPEs = array(0, dim = c(K, n.pars))

for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data and rescale predictors
  data.train  = Data[folds != i,]
  X.train.raw = subset(data.train, select = -c(Ozone))
  X.train = rescale(X.train.raw, X.train.raw)
  Y.train = data.train$Ozone
  
  data.valid = Data[folds == i,]
  X.valid.raw = subset(data.valid, select = -c(Ozone))
  X.valid = rescale(X.valid.raw, X.train.raw)
  Y.valid = data.valid$Ozone
  
  
  ### Fit neural net models for each parameter combination. A second 
  ### for loop will make our life easier here
  for(j in 1:n.pars){
    ### Get current parameter values
    this.n.hidden = all.pars[j,1]
    this.shrink = all.pars[j,2]
    
    ### We need to run nnet multiple times to avoid bad local minima. Create
    ### containers to store the models and their errors.
    all.nnets = list(1:M)
    all.SSEs = rep(0, times = M)
    
    ### We need to fit each model multiple times. This calls for another
    ### for loop.
    for(l in 1:M){
      ### Fit model
      fit.nnet = nnet(X.train, Y.train, linout = TRUE, size = this.n.hidden,
                      decay = this.shrink, maxit = 500, trace = FALSE)
      
      ### Get model SSE
      SSE.nnet = fit.nnet$value
      
      ### Store model and its SSE
      all.nnets[[l]] = fit.nnet
      all.SSEs[l] = SSE.nnet
    }
    
    ### Get best fit using current parameter values
    ind.best = which.min(all.SSEs)
    fit.nnet.best = all.nnets[[ind.best]]
    
    ### Get predictions and MSPE, then store MSPE
    pred.nnet = predict(fit.nnet.best, X.valid)
    MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
    
    CV.MSPEs[i, j] = MSPE.nnet # Be careful with indices for CV.MSPEs
  }
}



CV.MSPEs.sqrt = sqrt(CV.MSPEs)

for(i in 1:n.pars) {
  
  ci = CI(CV.MSPEs.sqrt[,i], ci = 0.95)
  
  print(paste0( "--> (", all.pars[i, 1], ", ", all.pars[i, 2], "):  ", round(sqrt(ci[2]), 3), " (mean), ", round(sqrt(ci[1]), 3), "(lower), ", round(sqrt(ci[3]), 3), "(upper)") )
  
  # print(paste0( " -> Square Root of Overall MSPE: ", round(sqrt(ci[2]), 3) ) )
  
  # print(paste0( " -> Square Root of CI: [", round(sqrt(ci[1]), 3), ", ", round(sqrt(ci[3]), 3), "]" ) )
  
}


names.pars = paste0(all.pars$n.hidden,",",
                    all.pars$shrink)
colnames(CV.MSPEs.sqrt) = names.pars

plot.RMSPE.boxplot(get.rmspe(CV.MSPEs.sqrt))






