source('Helper Functions - Main.R')

# 1
fit.gam = gam(Ozone ~ s(Solar.R) + s(Wind) + s(Temp) + s(TWcp) + s(TWrat),
              data = AQ)

summary(fit.gam)

par(mfrow = c(3,2))
plot(fit.gam)



# 2

## Setting the Parameters of the CV
K = 10
n = nrow(AQ)
folds = get.folds(n, K)

all.models = c("LS", "stepwise.AIC", "stepwise.BIC", "Ridge", "LASSO-Min", "LASSO-1se", "GAM")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models

partial.ls.comp.count = array(0, dim = c(K, 1))
colnames(partial.ls.comp.count) = c('Component Count')


# lets actually run the CV
for(i in 1:K) {
  
  print(paste0("Running Fold: ", i))
  
  
  ### Set up the training data for folds ###
  data.train = AQ[folds != i,]
  data.valid = AQ[folds == i,]
  Y.train = data.train$Ozone
  Y.valid = data.valid$Ozone
  n.train = nrow(data.train)
  
  
  ### Least Squares ###
  all.MSPEs[i, "LS"] = fit.ls(data.train, data.valid, Y.valid)
  
  
  #### Step ###
  all.MSPEs[i, "stepwise.AIC"] = fit.step.aic(data.train, data.valid, Y.valid)
  all.MSPEs[i, "stepwise.BIC"] = fit.step.bic(data.train, data.valid, Y.valid)
  
  
  ### Ridge Reg ###
  all.MSPEs[i, "Ridge"] = fit.ridge.reg(data.train, data.valid, Y.valid)
  
  
  ### LASSO ###
  all.MSPEs[i, "LASSO-Min"] = fit.lasso.min(data.train, data.valid, Y.valid, Y.train)
  all.MSPEs[i, "LASSO-1se"] = fit.lasso.1se(data.train, data.valid, Y.valid, Y.train)  
  
  ### Fitting a GAM ###
  all.MSPEs[i, "GAM"] = fit.gam.model(data.train, data.valid, Y.valid)
  
  
  

}


# print the MSPES + boxplots
par( mfrow = c(1,1) )
boxplot(all.MSPEs, 
        main = paste0("CV MSPEs over ", K, " folds"), 
        las = 2)

par(cex.axis=0.75)


### Calculate RMSPEs
all.RMSPEs = get.rmspe(all.MSPEs)

boxplot(all.RMSPEs, 
        main = paste0("CV RMSPEs over ", K, " folds"), 
        las = 2)
par(cex.axis=0.75)












