#################### LOAD LIBRARIES AND FUNCTIONS ####################
source('Function Template.R')


#################### LOAD DATA ####################

AQ = na.omit (  airquality[ , 1 : 4 ] )

# add in some new data
AQ$TWcp = AQ$Temp*AQ$Wind
AQ$TWrat = AQ$Temp/AQ$Wind

Data = AQ  ## DYNAMIC
response_name = "Ozone"  ## DYNAMIC

#################### CROSS VALIDATION ####################
K = 10
n = nrow(Data)
folds = get.folds(n, K)

all.models = c("LS", "stepwise.AIC", "stepwise.BIC", "Ridge", "LASSO-Min", "LASSO-1se", "GAM", "PPR", "NN-1layer", "tree.cp.zero", "tree.cp.min", "tree.1SE") ## DYNAMIC
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models

PPR.fold.terms = array(0, dim = c(K, 1))
colnames(PPR.fold.terms) = c('Terms')

# lets actually run the CV
for(i in 1:K) {
  
  print(paste0("-- Running Fold: ", i, " --"))
  
  
  ### Set up the training data for folds ###
  data.train = Data[folds != i,]
  data.valid = Data[folds == i,]
  Y.train    = data.train[, response_name]
  Y.valid    = data.valid[, response_name]
  n.train    = nrow(data.train)
  
  
  ### Least Squares ###
  all.MSPEs[i, "LS"] = fit.ls(data.train, data.valid, Y.valid, response_name)
  
  
  #### Step ###
  all.MSPEs[i, "stepwise.AIC"] = fit.step.aic(data.train, data.valid, Y.valid, response_name)
  all.MSPEs[i, "stepwise.BIC"] = fit.step.bic(data.train, data.valid, Y.valid, response_name)
  
  
  ### Ridge Reg ###
  all.MSPEs[i, "Ridge"] = fit.ridge.reg(data.train, data.valid, Y.valid, response_name)
  
  
  ### LASSO ###
  all.MSPEs[i, "LASSO-Min"] = fit.lasso.min(data.train, data.valid, Y.valid, Y.train, response_name)
  all.MSPEs[i, "LASSO-1se"] = fit.lasso.1se(data.train, data.valid, Y.valid, Y.train, response_name)  
  
  ### Fitting a GAM ###
  all.MSPEs[i, "GAM"] = fit.gam.model(data.train, data.valid, Y.valid, response_name)
  
  ### Fitting a PPR ###
  all.MSPEs[i, "PPR"] = fit.ppr.model(data.train, data.valid, 5, 5, i, response_name)  ## DYNAMIC  
  
  ### NN ###
  all.n.hidden = c(1,3,5,7,9)
  all.shrink   = c(0.001, 0.1, 0.5, 1, 2)
  all.MSPEs[i, "NN-1layer"] = fit.1layer.nnet(data.train, data.valid, 5, 5, all.n.hidden, all.shrink, response_name)
  
  
  # Tree 
  all.MSPEs[i, "tree.cp.zero"] = reg.tree.cp.zero(data.train, data.valid, response_name)
  all.MSPEs[i, "tree.cp.min"]  = reg.tree.cp.min(data.train, data.valid, response_name)
  all.MSPEs[i, "tree.1SE"]     = reg.tree.1se(data.train, data.valid, response_name)

}


# print the MSPES + boxplots
plot.MSPE.boxplot(all.MSPEs)

### Calculate RMSPEs
all.RMSPEs = get.rmspe(all.MSPEs)
plot.RMSPE.boxplot(all.RMSPEs)








