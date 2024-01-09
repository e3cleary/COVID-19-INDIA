# !/usr/bin/env Rscript

# wweek = as.integer(commandArgs(trailingOnly=TRUE)[1])
# print(c(wweek))
inla.setOption(num.threads = "4:1")


# Script to run INLA models in cross validation prediction mode

# Step 0: load packages pre-processed data and functions
# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 
# Step 2: produce cross-validated posterior predictive samples leaving out one week at a time

## Step 0: load packages and pre-processed data
source("3_load_packages_data.R")

# Step 1: rerun the selected model (fitted with config = TRUE for sampling) 

## fit full final model to use with control.mode = list(result = model1, restart = TRUE)
formulas[which(lab==best.model)]

formula <- Y ~ 1 + f(W1, model= "rw1", hyper = precision.prior) +
  f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + 
  Vp + GLH + TEMP + UV + policy

# Step 2: produce cross-validated posterior predictive samples leaving out one week at a time

# define number of samples

wweek=1
for(wweek in unique(data$week_index)){
  df2 <- df 
  
  # replace Rt data in testing period with NA for out of sample prediction
  casestopred <- data$R # response variable
  idx.pred <- which(data$week_index == wweek)
  casestopred[idx.pred] <- NA # replace Rt in week of interest to NA
  mpred <- length(idx.pred)
  
  # set response variable and week indicator
  df2$Y <- casestopred
  
  # final model for semi_urban
  model <- mymodel(formula, data = df2, family = "gamma", config = T)
  
  
  # model <- inla.rerun(model)
  
  preds <- list(week = 0 + wweek, idx.pred = idx.pred, 
                mean = model$summary.fitted.values$mean[idx.pred], 
                sd = model$summary.fitted.values$sd[idx.pred], 
                median = model$summary.fitted.values$`0.5quant`[idx.pred],
                lci = model$summary.fitted.values$`0.025quant`[idx.pred],
                uci = model$summary.fitted.values$`0.975quant`[idx.pred])
  save(preds, file = paste0("output/", region_subset, "/preds/preds_week", 0 + wweek, ".RData"))
  
  print(paste('Week:', wweek))
}
