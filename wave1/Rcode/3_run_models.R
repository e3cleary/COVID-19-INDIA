# R script to run INLA models of increasing complexity
# WARNING: the script may take over a day to run

# Step 0: load packages and pre-processed data 
# Step 1: formulate a baseline model including spatiotemporal random effects and test different combinations of DLNM variables

## Step 0: load packages and pre-processed data
# source("3_load_packages_data.R")

# run models of increasing complexity in INLA

# Step 1: fit a baseline model including spatiotemporal random effects

## formulate a base model including: 
# rw1: province-specific random effects to account for weekly variation (random walk cyclic prior) - with Random walk model of order 1 (RW1)
# https://inla.r-inla-download.org/r-inla.org/doc/latent/rw1.pdf
# bym2: week-specific spatial random effects to account for inter-week variation in spatial overdisperson and dependency structures (modified Besag-York-Mollie prior bym2)
# https://inla.r-inla-download.org/r-inla.org/doc/latent/bym2.pdf

#### set up data and priors for INLA model

##  baseline model -----
# baseformula <- Y ~ f(S1.1, W1, model= "iid", hyper = precision.prior) +
#   f(S1, model = "besag", scale.model = TRUE, graph = g, hyper = precision.prior)
# 
# baseformula <- Y ~ f(S1.1, W1, model= "iid", hyper = precision.prior) +
#   f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) 
# 
# baseformula <- Y ~ f(W1, model= "rw1", hyper = precision.prior) +
#   f(S1, model = "besag", scale.model = TRUE, graph = g, hyper = precision.prior) 

# baseformula <- Y ~ f(W1, model= "rw1", hyper = precision.prior) +
#   f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) # dic 9819.72
# # test baseline model
# model <- mymodel(formula = baseformula, data = df, family = "Gaussian", config = FALSE)
# model$dic$dic
# summary(model)

# colnames(df) <- c('Y', 'R', 'R0', 'ln_R', 'W1', 'S1', 'S2', 
                  # 'GLH', 'TEMP', 'PREC', 'UV', 'policy', 'Vp', 'Vh')
head(df)

baseformula <- Y ~ 1 + f(W1, model= "rw1", hyper = precision.prior) + 
  f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + 
  Vp # dic gamma 7212 / gaussian 13411 / weibull 8444

# baseformula <- Y ~ f(W1, model= "rw1", hyper = precision.prior) +
#   f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + Vp +
#   basis_GLH + basis_TEMP + basis_PREC + basis_policy 
# # gamma 6664 / gaussian 13076 / weibull 7855

# test baseline model
head(df)
nrow(df)
model <- mymodel(formula = baseformula, data = df, family = 'gamma', config = FALSE)
model$dic$dic
-mean(log(model$cpo$cpo))

summary(model)

save(model, file = "output/whole country/basemodel.RData")

head(data)
head(df)

## define formulas by updating the baseline formula with different combinations of
# cross-basis functions of temperature, precipitation, covid cases, and intervention index
f1.1 <- update.formula(baseformula, ~. + basis_GLH )
f1.2 <- update.formula(baseformula, ~. + basis_TEMP )
f1.3 <- update.formula(baseformula, ~. + basis_PREC )
f1.4 <- update.formula(baseformula, ~. + basis_policy )

f2.1 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP )
f2.2 <- update.formula(baseformula, ~. + basis_GLH + basis_PREC)
f2.3 <- update.formula(baseformula, ~. + basis_GLH + basis_policy)
f2.4 <- update.formula(baseformula, ~. + basis_GLH + Vh)

f3.1 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP + basis_PREC)
f3.2 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP + basis_policy)
f3.3 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP + Vh)
f3.4 <- update.formula(baseformula, ~. + basis_GLH + basis_PREC + basis_policy)
f3.5 <- update.formula(baseformula, ~. + basis_GLH + basis_PREC + Vh)
f3.6 <- update.formula(baseformula, ~. + basis_GLH + basis_policy + Vh)

f4.1 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP + basis_PREC + basis_policy)
f4.2 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP + basis_PREC + Vh)
f4.3 <- update.formula(baseformula, ~. + basis_GLH + basis_TEMP + basis_policy + Vh)
f4.4 <- update.formula(baseformula, ~. + basis_GLH + basis_PREC + basis_policy + Vh)


# create a list of formulas
formulas <- list(f1.1, f1.2, f1.3, f1.4, 
                 f2.1, f2.2, f2.3, f2.4, 
                 f3.1, f3.2, f3.3, f3.4, f3.5, f3.6, 
                 f4.1, f4.2, f4.3, f4.4)

# create model label string
lab <- c("model_1.1", "model_1.2", "model_1.3", "model_1.4",
         "model_2.1", "model_2.2", "model_2.3", "model_2.4",
         "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5","model_3.6",
         "model_4.1", "model_4.2", "model_4.3", "model_4.4")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
              function(i) {
                model <- mymodel(formula = formulas[[i]], data = df, 
                                 family = "gamma", config = FALSE)
                save(model, file = paste0("output/whole country/", lab[i],".RData"))})

# create table to store DIC and select best model
table1 <- data.table(Model = c("model_1.1", "model_1.2", "model_1.3", "model_1.4",
                               "model_2.1", "model_2.2", "model_2.3", "model_2.4",
                               "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5","model_3.6",
                               "model_4.1", "model_4.2", "model_4.3", "model_4.4"),
                     DIC = NA,
                     logscore = NA)

for(i in 1:length(formulas)){
  load(paste0("output/whole country/",lab[i],".RData"))

  table1$DIC[i] <- round(model$dic$dic, 2)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}

# view table
table1

# define position of best fitting model
best.fit <- which.min(table1$DIC)

best.model <- table1$Model[best.fit] 
best.model
# Write results of model selection
fwrite(table1, file = "output/whole country/best_model_selection1.csv", quote = FALSE,
       row.names = FALSE)


#### test model priors ####
### best model 4.1 ###
#https://julianfaraway.github.io/brinlabook/chaglmm.html
library(brinla)

# use standrard deviation of outcome variable first 
sdr <- sd(df$R)
sdr

# use half Cauchy distribution for the standard deviation
(lambda <- 3*sdr/tan(pi*0.99/2)) # 0.01697901
halfcauchy <- "expression:
  lambda = 0.017;
  precision = exp(log_precision);
  logdens = -1.5*log_precision-log(pi*lambda) - log(1+1/(precision*lambda^2));  
  log_jacobian = log_precision;
  return(logdens+log_jacobian);"


precision.prior <- list(prec = list(prior="pc.prec", param = c(3*sdr,0.01)))#dic=1378.23
precision.prior <- list(prec = list(prior="pc.prec", param = c(2*sdr,0.01)))#dic=1378.11
precision.prior <- list(prec = list(prior="pc.prec", param = c(sdr,0.01))) #dic=1380.60

precision.prior <- list(prec = list(prior = halfcauchy))#dic=1384.96

precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))#dic=1377.563
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.001)))#dic=1380.94
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.1)))#dic=1378.18
precision.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.001))) #dic= 1378.00
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.1, 0.01))) #dic=1364.86
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.1, 0.001))) #dic=1362.94
precision.prior <- list(prec = list(prior = "pc.prec", param = c(1, 0.1))) 



# baseformula <- Y ~ 1 + f(W1, model= "rw1", hyper = precision.prior) + 
#   f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + 
#   Vp # 

formula <- Y ~ f(W1, model = "rw1", hyper = precision.prior) + 
  f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + 
  Vp + basis_GLH + basis_TEMP + basis_PREC + basis_policy


model <- mymodel(formula = formula, data = df, 
                 family = "gamma", config = FALSE)

summary(model)
model$dic$dic

post <- data.frame(model$marginals.fixed[[1]])
#post
ggplot(post, aes(x,y)) + geom_line()

#model$summary.fixed

bri.hyperpar.summary(model)
bri.hyperpar.plot(model)


