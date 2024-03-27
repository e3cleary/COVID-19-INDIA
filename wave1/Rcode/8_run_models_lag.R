# R script to run INLA models of increasing complexity
# WARNING: the script may take over a day to run

# Step 0: load packages and pre-processed data 
# Step 1: formulate a baseline model including spatiotemporal random effects and test different combinations of DLNM variables

## Step 0: load packages and pre-processed data
# source("9_load_packages_data_lag.R")

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


baseformula <- Y ~ 1 + f(W1, model= "rw1", hyper = precision.prior) + 
  f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + 
  Vp # dic gamma 7221 / gaussian 13411 / weibull 8444

# baseformula <- Y ~ f(W1, model= "rw1", hyper = precision.prior) +
#   f(S1, model = "bym2", scale.model = TRUE, graph = g, hyper = precision.prior) + Vp +
#   GLH + TEMP + UV + policy 
# # gamma 6664 / gaussian 13076 / weibull 7855

# test baseline model
model <- mymodel(formula = baseformula, data = df, family = 'gamma', config = FALSE)
model$dic$dic
-mean(log(model$cpo$cpo))

summary(model)

save(model, file = "output_lag/whole country/basemodel.RData")


## define formulas by updating the baseline formula with different combinations of
# cross-basis functions of temperature, precipitation, covid cases, and intervention index

f1.1 <- update.formula(baseformula, ~. + GLH )
f1.2 <- update.formula(baseformula, ~. + TEMP )
f1.3 <- update.formula(baseformula, ~. + UV )
f1.4 <- update.formula(baseformula, ~. + policy )

f2.1 <- update.formula(baseformula, ~. + GLH + TEMP )
f2.2 <- update.formula(baseformula, ~. + GLH + UV)
f2.3 <- update.formula(baseformula, ~. + GLH + policy)
f2.4 <- update.formula(baseformula, ~. + GLH + Vh)

f3.1 <- update.formula(baseformula, ~. + GLH + TEMP + UV)
f3.2 <- update.formula(baseformula, ~. + GLH + TEMP + policy)
f3.3 <- update.formula(baseformula, ~. + GLH + TEMP + Vh)
f3.4 <- update.formula(baseformula, ~. + GLH + UV + policy)
f3.5 <- update.formula(baseformula, ~. + GLH + UV + Vh)
f3.6 <- update.formula(baseformula, ~. + GLH + policy + Vh)

f4.1 <- update.formula(baseformula, ~. + GLH + TEMP + UV + policy)
f4.2 <- update.formula(baseformula, ~. + GLH + TEMP + UV + Vh)
f4.3 <- update.formula(baseformula, ~. + GLH + TEMP + policy + Vh)
f4.4 <- update.formula(baseformula, ~. + GLH + UV + policy + Vh)

# 
# # create a list of formulas
formulas <- list(f1.1, f1.2, f1.3, f1.4,
                 f2.1, f2.2, f2.3, f2.4,
                 f3.1, f3.2, f3.3, f3.4, f3.5, f3.6,
                 f4.1, f4.2, f4.3, f4.4)

# create model label string
lab <- c("model_1.1", "model_1.2", "model_1.3", "model_1.4",
         "model_2.1", "model_2.2", "model_2.3", "model_2.4",
         "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5","model_3.6",
         "model_4.1", "model_4.2", "model_4.3", "model_4.4")

#################################################################################

## define formulas by updating the baseline formula with different combinations of
# cross-basis functions of temperature, precipitation, covid cases, and intervention index

# f1.1 <- update.formula(baseformula, ~. + GLH )
# f1.2 <- update.formula(baseformula, ~. + HUMI )
# f1.3 <- update.formula(baseformula, ~. + TEMP )
# f1.4 <- update.formula(baseformula, ~. + UV )
# f1.5 <- update.formula(baseformula, ~. + policy )
# 
# f2.1 <- update.formula(baseformula, ~. + GLH + HUMI )
# f2.2 <- update.formula(baseformula, ~. + GLH + TEMP )
# f2.3 <- update.formula(baseformula, ~. + GLH + UV)
# f2.4 <- update.formula(baseformula, ~. + GLH + policy)
# f2.5 <- update.formula(baseformula, ~. + GLH + Vh)
# 
# f3.1 <- update.formula(baseformula, ~. + GLH + HUMI + UV)
# f3.2 <- update.formula(baseformula, ~. + GLH + HUMI + policy)
# f3.3 <- update.formula(baseformula, ~. + GLH + HUMI + Vh)
# f3.4 <- update.formula(baseformula, ~. + GLH + TEMP + UV)
# f3.5 <- update.formula(baseformula, ~. + GLH + TEMP + policy)
# f3.6 <- update.formula(baseformula, ~. + GLH + TEMP + Vh)
# f3.7 <- update.formula(baseformula, ~. + GLH + UV + policy)
# f3.8 <- update.formula(baseformula, ~. + GLH + UV + Vh)
# f3.9 <- update.formula(baseformula, ~. + GLH + policy + Vh)
# 
# f4.1 <- update.formula(baseformula, ~. + GLH + HUMI + UV + policy)
# f4.2 <- update.formula(baseformula, ~. + GLH + HUMI + UV + Vh)
# f4.3 <- update.formula(baseformula, ~. + GLH + HUMI + policy + Vh)
# f4.4 <- update.formula(baseformula, ~. + GLH + TEMP + UV + policy)
# f4.5 <- update.formula(baseformula, ~. + GLH + TEMP + UV + Vh)
# f4.6 <- update.formula(baseformula, ~. + GLH + TEMP + policy + Vh)
# f4.7 <- update.formula(baseformula, ~. + GLH + UV + policy + Vh)
# 
# 
# # create a list of formulas
# formulas <- list(f1.1, f1.2, f1.3, f1.4, f1.5,
#                  f2.1, f2.2, f2.3, f2.4, f2.5,
#                  f3.1, f3.2, f3.3, f3.4, f3.5, f3.6, f3.7, f3.8, f3.9, 
#                  f4.1, f4.2, f4.3, f4.4, f4.5, f4.6, f4.7)

# create model label string
# lab <- c("model_1.1", "model_1.2", "model_1.3", "model_1.4", "model_1.5",
#          "model_2.1", "model_2.2", "model_2.3", "model_2.4", "model_2.5",
#          "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5",
#          "model_3.6", "model_3.7", "model_3.8", "model_3.9",
#          "model_4.1", "model_4.2", "model_4.3", "model_4.4",
#          "model_4.5", "model_4.6", "model_4.7")

# create a function to run a model for each formula in the list and save the model output to file
# WARNING: this may take a long time to run
models <- lapply(1:length(formulas),
              function(i) {
                model <- mymodel(formula = formulas[[i]], data = df, 
                                 family = "gamma", config = FALSE)
                save(model, file = paste0("output_lag/whole country/", lab[i],".RData"))})

# create table to store DIC and select best model
table1 <- data.table(Model = c("model_1.1", "model_1.2", "model_1.3", "model_1.4",
         "model_2.1", "model_2.2", "model_2.3", "model_2.4",
         "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5","model_3.6",
         "model_4.1", "model_4.2", "model_4.3", "model_4.4"),
         DIC = NA,
         logscore = NA)


# table1 <- data.table(Model = c("model_1.1", "model_1.2", "model_1.3", "model_1.4", "model_1.5",
#                                "model_2.1", "model_2.2", "model_2.3", "model_2.4", "model_2.5",
#                                "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5",
#                                "model_3.6", "model_3.7", "model_3.8", "model_3.9",
#                                "model_4.1", "model_4.2", "model_4.3", "model_4.4",
#                                "model_4.5", "model_4.6", "model_4.7"),
#                      DIC = NA,
#                      logscore = NA)

for(i in 1:length(formulas)){
  load(paste0("output_lag/whole country/",lab[i],".RData"))

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
fwrite(table1, file = "output_lag/whole country/best_model_selection1.csv", quote = FALSE,
       row.names = FALSE)

