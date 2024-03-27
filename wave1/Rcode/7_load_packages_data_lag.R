## R script to prepare data and lagged variables for INLA-DLNM modelling
# rm(list=ls())

setwd("wave1")

# install INLA
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

# load INLA
library(INLA)

#  select other packages
packages <- c("data.table", "tidyverse", "sf", "sp", "spdep",
              "dlnm", "tsModel", "hydroGOF","RColorBrewer", 
              "geofacet", "ggpubr", "ggthemes")

# install.packages
# lapply(packages, install.packages, character.only = TRUE)

# load packages
lapply(packages, library, character.only = TRUE)

## load data for wave 1 ----
#### Climate and case/intervention/holiday data in the first week for lagged values 
## 1-week lag
#data_all <- fread("data/data_weekly_all_lag1week_merged_wave1.csv", stringsAsFactors = F)
## 2-week lag
data_all <- fread("data/data_weekly_all_lag2week_merged_wave1.csv", stringsAsFactors = F)

### urban, semiurban, rural subsets
# data_all <- data_all[data_all$DEGURBA_L1 =='Urban',]
# data_all <- data_all[data_all$DEGURBA_L1 =='Suburban',]
 data_all <- data_all[data_all$DEGURBA_L1 =='Rural',]

data_all <- data_all[order(data_all$date_begin, data_all$GID_2),]
data <- data_all
str(data)

## correlation for all data in India ----
# install.packages("corrplot")
library(corrplot)

data.m <- data[,c("R0", "R", "ln_R", "cases", "cases_rate", 'cases_accu', 'cases_accu_rate',
                  "mean_intra", 
                  "d2m", "t2m", "tp", "uv",  "Stringency", 
                  "holiday", "pop_sum", "pop_density")]
M <- cor(data.m)
head(round(M,2))

#pdf("figs_lag/fig_S_correlation_matrix.pdf", width = 11, height = 10, onefile = T)
pdf("figs_lag_2wk/fig_S_correlation_matrix.pdf", width = 11, height = 10, onefile = T)
corrplot(M, method="circle")
corrplot(M, method="pie")
corrplot(M, method="color")
corrplot(M, method="number")
corrplot(M, type="lower", hclust.method = 'complete')
corrplot(M, type="lower", order = 'hclust',hclust.method = 'complete')
dev.off()

res <- cor(data.m)
round(res, 2)

## ~~ test glm ----

fit = glm(R  ~ cases_accu_rate + mean_intra + d2m + t2m + tp + uv + Stringency + holiday, 
          data = data_all, family = 'gaussian') #AIC: 9616.6
summary(fit)

fit = glm(R  ~ cases_accu_rate + mean_intra + d2m + tp + uv + Stringency + holiday, 
          data = data_all, family = 'gaussian') #AIC: 9616.4
summary(fit)

fit = glm(R  ~ cases_accu_rate + mean_intra + t2m + tp + uv + Stringency + holiday, 
          data = data_all, family = 'gaussian') #AIC: 9632.7
summary(fit)

fit = glm(R ~ 1 +  cases_accu_rate + mean_intra + t2m + tp + uv + Stringency + 
            pop_density + holiday + offset(log(R0)), 
          data = data_all, family = Gamma(link = "log")) #AIC: 4751.9
summary(fit)

fit = glm(R ~ 1 + cases_accu_rate + mean_intra + t2m + tp + Stringency +  
            offset(log(R0)), 
          data = data_all, family = gaussian(link = "log")) #AIC: 9479.3
summary(fit)

fit = lm(ln_R ~ cases_accu_rate + mean_intra + t2m + tp + Stringency + holiday, 
          data = data_all) #Adjusted R-squared:  0.07763 
summary(fit)

fit = lm(ln_R ~ cases_accu_rate + mean_intra + uv + tp + Stringency + holiday, 
         data = data_all) #Adjusted R-squared:  0.06866 
summary(fit)

## Evaluate Collinearity of variates
# par(mar = c(5, 4, 2, 4))
# hist(data_all$R)
# hist(data_all$R/data_all$R0)
# hist(data_all$ln_R)

fit = glm(R ~ 1 + cases_accu_rate + mean_intra + d2m + t2m + tp + uv + Stringency + 
            holiday + offset(log(R0)), 
          data = data_all, family = Gamma(link = "log")) 
fit$aic #4732.709

fit = glm(R ~ cases_accu_rate + mean_intra + t2m + tp + uv + Stringency + 
            holiday + offset(log(R0)), 
          data = data_all, family = Gamma(link = "log")) 
fit$aic # 4765.877

fit = glm(R ~ cases_accu_rate + mean_intra + d2m + tp + uv + Stringency + 
            holiday + offset(log(R0)), 
          data = data_all, family = Gamma(link = "log")) 
fit$aic # 4731.153

fit = glm(R ~ cases_accu_rate + mean_intra + t2m + tp + Stringency + 
            holiday + offset(log(R0)), 
          data = data_all, family = Gamma(link = "log")) 
fit$aic # 4781.551

fit = glm(R ~ cases_accu_rate + mean_intra + t2m + uv + Stringency + 
            holiday + offset(log(R0)), 
          data = data_all, family = Gamma(link = "log")) 
fit$aic # 4841.552


anova(fit, test = 'Chisq')
deviance(fit)/fit$df.residual 

library(car)
round(vif(fit),1) # variance inflation factors 
round(vif(fit),1) >= 2.5 # problem? - d2m / uv

### Create adjacency matrix ------
## load shape file ----
map_all <- read_sf("data/shapefile/gadm36_IND_2.shp")
map <- map_all[map_all$GID_2 %in% unique(data_all$GID_2),]
# dim(map)

# Create adjacency matrix
sf::sf_use_s2(FALSE) # to fix 4 features with invalid spherical geometry
nb.map <- poly2nb(as_Spatial(map$geometry))
g.file <- "output_lag/map.graph"
# if (!file.exists(g.file)) nb2INLA(g.file, nb.map)
nb2INLA(g.file, nb.map)

g <- inla.read.graph(filename = g.file)

# load pre-defined grid of Chinese provinces for geofacet plots
# grid <- read.csv("data/Provinces.csv")

## integrate data
# Create lagged variables
# define matrices of lagged terms for climate, covid and intervention variables

# set max lag - by week
nlag = 3

# domestic mobility
lag_GLH <- tsModel::Lag(data_all$mean_intra, group = data_all$GID_2, k = 0:nlag)
## humidity
lag_HUMI <- tsModel::Lag(data_all$d2m, group = data_all$GID_2, k = 0:nlag)
## temperature
lag_TEMP <- tsModel::Lag(data_all$t2m, group = data_all$GID_2, k = 0:nlag)
## precipitation
lag_PREC <- tsModel::Lag(data_all$tp, group = data_all$GID_2, k = 0:nlag)
## UV
lag_UV <- tsModel::Lag(data_all$uv, group = data_all$GID_2, k = 0:nlag)

## covid intervention stringency
lag_policy <- tsModel::Lag(data_all$Stringency, group = data_all$GID_2, k = 0:nlag)

# Remove weeks from lagged variables
lag_GLH <- lag_GLH[is.na(data_all$week)==F,]
lag_HUMI <- lag_HUMI[is.na(data_all$week)==F,]
lag_TEMP <- lag_TEMP[is.na(data_all$week)==F,]
lag_PREC <- lag_PREC[is.na(data_all$week)==F,]
lag_UV <- lag_UV[is.na(data_all$week)==F,]
lag_policy <- lag_policy[is.na(data_all$week)==F,]

data <- data_all[is.na(data_all$week)==F,]
head(data)

## cross-basis matrix ----
# define dimensions 
# re-define time indicator 
unique(data$weeknum)

# total number of weeks
nweek <- length(unique(data$weeknum))
# total number of district
ncity <- length(unique(data$GID_2))
# total number of provinces
nprov <- length(unique(data$GID_1))

# define cross-basis matrix (combining nonlinear exposure and lag functions)
# set lag knots
lagknot = equalknots(0:nlag, 2)

# Humidity / fun 'ns' - Generate a Basis Matrix for Natural Cubic Splines
var <- lag_HUMI
basis_HUMI <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$d2m, 2)),
                         arglag = list(fun = "ns", knots = nlag/2))
head(basis_HUMI)

# Temporature / fun 'ns' - Generate a Basis Matrix for Natural Cubic Splines
var <- lag_TEMP
basis_TEMP <- crossbasis(var,
                    argvar = list(fun = "ns", knots = equalknots(data$t2m, 2)),
                    arglag = list(fun = "ns", knots = nlag/2))
head(basis_TEMP)

# Precipitation
var <- lag_PREC
basis_PREC <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$tp, 2)),
                         arglag = list(fun = "ns", knots = nlag/2))
head(basis_PREC)

# UV
var <- lag_UV
basis_UV <- crossbasis(var,
                         argvar = list(fun = "ns", knots = equalknots(data$uv, 2)),
                         arglag = list(fun = "ns", knots = nlag/2))
head(basis_PREC)

# mobility
var <- lag_GLH
basis_GLH <- crossbasis(var,
                         argvar = list(fun="ns", knots = equalknots(data$mean_intra, 2)),
                         arglag = list(fun="ns", knots = lagknot))
head(basis_GLH)

# stringency
var <- lag_policy
basis_policy <- crossbasis(var,
                        argvar = list(fun="ns", knots = equalknots(data$Stringency, 2)),
                        arglag = list(fun="ns", knots = lagknot))
head(basis_policy)

# assign unique column names to cross-basis matrix for inla() model
# note: not necessary for glm(), gam() or glm.nb() models
colnames(basis_HUMI) = paste0("basis_HUMI.", colnames(basis_TEMP))
colnames(basis_TEMP) = paste0("basis_TEMP.", colnames(basis_TEMP))
colnames(basis_PREC) = paste0("basis_PREC.", colnames(basis_PREC))
colnames(basis_UV) = paste0("basis_UV.", colnames(basis_UV))
colnames(basis_GLH) = paste0("basis_GLH.", colnames(basis_GLH))
colnames(basis_policy) = paste0("basis_policy", colnames(basis_policy))

head(basis_GLH)


# create indices for INLA models
# note: for INLA models an index should start with 1 and with the max value equal to the length of unique values

# create district index 
data$city_index <- rep(1:ncity, nweek)

# create province index
# state length
k <- unique(data$GID_1)

for (j in 1:nprov){
  data$prov_index[data$GID_1 == k[j]] <- j 
}

# create week index
# set first week for modelling to 1
data$week_index <- data$weeknum

#### set up data and priors for INLA model ----
## set data for models 
Y  <- data$R # response variable
ln_R  <- data$ln_R # response variable
R  <- data$R # response variable
R0  <- data$R0 # expected outcome
N  <- length(Y) # total number of data points
## random variable
W1 <- data$week_index # for random effect to account for inter-week variability
S1 <- data$city_index # for district-level spatial random effect
S2 <- data$prov_index # for state interaction with daily random effect

## fixed effect variables without DLNMs 
#  basis_GLH + basis_TEMP + basis_UV + basis_policy
GLH <- data$mean_intra
HUMI <- data$d2m
TEMP <- data$t2m
PREC <- data$tp
UV <- data$uv
policy <- data$Stringency

## Other fixed effect variables
Vp  <- data$cases_accu_rate 
Vh <- data$holiday # holiday 

# create dataframe for model testing
df <- data
df$Y <- df$R

df <- df[, c('Y', 'R', 'R0', 'ln_R', 'week_index', 'city_index', 'prov_index',
               'mean_intra', 'd2m', 't2m', 'tp', 'uv', 'Stringency', 'cases_accu_rate', 'holiday')]
colnames(df) <- c('Y', 'R', 'R0', 'ln_R', 'W1', 'S1', 'S2', 
                  'GLH', 'HUMI', 'TEMP', 'PREC', 'UV', 'policy', 'Vp', 'Vh')

# df <- data.frame(Y, R, R0, ln_R, W1, S1, S2, GLH, TEMP, PREC, UV, policy, Vp, Vh)
head(df)

## ~~ test model ----
# df1 <- df
# formula <- Y ~ f(S1, model = "bym2", graph = g) +
#   f(S1.1, W1, model = "iid") + W1
# 
# res <- inla(formula,
#             family = "Gaussian", data = df1, 
#             control.predictor = list(compute = TRUE))
# 
# df1$RR <- res$summary.fitted.values[, "mean"]
# df1$LL <- res$summary.fitted.values[, "0.025quant"]
# df1$UL <- res$summary.fitted.values[, "0.975quant"]

# define priors
precision.prior <- list(prec = list(prior = "pc.prec", param = c(0.5, 0.01)))
# precision.prior <- list(prec = list(prior = "loggamma", param = c(1, 0.01)))
# precision.prior2 <- list(prec = list(prior = "pc.prec", param = c(1, 0.01)))

# inla model function

# include formula and set defaults for data, family (to allow other prob dist models e.g. Poisson) and config (to allow for sampling)
# mymodel <- function(formula, data = df, family = "Gaussian", config = FALSE){
mymodel <- function(formula, data = df, family = "gamma", config = FALSE){
  model <- inla(formula = formula, data = data, family = family, 
                offset = log(R0),
                control.inla = list(strategy = 'adaptive'), 
                # control.inla = list(strategy = 'adaptive',int.strategy='eb'),
                control.compute = list(dic = TRUE, config = config, 
                                       cpo = TRUE, return.marginals = FALSE),
                control.fixed = list(correlation.matrix = TRUE, 
                                     prec.intercept = 1, prec = 1),
                control.predictor = list(link = 1, compute = TRUE), 
                control.family = list(link = "log"), # log link between y ~ x
                verbose = FALSE)
  model <- inla.rerun(model)
  return(model)

}


