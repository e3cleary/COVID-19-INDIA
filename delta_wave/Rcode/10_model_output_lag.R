## R script to visualise INLA-DLMN model output

# Step 0: load packages and pre-processed data
# Step 1: compare models using goodness of fit statistics
# Step 2: visualise random effects for selected model
# Step 3: compare selected model to the baseline model using the mean absolute error
# Step 4: compare cross-validated posterior predictions to observations

# Step 0: load packages and pre-processed data

source("9_load_packages_data_lag.R")

# load pre-defined grid of states for geofacet plots
grid <- fread("data/India_state_grid.csv")
state <- fread("data/States.csv")

# load shape file 
map_all <- read_sf("data/shapefile/gadm36_IND_2.shp")
map <- map_all[map_all$GID_2 %in% unique(data_all$GID_2),]

data <- merge(data, state[,c('GID_1', 'region', 'NAME_1')], by='GID_1', all.x=T)
data <- data[order(data$week_index, data$GID_2),]

### for mapping
case.map <- ggplot() +
  # xlim(-165, 165)+
  # ylim(-52, 78) +
  xlab('') + ylab('') +  theme_bw() + 
  theme(legend.position = c(0.7,0.2), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme( axis.text  = element_blank(), axis.ticks = element_blank(), 
         axis.title = element_text(size = 15),
         title = element_text(size=18),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 

### Step 1: compare models using goodness of fit statistics ----
# create a table to store model adequacy results (DIC, CV log score, MAE difference results)
# create model label string
lab <- c("model_1.1", "model_1.2", "model_1.3", "model_1.4",
         "model_2.1", "model_2.2", "model_2.3", "model_2.4",
         "model_3.1", "model_3.2", "model_3.3", "model_3.4", "model_3.5","model_3.6",
         "model_4.1", "model_4.2", "model_4.3", "model_4.4")

mod.name <- c("basemodel", lab)

table1 <- data.table(Model = mod.name, 
                     DIC = NA,
                     logscore = NA)

# create loop to read in model object and extract DIC and CV log score
for (i in 1:length(mod.name)){
  load(paste0("output_lag/", region_subset, "/", mod.name[i],".RData"))
  # add model fit statistics from each model to table
  table1$DIC[i] <- round(model$dic$dic,0)
  table1$logscore[i] <- round(-mean(log(model$cpo$cpo), na.rm = T), 3)
}
table1

# save model adequacy results for all models (Appendix Table S1) 
fwrite(table1, file = paste0("figs_lag/", region_subset, "/table_S01.csv"), quote = FALSE, 
       row.names = FALSE) 

# load baseline and selected best fit model
load(paste0("output_lag/", region_subset, "/basemodel.RData"))
basemodel <- model
summary(basemodel)


best.model <- table1$Model[which.min(table1$DIC)] 
best.model

load(paste0("output_lag/", region_subset, "/", best.model, ".RData"))
summary(model)



### Step 2: visualise random effects for selected model ----
## explore spatial and temporal random effects 

# temporal random effects per week
daily_effects <- data.table(model$summary.random$W1)

write.csv(daily_effects,file = paste0("figs_lag/", region_subset, "/table_S02_week_effectsi.csv"),row.names = F)

# make maps of spatial random effects  
# extract posterior mean estimates for combined unstructured and structured random effects
n_cities <- length(unique(data$GID_2))
nyear <- length(unique(data$year))

space <- data.table(model$summary.random$S1)
space$year <- rep(min(data$year):max(data$year), each = 2*n_cities)
space$re <- rep(c(rep(1, n_cities),rep(2, n_cities)),nyear)
space <- space[space$re == 1,]
space$GID_2 <- rep(unique(data$GID_2), nyear)
mn <-min(space$mean)
mx <-max(space$mean)

write.csv(space, file = paste0("figs_lag/", region_subset, "/table_S03_spatial random effects per wave.csv"),row.names = F)

# Add the map geometry to the space dataframe
space <- left_join(map, space, by = c("GID_2" = "GID_2"))
space_effects <- ggplot() + 
  geom_sf(data = space, aes(fill = mean), lwd = 0, color = NA) +
  scale_fill_distiller(palette = "PiYG", direction = -1, 
                       limits = c(min(mn,-mx),max(mx,-mn))) +
  labs(fill = "Contribution to\nChanges in Rt)") +
  theme_void() +
  facet_wrap(~space$year, ncol = 5)
  
ggsave(paste0("figs_lag/", region_subset, "/fig_S05_week_spatial_effect.pdf"), height = 20, width = 30, units = "cm")

### Step 3: compare selected model to the baseline model using the mean absolute error ----
# add baseline fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$base.fit <- basemodel$summary.fitted.values$`0.5quant`
data$base.fit.lci <- basemodel$summary.fitted.values$`0.025quant`
data$base.fit.uci <- basemodel$summary.fitted.values$`0.975quant`

# add selected fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$fit <- model$summary.fitted.values$`0.5quant`
data$fit.lci<-model$summary.fitted.values$`0.025quant`
data$fit.uci<-model$summary.fitted.values$`0.975quant`
head(data)

# compute mean absolute error (MAE) and compare base model to final model
MAE <- as.data.frame(matrix(NA, nrow = n_cities, ncol = 2))
names(MAE) <-c("base", "new")
  
# calculate the MAE for observed and mean fit Rt
for (i in 1:n_cities){
  # Rt
  MAE$base[i] <- hydroGOF::mae(data$base.fit[data$city_index == i], 
                               data$R[data$city_index == i], # response variable
                               na.rm = TRUE)
  MAE$new[i] <- hydroGOF::mae(data$fit[data$city_index == i], 
                              data$R[data$city_index == i], # response variable
                              na.rm = TRUE)
}
  
# calculate difference between MAE from the baseline model and MAE from the selected model
MAE$diff <- MAE$base - MAE$new
mn <-min(MAE$diff)
mx <-max(MAE$diff)
write.csv(MAE,file = paste0("figs_lag/", region_subset, "/table_S04_MAEi.csv"),row.names = F)

MAE$value <- 1

# Specify the region where the difference is greater than or equal to 0 as '2'
# that is, the MAE of the new model is smaller (better than that of the base model)
MAE$value[MAE$diff >= 0 ] <- 2
head(MAE)

# plot map to show areas where the new model provided 'Improved' over the basemodel (e.g. MAE is smaller for new model) 

value_map <- case.map + 
  geom_sf(data = map_all, aes(), fill=NA, colour='grey', lwd = 0.1) +
  geom_sf(data = map, aes(fill = factor(MAE$value)), alpha=0.6, lwd = 0) +
  scale_fill_manual(values = c("blue","red"), breaks = 1:2, 
                    labels = c("No improved", "Improved")) +
  labs(fill = "") +
  theme_void() + theme(legend.position = c(0.7,0.3))

  
ggsave(value_map, filename = paste0("figs_lag/", region_subset, "/fig_S06_value_map.pdf"),  width=6, height=7)

# create a table to store the number and proportion of cities where the selected model fits better than the baseline model
tableS2 <- as.data.frame(matrix(NA, 7, 3))
# add column names to table
names(tableS2) <- c("Region", "Improved", "Total")
# add model names to first column
tableS2[,1] <-  c(unique(state$region), "India")

# extract region names for first time step
region <- data$region[data$week_index == 1]
# create unique region name vector
k <- unique(region)

for (i in 1:length(k)){
  
  # number of cities where the selected model fits better than the baseline model
  tableS2$`Improved`[i] <- length(MAE$value[MAE$value == 2 & region == k[i]])
  # total number of cities
  tableS2$Total[i] <- length(region[region == k[i]])
  
}

# India
tableS2[7,2] <- length(MAE$value[MAE$value == 2])
tableS2[7,3] <- n_cities

# calculate proportion
tableS2$Proportion <- round(tableS2$`Improved`/tableS2$Total *100, 1)

tableS2

# Appendix Table S2
write.csv(tableS2, file = paste0("figs_lag/", region_subset, "/table_S05_Added_value_region.csv"), quote = FALSE, 
          row.names = FALSE) 


### Step 4: compare best model's fitted.values to observations ----
data <- data[order(data$date_begin, data$GID_2),]

data$fit.mean <-model$summary.fitted.values$mean
data$fit.sd <-model$summary.fitted.values$sd
data$fit.median <- model$summary.fitted.values$`0.5quant`
data$fit.lci<-model$summary.fitted.values$`0.025quant`
data$fit.uci<-model$summary.fitted.values$`0.975quant`

## plot maps of fitted mean Rt per week (note, many graphs)
data.w <- merge(map, data[, c('week_index', 'week', 'GID_2', 'fit.mean', 'R')], by = 'GID_2')

# fitted Rt
Rt_pred_week <- ggplot(data.w) +
  geom_sf(mapping = aes(fill = fit.mean), lwd = 0, color = NA) +
  scale_fill_gradientn(name = "Fitted Rt", colours = brewer.pal(9, "RdPu"),
                       trans = "log1p",
                       breaks = c(0, 0.5, 1, 2, 4),
                       labels = c(0, 0.5, 1, 2, 4) ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void()+ theme(legend.position = c(0.7,0.15))

pdf(paste0("figs_lag/", region_subset, "/fig_S07_maps_fitted_Rt_by_week.pdf"), height = 8, width = 12)
Rt_pred_week
dev.off()


## plot observed v fitted Rt per state
state.capital <- unique(data[,c('GID_1', 'GID_2')])
state.capital <- state.capital[str_sub(state.capital$GID_2, -4)=='.1_1',]

data.cap <- data[data$GID_2 %in% state.capital$GID_2,]
data.cap <- merge(data.cap, grid, by.x="GID_1", by.y="code_num")

Rt_ts_prov <- ggplot(data=data.cap) +
  geom_hline(yintercept = 1, colour = 'grey' , alpha=0.8, size =0.7) +
  geom_ribbon(aes(x = week, ymin = fit.lci, ymax = fit.uci),
              fill = "#D37295", alpha = 0.5) +
  geom_line(aes(x = week, y = fit.mean, col = "#D37295")) +
  geom_line(aes(x = week, y = R, col = "#499894")) +
  xlab("Week") +
  ylab("Rt") +
  scale_colour_identity(name = "",
                        breaks = c("#499894", "#D37295"),
                        labels = c("Observed", "Posterior fitted mean with 95%CI"),
                        guide = "legend") +
  # scale_x_continuous(breaks = seq(1, nweek),
  #                    labels = seq(min(data$week), max(data$week))) +
  scale_y_continuous(breaks = c(0, 1, 2, 4),
                     trans = "log1p",
                     labels = c(0, 1, 2, 4)) +
  # organise by state name in grid file
  facet_geo( ~ name, grid = grid) +
  # plot theme
  # theme(axis.text.x = element_text(size = rel(0.85)), legend.position = "bottom") +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank())  +
  theme(legend.position=c(0.8,0.06), legend.background = element_blank())+
  theme( axis.text  = element_text(size=14,colour = 'black'),
         axis.title = element_text(size = 15),
         title = element_text(size=16),
         legend.title = element_text(size = 11),
         legend.text = element_text(size=14)) +
  theme(plot.margin = unit(c(0.5,1,1,1), "cm"))


pdf(paste0("figs_lag/", region_subset, "/fig_S08_Rt_obs_vs_fitted_state.pdf"), height = 12, width = 12)
Rt_ts_prov
dev.off()


### Step 5: compare cross-validated posterior predictions to observations ----
# add cross-validation posterior predictive summaries (already processed) to data

## see 11_run_models_leave_one_week_out_lag.R for script to perform cross-validation in parallel (note, very computationally intensive)

data$pred.mean <- NA
data$pred.sd <- NA
data$pred.median <- NA
data$pred.lci <- NA
data$pred.uci <- NA

data <- data[order(data$week_index, data$GID_2),]

i=1
for (i in 1:nweek){ # no of weeks
    load(paste0("output_lag/", region_subset, "/preds/preds_week", i, ".RData"))

    data$pred.mean[preds$idx.pred] <- preds$mean
    data$pred.sd[preds$idx.pred] <- preds$sd
    data$pred.median[preds$idx.pred] <- preds$median
    data$pred.lci[preds$idx.pred] <- preds$lci
    data$pred.uci[preds$idx.pred] <- preds$uci

}

## plot maps of observed and CV posterior mean RT per week (note, many graphs)
data.w <- merge(map, data[, c('week_index', 'week', 'GID_2', 
                              'pred.mean', 'pred.lci', 'pred.uci', 'pred.sd', 'R')], by = 'GID_2')

Rt_week <- ggplot(data.w) +
  geom_sf(mapping = aes(fill = R), lwd = 0, color = NA) +
  scale_fill_gradientn(name = "Rt", colours = brewer.pal(9, "RdPu"),
                       trans = "log1p",
                       breaks = c(0, 0.5, 1, 2, 4),
                       labels = c(0, 0.5, 1, 2, 4) ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))
  # xlab('') + ylab('') +  theme_bw() +
  # theme(legend.position = c(0.7,0.2), legend.background = element_blank()) +
  # theme(panel.grid.major = element_line(colour = 'transparent')) +
  # theme( axis.text  = element_blank(), axis.ticks = element_blank(),
  #        axis.title = element_text(size = 15),
  #        title = element_text(size=18),
  #        legend.title = element_text(size = 14),
  #        legend.text = element_text(size=14))

pdf(paste0("figs_lag/", region_subset, "/fig_S07_maps_Rt_by_week.pdf"), height = 8, width = 12)
Rt_week
dev.off()

# Predicted Rt
Rt_pred_week <- ggplot(data.w) +
  geom_sf(mapping = aes(fill = pred.mean), lwd = 0, color = NA) +
  scale_fill_gradientn(name = "Predicted Rt", colours = brewer.pal(9, "RdPu"),
                       trans = "log1p",
                       breaks = c(0, 0.5, 1, 2, 4),
                       labels = c(0, 0.5, 1, 2, 4) ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void()  + theme(legend.position = c(0.7,0.15))

pdf(paste0("figs_lag/", region_subset, "/fig_S07_maps_pred_Rt_mean_by_week.pdf"), height = 8, width = 12)
Rt_pred_week
dev.off()

## sd
Rt_pred_week <- ggplot(data.w) +
  geom_sf(mapping = aes(fill = pred.sd), lwd = 0, color = NA) +
  scale_fill_distiller(name = "SD",  palette="PuBu", direction=1) +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf(paste0("figs_lag/", region_subset, "/fig_S07_maps_pred_Rt_sd_by_week.pdf"), height = 8, width = 12)
Rt_pred_week
dev.off()

## Lower CI
Rt_pred_week <- ggplot(data.w) +
  geom_sf(mapping = aes(fill = pred.lci), lwd = 0, color = NA) +
  scale_fill_gradientn(name = "Predicted Rt", colours = brewer.pal(9, "RdPu"),
                       trans = "log1p",
                       breaks = c(0, 0.5, 1, 2, 4),
                       labels = c(0, 0.5, 1, 2, 4) ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf(paste0("figs_lag/", region_subset, "/fig_S07_maps_pred_Rt_lci_by_week.pdf"), height = 8, width = 12)
Rt_pred_week
dev.off()

# Upper CI
Rt_pred_week <- ggplot(data.w) +
  geom_sf(mapping = aes(fill = pred.uci), lwd = 0, color = NA) +
  scale_fill_gradientn(name = "Predicted Rt", colours = brewer.pal(9, "RdPu"),
                       trans = "log1p",
                       breaks = c(0, 0.5, 1, 2, 4),
                       labels = c(0, 0.5, 1, 2, 4) ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf(paste0("figs_lag/", region_subset, "/fig_S07_maps_pred_Rt_uci_by_week.pdf"), height = 8, width = 12)
Rt_pred_week
dev.off()


## plot observed v predicted Rt per state capital
state.capital <- unique(data[,c('GID_1', 'GID_2')])
state.capital <- state.capital[str_sub(state.capital$GID_2, -4)=='.1_1',]

data.cap <- data[data$GID_2 %in% state.capital$GID_2,]
data.cap <- merge(data.cap, grid, by.x="GID_1", by.y="code_num")

Rt_ts_prov <- ggplot(data=data.cap) +
  geom_hline(yintercept = 1, colour = 'grey' , alpha=0.8, size =0.7) +
  geom_ribbon(aes(x = week, ymin = pred.lci, ymax = pred.uci),
              fill = "#D37295", alpha = 0.5) +
  geom_line(aes(x = week, y = pred.mean, col = "#D37295")) +
  geom_line(aes(x = week, y = R, col = "#499894")) +
  xlab("Week") +
  ylab("Rt") +
  scale_colour_identity(name = "",
                        breaks = c("#499894", "#D37295"),
                        labels = c("Observed", "Posterior predicted mean and 95%CI"),
                        guide = "legend") +
  # scale_x_continuous(breaks = seq(1, nweek),
  #                    labels = seq(min(data$week), max(data$week))) +
  scale_y_continuous(breaks = c(0, 1, 2, 4),
                     trans = "log1p",
                     labels = c(0, 1, 2, 4)) +
  # organise by state name in grid file
  facet_geo( ~ name, grid = grid) +
  # plot theme
  # theme(axis.text.x = element_text(size = rel(0.85)), legend.position = "bottom") +
  theme_bw() + theme(panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank())  +
  theme(legend.position=c(0.8,0.06), legend.background = element_blank())+
  theme( axis.text  = element_text(size=14,colour = 'black'),
         axis.title = element_text(size = 15),
         title = element_text(size=16),
         legend.title = element_text(size = 11),
         legend.text = element_text(size=14)) +
  theme(plot.margin = unit(c(0.5,1,1,1), "cm"))

pdf(paste0("figs_lag/", region_subset, "/fig_S08_Rt_obs_vs_pred_state_capital.pdf"), height = 12, width = 12)
Rt_ts_prov
dev.off()
