### R script to explore exposure-lag-response relationships

# load packages and pre-processed data
# source("3_load_packages_data.R")
data <- data[order(data$week_index, data$GID_2),]
head(data)

## load best fitting model with DLNMs 
best.model
load(paste0("output/whole country/", best.model, ".RData"))

head(data)
min(data$week)
max(data$week)
max(data$date_begin)

mean(data$mean_intra)
mean(data$t2m)
mean(data$Stringency)
min(data$date_begin)
max(data$date_begin)
## Specifies a unique column name for the model
# colnames(basis_policy) = paste0("basis_policy.", colnames(basis_policy))

##### Mobility ----
# Coefficient and covariance
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with basis_GLH crossbasis
indt <- grep("basis_GLH", model$names.fixed)

# set value
cen_c1 <- round(mean(data$mean_intra, na.rm = FALSE),0)
min_c1 <- min(data$mean_intra, na.rm = FALSE)
max_c1 <- max(data$mean_intra, na.rm = FALSE)

# extract predictions from the GLH DLNM centred on overall mean mobility
predt <- crosspred(basis_GLH, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = cen_c1) 
predt_low_cp <- crosspred(basis_GLH, coef = coef[indt], vcov=vcov[indt, indt],
                          model.link = "log", bylag = 0.25, at=seq(min_c1, cen_c1, by=0.01), cen=cen_c1) 
predt_high_cp <- crosspred(basis_GLH, coef = coef[indt], vcov=vcov[indt,indt],
                           model.link = "log", bylag = 0.25, at=seq(cen_c1, max_c1,by=0.01), cen=cen_c1) 

# x-axis label for lag time
x_laglabel <- 0:nlag
# x_laglabel <- c(0, '', 2, '', 4, '', 6, '', 8, '', 10, '', 12, '', 14)

## explore exposure lag response associations 
# Plot
pdf("figs/whole country/fig_03a_GLH_association.pdf", width=6, height=6)
par(mar = c(5, 4, 2, 4))
plot(predt_low_cp,"overall",col=2,xlim=c(0, 1.5),ylim=c(-1.,2),
     axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(predt_high_cp,"overall", ci="area", col=4, lwd=2.5)
axis(1,at=0:6*0.25,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Relative mobility",cex.lab=1.2)
mtext("Relative risk",side = 2, at=1, line =2.5,cex=1.2)
mtext(side = 2, at = 2*1.1, text = "a", las = 2, cex = 1.2, line = 2)
par(new=T)
hist(data$mean_intra, xlim=c(0, 1.5),ylim=c(0,5000),axes=F,ann=F,col="wheat",breaks=20)
axis(4, at=0:3*1000,cex.axis=1.2)
mtext("Frequency",side =4,at=1000, line =2.5,cex=1.2,las=3)
dev.off()

## contour plot of exposure-lag-response associations
pdf("figs/whole country/fig_03b_GLH_contour.pdf", width = 6, height = 6)

y <- predt$predvar
x <- seq(0, nlag, 0.25)
z <- t(predt$matRRfit)

pal <- rev(brewer.pal(11, "PRGn"))
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, weeks", ylab = 'Relative mobility', main = "",
               col = cols,levels = levels,
               plot.axes = { axis(1,at = 0:nlag, labels = x_laglabel) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.1, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

## lag response for different mobility scenarios
pdf("figs/whole country/fig_03c_GLH_scenario_3.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar
vars

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
#r1
r2 <- max(range(rr, rr.lci, rr.uci))
#r2

# get selected exposure variable positions
#mn <- which(round(vars, 2) == 0.6)
mn <- which(round(vars, 2) == 0.8)
mn
mx <- which(round(vars, 2) == 1.4)
mx
mx2 <- which(round(vars, 2) == 1.6)
mx2

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.25)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
     xlab = "Lag, weeks", ylab = "Relative risk", main = "", 
     # ylim = range(r1, r2*1.11), 
     ylim = range(0.5, 1.5), 
     frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = x_laglabel)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)
# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)
abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Mobility = ",vars[mn]),
                  paste0("Mobility = ", vars[mx]),
                  paste0("Mobility = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = 1.5*1.1, text = "c", las = 2, cex = 1.2, line = 2)

dev.off()


##### Temperature ----
# Coefficient and covariance
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with basis_TEMP crossbasis
indt <- grep("basis_TEMP", model$names.fixed)

# set value
cen_c1 <- round(mean(data$t2m), 0)
min_c1 <- min(data$t2m, na.rm = FALSE)
max_c1 <- max(data$t2m, na.rm = FALSE)

# extract predictions from the TEMP DLNM centred on overall mean Temperature 3
predt <- crosspred(basis_TEMP, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = cen_c1) 
predt_low_cp <- crosspred(basis_TEMP, coef = coef[indt], vcov=vcov[indt, indt],
                       model.link = "log", bylag = 0.25, at=seq(min_c1, cen_c1, by=0.1), cen=cen_c1) 
predt_high_cp <- crosspred(basis_TEMP, coef = coef[indt], vcov=vcov[indt,indt],
                        model.link = "log", bylag = 0.25, at=seq(cen_c1, max_c1,by=0.1), cen=cen_c1) 

# x-axis label for lag time
x_laglabel <- 0:nlag
# x_laglabel <- c(0, '', 2, '', 4, '', 6, '', 8, '', 10, '', 12, '', 14)
  
## explore exposure lag response associations 
# Plot
pdf("figs/whole country/fig_03a_temp_association.pdf", width=6, height=6)
par(mar = c(5, 4, 2, 4))
plot(predt_low_cp,"overall",col=2,xlim=c(-20, 40),ylim=c(-1.,2),
     axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(predt_high_cp,"overall", ci="area", col=4, lwd=2.5)
axis(1,at=-2:4*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Temperature",cex.lab=1.2)
mtext("Relative risk",side = 2, at=1, line =2.5,cex=1.2)
mtext(side = 2, at = 2*1.1, text = "a", las = 2, cex = 1.2, line = 2)
par(new=T)
hist(data$t2m, xlim=c(-20,40),ylim=c(0,5000),axes=F,ann=F,col="wheat",breaks=20)
axis(4, at=0:3*1000,cex.axis=1.2)
mtext("Frequency",side =4,at=1000, line =2.5,cex=1.2,las=3)
dev.off()

## contour plot of exposure-lag-response associations
pdf("figs/whole country/fig_03b_temp_contour.pdf", width = 6, height = 6)

y <- predt$predvar
x <- seq(0, nlag, 0.25)
z <- t(predt$matRRfit)

pal <- rev(brewer.pal(11, "PRGn"))
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, weeks", ylab = expression(paste("Temperature (",degree,"C)")), main = "",
               col = cols,levels = levels,
               plot.axes = { axis(1,at = 0:nlag, labels = x_laglabel) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.1, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

## lag response for different Temperature scenarios
pdf("figs/whole country/fig_03c_temp_scenario_2.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar
vars

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
#r1
r2 <- max(range(rr, rr.lci, rr.uci))
#r2
#r2 <- 1.4

# get selected exposure variable positions
mn <- which(round(vars, 2) == 10)
mx <- which(round(vars, 2) == 20)
mx2 <- which(round(vars, 2) == 30)

# mn <- which(round(vars, 2) == -10)
# mx <- which(round(vars, 2) == 10)
# mx2 <- which(round(vars, 2) == 20)

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.25)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
     xlab = "Lag, weeks", ylab = "Relative risk", main = "", 
     ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = x_laglabel)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)
# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)
abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Temp = ",vars[mn]," deg C"),
                            paste0("Temp = ", vars[mx]," deg C"),
                            paste0("Temp = ", vars[mx2]," deg C")),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "c", las = 2, cex = 1.2, line = 2)

dev.off()

##### Precipitation ----
# Coefficient and covariance
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with basis_PREC crossbasis
indt <- grep("basis_PREC", model$names.fixed)

# set value
cen_c1 <- mean(data$tp, na.rm=FALSE)
in_c1 <- min(data$tp, na.rm = FALSE)
max_c1 <- max(data$tp, na.rm = FALSE)

# extract predictions from the PREC DLNM centred on overall mean 
predt <- crosspred(basis_PREC, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = cen_c1) 
predt_low_cp <- crosspred(basis_PREC, coef = coef[indt], vcov=vcov[indt, indt],
                          model.link = "log", bylag = 0.25, at=seq(min_c1, cen_c1, by=0.1), cen=cen_c1) 
predt_high_cp <- crosspred(basis_PREC, coef = coef[indt], vcov=vcov[indt,indt],
                           model.link = "log", bylag = 0.25, at=seq(cen_c1, max_c1,by=0.1), cen=cen_c1) 

## explore exposure lag response associations 
# Plot
pdf("figs/whole country/fig_03a_prec_association.pdf", width=6, height=6)
par(mar = c(5, 4, 2, 4))
plot(predt_low_cp,"overall",col=2,xlim=c(0, 1),ylim=c(-1.,2),
     axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(predt_high_cp,"overall", ci="area", col=4, lwd=2.5)
axis(1,at=0:5*0.5,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Precipitation (m)",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
mtext(side = 2, at = 2*1.1, text = "a", las = 2, cex = 1.2, line = 2)
par(new=T)
hist(data$tp,xlim=c(0, 1),ylim=c(0, 15000),axes=F,ann=F,col="wheat",breaks=45)
axis(4,at=0:3*2500,cex.axis=1.2)
mtext("Frequency",side =4,at=4000, line =2.5,cex=1.2,las=3)
dev.off()

## contour plot of exposure-lag-response associations
pdf("figs/whole country/fig_03b_prec_contour.pdf", width = 6, height = 6)

y <- predt$predvar
x <- seq(0, nlag, 0.25)
z <- t(predt$matRRfit)

pal <- rev(brewer.pal(11, "PRGn"))
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, weeks", ylab = "Precipitation (m)", main = "",
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, x_laglabel) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.1, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

## lag response for different Precipitation scenarios
pdf("figs/whole country/fig_03c_prec_scenario_3.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar
vars

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
#r1
#r1 <- 0.85
r2 <- max(range(rr, rr.lci, rr.uci))
#r2
#r2 <- 1.05

# get selected exposure variable positions
mn <- which(round(vars, 2) == 0.05)
#mn
mx <- which(round(vars, 2) == 0.5)
#mx
mx2 <- which(round(vars, 2) == 1.0)
#mx2

mn <- which(round(vars, 2) == 1.00)
mn
mx <- which(round(vars, 2) == 2.0)
mx
mx2 <- which(round(vars, 2) == 3.0)
mx2

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.25)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
     xlab = "Lag, weeks", ylab = "Relative risk", main = "", 
     ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = x_laglabel)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)
# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)
abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Prec = ",vars[mn]," m"),
                  paste0("Prec = ", vars[mx]," m"),
                  paste0("Prec = ", vars[mx2]," m")),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.15, text = "c", las = 2, cex = 1.2, line = 2)

dev.off()


##### Covid intervention policy ----
# basis_policy -- data$Stringency

# Coefficient and covariance
coef <- model$summary.fixed$mean
vcov <- model$misc$lincomb.derived.covariance.matrix

# find position of the terms associated with basis_policy crossbasis
indt <- grep("basis_policy", model$names.fixed)

# set value
cen_c1 <- mean(data$Stringency, na.rm = FALSE)
min_c1 <- min(data$Stringency, na.rm = FALSE)
max_c1 <- max(data$Stringency, na.rm = FALSE)

# extract predictions from the ContainmentHealthIndex DLNM centred on overall mean 
predt <- crosspred(basis_policy, coef = coef[indt], vcov=vcov[indt,indt],
                   model.link = "log", bylag = 0.25, cen = cen_c1) 
predt_low_cp <- crosspred(basis_policy, coef = coef[indt], vcov=vcov[indt, indt],
                          model.link = "log", bylag = 0.25, at=seq(min_c1, cen_c1, by=0.1), cen=cen_c1) 
predt_high_cp <- crosspred(basis_policy, coef = coef[indt], vcov=vcov[indt,indt],
                           model.link = "log", bylag = 0.25, at=seq(cen_c1, max_c1,by=0.1), cen=cen_c1) 

## explore exposure lag response associations 
# Plot
pdf("figs/whole country/fig_03a_policy_association.pdf", width=6, height=6)
par(mar = c(5, 4, 2, 4))
plot(predt_low_cp,"overall",col=2,xlim=c(0, 100),ylim=c(-1.,2),
     axes=F,ann=F,lwd=2.5,cex.axis=5)
lines(predt_high_cp,"overall", ci="area", col=4, lwd=2.5)
axis(1,at=0:10*10,cex.axis=1.2)
axis(2,at=c(0:2*1),cex.axis=1.2)
title(xlab= "Intervention stringency",cex.lab=1.2)
mtext("Relative risk",side = 2,at=1, line =2.5,cex=1.2)
mtext(side = 2, at = 2*1.1, text = "a", las = 2, cex = 1.2, line = 2)
par(new=T)
hist(data$Stringency,xlim=c(0,100),ylim=c(0,3500),axes=F,ann=F,col="wheat",breaks=20)
axis(4,at=0:4*500,cex.axis=1.2)
mtext("Frequency",side =4,at=1000, line =2.5,cex=1.2,las=3)
dev.off()

## contour plot of exposure-lag-response associations
pdf("figs/whole country/fig_03b_policy_contour.pdf", width = 6, height = 6)

y <- predt$predvar
x <- seq(0, nlag, 0.25)
z <- t(predt$matRRfit)

pal <- rev(brewer.pal(11, "PRGn"))
levels <- pretty(z, 20)
col1 <- colorRampPalette(pal[1:6])
col2 <- colorRampPalette(pal[6:11])
cols <- c(col1(sum(levels < 1)), col2(sum(levels > 1)))

filled.contour(x,y,z,
               xlab = "Lag, weeks", ylab = 'Intervention stringency', main = "",
               col = cols,levels = levels,
               plot.axes = { axis(1, at = 0:nlag, x_laglabel) 
                 axis(2)})
mtext(side = 2, at = max(y)*1.1, text = "b", las = 2, cex = 1.2, line = 2)

dev.off()

## lag response for different intervention stringency 
pdf("figs/whole country/fig_03c_policy_scenario_9.pdf", width = 6, height = 6)

# get exposures values
vars <- predt$predvar
vars

# obtain relative risk (RR) fit and upper and lower confidence limits for all exposure variables
rr <- predt$matRRfit
rr.lci <- predt$matRRlow
rr.uci <- predt$matRRhigh

# set relative risk range 
r1 <- min(range(rr, rr.lci, rr.uci))
#r1
r2 <- max(range(rr, rr.lci, rr.uci))
#r2

# get selected exposure variable positions
# mn <- which(round(vars, 2) == 70)
# #mn
# mx <- which(round(vars, 2) == 75)
# #mx
# mx2 <- which(round(vars, 2) == 80)
# #mx2

mn <- which(round(vars, 2) == 65)
#mn
mx <- which(round(vars, 2) == 75)
#mx
mx2 <- which(round(vars, 2) == 85)
#mx2

# mn <- which(round(vars, 2) == 65)
# #mn
# mx <- which(round(vars, 2) == 70)
# #mx
# mx2 <- which(round(vars, 2) == 80)
# #mx2

# get selected exposure variable positions
# mn <- which(round(vars, 2) == 70)
# #mn
# mx <- which(round(vars, 2) == 80)
# #mx
# mx2 <- which(round(vars, 2) == 87.5)
# #mx2

# define colours
col1 <- brewer.pal(11, "RdBu")[9]
tcol1 <- do.call(rgb, c(as.list(col2rgb(col1)), alpha = 255/4, max = 255))

col2 <- brewer.pal(11, "RdBu")[3]
tcol2 <- do.call(rgb, c(as.list(col2rgb(col2)), alpha = 255/4, max = 255))

col3 <- brewer.pal(11, "RdBu")[1]
tcol3 <- do.call(rgb, c(as.list(col2rgb(col3)), alpha = 255/4, max = 255))

# define x values (lag, by lag)
lagbylag <- seq(0, nlag, 0.25)

# cool
plot(lagbylag, rr[mn,], col = col1, type = "l", lwd = 1, 
     xlab = "Lag, weeks", ylab = "Relative risk", main = "", 
     ylim = range(r1, r2*1.11), frame.plot = T, axes = F)
axis(1, at = 0:nlag, labels = x_laglabel)
axis(2)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mn,], rev(rr.uci[mn,]))
polygon(xx, yy, col = tcol1, border = tcol1)
# warm
lines(lagbylag, rr[mx,], col = col2, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx,], rev(rr.uci[mx,]))
polygon(xx, yy, col = tcol2, border = tcol2)
abline(h = 1, lty = 3)
# warmest
lines(lagbylag, rr[mx2,], col = col3, lwd = 1)
xx <- c(lagbylag, rev(lagbylag))
yy <- c(rr.lci[mx2,],rev(rr.uci[mx2,]))
polygon(xx, yy, col = tcol3, border = tcol3)
abline(h = 1, lty = 3)

legend("topleft",
       legend = c(paste0("Policy index = ",vars[mn]),
                  paste0("Policy index = ", vars[mx]),
                  paste0("Policy index = ", vars[mx2])),
       col = c(col1, col2, col3), 
       lwd = 2, lty = 1, bty = "n", 
       y.intersp = 1.5, horiz = F)

mtext(side = 2, at = r2*1.25, text = "c", las = 2, cex = 1.2, line = 2)

dev.off()


