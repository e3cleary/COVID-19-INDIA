rm(list=ls())
library(data.table) # fread - fastly reading data
library(lubridate)
library(ggplot2)
library(ggmap)
library(rgeos)
library("plyr")
library("ggplot2")
library("maptools")
library(raster)
library(igraph)
library(rgdal)
library(MASS)
library(fossil)
# library(McSpatial)
library(geosphere)
library(ggrepel)
library(hexbin)
library(gganimate)
library(viridis)
library(sf)
library(maps)
library(gapminder)
library(animation)
library(scales) # change  scientific notation on legend labels
library(tidyr)
library(reshape2)
library(readxl) # reading excel
library(RColorBrewer)
library(grid) #grid.draw
library(gridExtra) #grid.draw
library(gridBase)
# library(wbstats) # worldbank stats
library(countrycode)
library("ggspatial") # add scale and north_arrow into map

# library(ggsci) ## color scheme
# Sys.setlocale(category = "LC_ALL", locale = "chs") # read Chinese
# Sys.setlocale("LC_TIME", "English") # CN to EN for date

setwd('C:/Users/sl4m18/Documents/India/Mobility_network_analysis')
setwd('C:/Users/sl4m18/OneDrive - University of Southampton/Wuhan Coronavirus R0/Spread risk/manuscript/model_Manuscript_report/India/Modelling/Mobility_network_analysis')

### data preparation -------
# countries
iso3 <- c('IND')
country <- c('India')
i=1

## ~~ ISO week number by year -------
date <- seq(as.Date('2015-12-27'), as.Date('2022-12-31'), by=1)
ywd <- data.frame(date)
ywd$year <- as.integer(isoyear(ywd$date+1))
ywd$week <- as.integer(isoweek(ywd$date+1))
ywd$weekday <- weekdays(ywd$date)
ywd$weeknum <- wday(ywd$date)
write.csv(ywd, 'data/Google_year_week_number_by_day.csv', row.names = F)

# by week
date_begin <- seq(as.Date('2015-12-27'), as.Date('2022-12-31'), by=7)
yw <- data.frame(date_begin)
yw$date_end <- yw$date_begin + 6
yw$yearweek <- paste(yw$date_begin, '_', yw$date_end, sep = '')
yw$year <- as.integer(isoyear(yw$date_begin+1))
yw$week <- as.integer(isoweek(yw$date_begin+1))
write.csv(yw, 'data/Google_year_week_number_by_week.csv', row.names = F)

yw1 <- yw[,c('yearweek', 'year', 'week')]
yw2 <- yw[,c('year', 'week', 'date_begin')]

### function for reading weekly data files
read.weekdata <- function(date_begin, weeknum){
  glh.w.all <- data.frame()
  # read weekly file
  for(i in 1:weeknum){
    startdate <- as.Date('20200322', '%Y%m%d') + (i - 1)*7
    endate <- startdate + 6
    period <- paste('agg_epi_mobility_covid_', format(startdate, '%Y%m%d'), '_', format(endate, '%Y%m%d'), sep = '')
    
    ## change work dir to the folder where weekly GLH data saved  ##
    period <- paste('Weekly/Covid GLH data/', period, sep = '') 
    glh.w <- fread(period, header=F, stringsAsFactors=F, select = c('V1', 'V2', 'V5', 'V8'))
    glh.w$V8 <- exp(glh.w$V8)
    ### merge weekly data
    glh.w.all <- rbind(glh.w.all, glh.w)
  }  
  return(glh.w.all)
}

### line chart -----
## ~~ mobility, cases and Rt / R0 ----
# iso3 <- 'IND'
# country <- 'India'

yw <- fread('data/Google_year_week_number_by_week.csv')
g0 <- fread('data/GLH_adm0.csv', stringsAsFactors=F)
g0 <- g0[g0$date_begin >= date('2019-11-10') & g0$date_begin < date('2021-12-31'),]
g0 <- g0[order(g0$ISO3_s, g0$date_begin),]
# unique(g0$date_begin [g0$date_begin >= date('2019-11-10')])

# plot theme
ggplot_theme <- ggplot()+  
  geom_vline(xintercept = as.Date('2020-03-08'), colour = 'red' , linetype = 'dashed', alpha=0.8, size =0.7) +
  geom_vline(xintercept = as.Date('2020-01-01'), colour = 'grey' , linetype = 'dashed',alpha=0.8, size =0.8) +
  geom_vline(xintercept = as.Date('2021-01-01'), colour = 'grey' , linetype = 'dashed',alpha=0.8, size =0.8) +
  geom_vline(xintercept = as.Date('2022-01-01'), colour = 'grey' , linetype = 'dashed',alpha=0.8, size =0.8) +
  geom_hline(yintercept = 1, colour = 'grey' , alpha=0.8, size =0.7) +
  theme_bw() + theme(panel.grid.minor = element_blank(), 
                     panel.grid.major = element_blank())  +
  theme(legend.position=c(0.4,0.9), legend.background = element_blank())+
  theme( axis.text  = element_text(size=14,colour = 'black'), 
         axis.title = element_text(size = 15),
         title = element_text(size=16),
         legend.title = element_text(size = 11),
         legend.text = element_text(size=14)) +
  theme(plot.margin = unit(c(0.5,1,1,1), "cm"))
alpha=0.6

i=1
### mobility

# adm 0 - domestic flow
g0.d <- g0[g0$ISO3_s == iso3[i] & g0$ISO3_d == iso3[i],]
bc.level <- mean(g0.d$sum[g0.d$year==2020 & g0.d$week <=8])
g0.d$sum.std <- g0.d$sum/bc.level

# adm 0 - international outflow 
g0.out <- g0[g0$ISO3_s == iso3[i] & g0$ISO3_d != iso3[i],]
g0.out.all <- g0.out[,list(sum = sum(sum, na.rm = T)), by='year,week,date_begin']
bc.level <- mean(g0.out.all$sum[g0.out.all$year==2020 & g0.out.all$week <=8])
g0.out.all$sum.std <- g0.out.all$sum/bc.level

# adm 0 - international inflow
g0.in <- g0[g0$ISO3_s != iso3[i] & g0$ISO3_d == iso3[i],]
g0.in.all <- g0.in[,list(sum = sum(sum, na.rm = T)), by='year,week,date_begin']
bc.level <- mean(g0.in.all$sum[g0.in.all$year==2020 & g0.in.all$week <=8])
g0.in.all$sum.std <- g0.in.all$sum/bc.level

p0.in.all <- ggplot_theme +
  ylim(0, 1.5) +
  geom_line(data = g0.d, aes(x=date_begin, y=sum.std, colour='Domestic travel'),alpha=alpha, size=0.6) + 
  geom_line(data = g0.out.all, aes(x=date_begin, y=sum.std, colour='International outflow'), alpha=alpha, size=0.6) + 
  geom_line(data = g0.in.all, aes(x=date_begin, y=sum.std, colour='International inflow'), alpha=alpha, size=0.6) + 
  scale_color_manual(name='',values = c(
    'Domestic travel' = 'Orange',
    'International outflow' = 'red',
    'International inflow' = 'blue')) + 
  # labs(title = paste(country[i], '(refer to average level of the first 8 weeks in 2020)'), x='Date', y = 'Relative mobility', colour='') + 
  labs(title = 'C', x='', y = 'Relative mobility', colour='') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2021-12-31')), date_breaks = '2 months', date_labels = "%b %y")
# print(p0.in.all)


### ~~~~ cases and Rt ----
## Rt and R0 calculation - see Rt_v1.R
cases <- fread('data/estimated_Rt.csv', stringsAsFactors = F)
cases <- cases[cases$date <= as.Date('2021-12-31'),]

p0.case.all <- ggplot_theme +
  geom_area(data = cases, aes(x=date, y=new_cases_smoothed/1000), 
            fill='lightgreen', colour= 'darkgreen', alpha=alpha, size=0.6) + 
  annotate("text", x=as.Date('2020-03-14'), y=max(cases$new_cases_smoothed,na.rm = T)/1000, hjust = 0, label= 'Pandemic declared by WHO') +
  labs(title = 'A', x='', y = 'No. of cases (in thousand)') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2021-12-31')), date_breaks = '2 months', date_labels = "%b %y")
p0.case.all

rt <- cases[cases$date >= as.Date('2020-03-01'),]
p0.rt.all <- ggplot_theme +
  ylim(0, 5) +
  geom_ribbon(data = rt, aes(x=date, ymin = R0_CI95low, ymax = R0_CI95high,
                             fill="R0 95% CI"), alpha = 0.7, show.legend = F)+
  geom_line(data = rt, aes(x=date, y=R0, color="R0 mean and 95% CI"), size=0.5)+
  geom_ribbon(data = rt, aes(x=date, ymin = Quantile.0.025.R., ymax = Quantile.0.975.R.,
                             fill="Rt 95% CI"), alpha = 0.7, show.legend = F)+
  geom_line(data = rt, aes(x=date,y=Mean.R.,color="Rt mean and 95%  CI"))+
  scale_fill_manual(breaks = c('R0 95% CI', 'Rt 95% CI'),
                    values = c('orange', '#c6e7e6')) +   
  scale_colour_manual(name=NULL, breaks = c('R0 mean and 95% CI', 'Rt mean and 95% CI'),
                      values = c('orange', '#14a1a7')) + 
  labs(title = 'B', x='', y = 'Reproduction number') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2021-12-31')), date_breaks = '2 months', date_labels = "%b %y")+
  theme(legend.position=c(0.75,0.5))

p0.rt.all


## ~~ rural/urban adm level 2 ---- 
## urban and rural classification data
level1 <- read_excel('data/DEGURBA/gadm36_L3_2_1_CLASS_opr_IND.xlsx', sheet = 'INDIA_CLASS_L1')
level2 <- read_excel('data/DEGURBA/gadm36_L3_2_1_CLASS_opr_IND.xlsx', sheet = 'INDIA_CLASS_L2')

level1$DEGURBA_L1 <- factor(level1$DEGURBA_L1, levels = c('1', '2', '3'),
                            labels = c('Rural', 'Suburban', 'Urban'))
level2$DEGURBA_L1 <- factor(level2$DEGURBA_L1, levels = c('1', '2', '3'),
                            labels = c('Rural', 'Suburban', 'Urban'))

level1$DEGURBA_L2 <- factor(level1$DEGURBA_L2, levels = c('30', '23', '22', '21', '13', '12', '11'),
                            labels = c('Urban center', 'Dense urban cluster', 'Semi-dense urban cluster',
                                       'Suburban', 'Rural cluster', 'Low density rural', 'Very low density'))
level2$DEGURBA_L2 <- factor(level2$DEGURBA_L2, levels = c('30', '23', '22', '21', '13', '12', '11'),
                            labels = c('Urban center', 'Dense urban cluster', 'Semi-dense urban cluster',
                                       'Suburban', 'Rural cluster', 'Low density rural', 'Very low density'))
# level3$DEGURBA_L2 <- factor(level3$DEGURBA_L2, levels = c('30', '23', '22', '21', '13', '12', '11'),
#                             labels = c('Urban center', 'Dense urban cluster', 'Semi-dense urban cluster',
#                                        'Suburban', 'Rural cluster', 'Low density rural', 'Very low density'))

## mobility data 
g1 <- fread("data/India_adm1_GLH.csv", stringsAsFactors=F)
g2 <- fread("data/India_adm2_GLH.csv", stringsAsFactors=F)

# urban/rural
level_s <- level1[,c('GID_1', 'DEGURBA_L1', 'DEGURBA_L2')]
colnames(level_s) <- c('GID_1', 'DEGURBA_L1_s', 'DEGURBA_L2_s')
level_d <- level1[,c('GID_1', 'DEGURBA_L1', 'DEGURBA_L2')]
colnames(level_d) <- c('GID_1', 'DEGURBA_L1_d', 'DEGURBA_L2_d')

g1 <- merge(g1, level_d, by.x='GID_1_d', by.y='GID_1')
g1 <- merge(g1, level_s, by.x='GID_1_s', by.y='GID_1')

level_s <- level2[,c('GID_2', 'DEGURBA_L1', 'DEGURBA_L2')]
colnames(level_s) <- c('GID_2', 'DEGURBA_L1_s', 'DEGURBA_L2_s')
level_d <- level2[,c('GID_2', 'DEGURBA_L1', 'DEGURBA_L2')]
colnames(level_d) <- c('GID_2', 'DEGURBA_L1_d', 'DEGURBA_L2_d')

g2 <- merge(g2, level_d, by.x='GID_2_d', by.y='GID_2')
g2 <- merge(g2, level_s, by.x='GID_2_s', by.y='GID_2')

# level_s <- level3[,c('GID_3', 'DEGURBA_L1', 'DEGURBA_L2')]
# colnames(level_s) <- c('GID_3', 'DEGURBA_L1_s', 'DEGURBA_L2_s')
# level_d <- level3[,c('GID_3', 'DEGURBA_L1', 'DEGURBA_L2')]
# colnames(level_d) <- c('GID_3', 'DEGURBA_L1_d', 'DEGURBA_L2_d')
# 
# g3 <- merge(g3, level_d, by.x='GID_3_d', by.y='GID_3')
# g3 <- merge(g3, level_s, by.x='GID_3_s', by.y='GID_3')

g2 <- g2[g2$date_begin >= date('2019-11-10') & g2$date_begin <= as.Date('2021-12-31'),]

# domestic flow by DEGURBA_L1, refer to first 8 weeks in 2020
g2.1 <- g2[g2$GID_2_s == g2$GID_2_d, list(sum = sum(sum,na.rm = T)), by = 'year,week,date_begin,DEGURBA_L1_s'] # no space between variables
g2.1$sum.std <- g2.1$sum
bc.level <- mean(g2.1$sum[g2.1$DEGURBA_L1_s == 'Rural' & g2.1$year==2020 & g2.1$week <=8], na.rm=T)
g2.1$sum.std[g2.1$DEGURBA_L1_s == 'Rural'] <- g2.1$sum[g2.1$DEGURBA_L1_s == 'Rural']/bc.level
bc.level <- mean(g2.1$sum[g2.1$DEGURBA_L1_s == 'Suburban' & g2.1$year==2020 & g2.1$week <=8], na.rm=T)
g2.1$sum.std[g2.1$DEGURBA_L1_s == 'Suburban'] <- g2.1$sum[g2.1$DEGURBA_L1_s == 'Suburban']/bc.level
bc.level <- mean(g2.1$sum[g2.1$DEGURBA_L1_s == 'Urban' & g2.1$year==2020 & g2.1$week <=8], na.rm=T)
g2.1$sum.std[g2.1$DEGURBA_L1_s == 'Urban'] <- g2.1$sum[g2.1$DEGURBA_L1_s == 'Urban']/bc.level

rural.mean <- mean(g2.1$sum.std[g2.1$DEGURBA_L1_s == 'Rural' & g2.1$date_begin >=ymd(20200328)])
suburban.mean <- mean(g2.1$sum.std[g2.1$DEGURBA_L1_s == 'Suburban' & g2.1$date_begin >=ymd(20200328)])
urban.mean <- mean(g2.1$sum.std[g2.1$DEGURBA_L1_s == 'Urban' & g2.1$date_begin >=ymd(20200328)])


alpha=0.8 
p2.domestic <- ggplot_theme +
  ylim(0,1.5) +
  geom_line(data = g2.1, aes(x=date_begin, y=sum.std, group=DEGURBA_L1_s, colour=DEGURBA_L1_s), alpha=alpha, size=0.6) +
  labs(title = 'D Domestic flow', x='Date', y = 'Relative mobility', colour='') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2021-12-31')), date_breaks = '2 months', date_labels = "%b %y")

## add background colour for five periods
date_begin <- c('2019-11-10', '2020-03-22', '2021-01-31', '2021-04-18', '2021-11-07')
date_end <- c('2020-02-22', '2020-05-02', '2021-03-27', '2021-05-29', '2022-01-01')
col <- letters[1:5]
rects <- data.frame(date_begin = as.Date(date_begin), date_end = as.Date(date_end), col)
p2.domestic2 <- ggplot_theme +
  ylim(0,1.5) +
  geom_rect(data = rects, aes(xmin = date_begin, xmax = date_end, ymin = -Inf, ymax = Inf, fill = col), alpha = 0.4, show.legend = F) +
  geom_line(data = g2.1, aes(x=date_begin, y=sum.std, group=DEGURBA_L1_s, colour=DEGURBA_L1_s), alpha=alpha, size=0.6) +
  labs(title = 'A Domestic flow', x='Date', y = 'Relative mobility', colour='') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2022-01-01')), date_breaks = '2 months', date_labels = "%b %y")

pdf('plots/GLH_line_admin0_domestic_rural_urban_5period.pdf', width=12, height=4, onefile = T)
p2.domestic2
dev.off()

# outflow by DEGURBA_L1
g2.1s <- g2[g2$GID_2_s != g2$GID_2_d, list(sum = sum(sum,na.rm = T)), by = 'year,week,date_begin,DEGURBA_L1_s'] # no space between variables
g2.1s$sum.std <- g2.1s$sum
bc.level <- mean(g2.1s$sum[g2.1s$DEGURBA_L1_s == 'Rural' & g2.1s$year==2020 & g2.1s$week <=8], na.rm=T)
g2.1s$sum.std[g2.1s$DEGURBA_L1_s == 'Rural'] <- g2.1s$sum[g2.1s$DEGURBA_L1_s == 'Rural']/bc.level
bc.level <- mean(g2.1s$sum[g2.1s$DEGURBA_L1_s == 'Suburban' & g2.1s$year==2020 & g2.1s$week <=8], na.rm=T)
g2.1s$sum.std[g2.1s$DEGURBA_L1_s == 'Suburban'] <- g2.1s$sum[g2.1s$DEGURBA_L1_s == 'Suburban']/bc.level
bc.level <- mean(g2.1s$sum[g2.1s$DEGURBA_L1_s == 'Urban' & g2.1s$year==2020 & g2.1s$week <=8], na.rm=T)
g2.1s$sum.std[g2.1s$DEGURBA_L1_s == 'Urban'] <- g2.1s$sum[g2.1s$DEGURBA_L1_s == 'Urban']/bc.level

alpha=0.8 
p2.out <- ggplot_theme +
  ylim(0,2) +
  geom_line(data = g2.1s, aes(x=date_begin, y=sum.std, group=DEGURBA_L1_s, colour=DEGURBA_L1_s), alpha=alpha, size=0.6) +
  # annotate("text", x=as.Date('2020-03-14'), y=1.9, hjust = 0, label= 'Pandemic declared by WHO') +
  labs(title = 'A Outflow', x='', y = 'Relative mobility', colour='') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2021-12-31')), date_breaks = '2 months', date_labels = "%b %y")
# p2.out

# inflow by DEGURBA_L1
g2.1d <- g2[g2$GID_2_s != g2$GID_2_d, list(sum = sum(sum,na.rm = T)), by = 'year,week,date_begin,DEGURBA_L1_d'] # no space between variables
g2.1d$sum.std <- g2.1d$sum
bc.level <- mean(g2.1d$sum[g2.1d$DEGURBA_L1_d == 'Rural' & g2.1d$year==2020 & g2.1d$week <=8])
g2.1d$sum.std[g2.1d$DEGURBA_L1_d == 'Rural'] <- g2.1d$sum[g2.1d$DEGURBA_L1_d == 'Rural']/bc.level
bc.level <- mean(g2.1d$sum[g2.1d$DEGURBA_L1_d == 'Suburban' & g2.1d$year==2020 & g2.1d$week <=8])
g2.1d$sum.std[g2.1d$DEGURBA_L1_d == 'Suburban'] <- g2.1d$sum[g2.1d$DEGURBA_L1_d == 'Suburban']/bc.level
bc.level <- mean(g2.1d$sum[g2.1d$DEGURBA_L1_d == 'Urban' & g2.1d$year==2020 & g2.1d$week <=8])
g2.1d$sum.std[g2.1d$DEGURBA_L1_d == 'Urban'] <- g2.1d$sum[g2.1d$DEGURBA_L1_d == 'Urban']/bc.level

p2.in <- ggplot_theme +
  ylim(0,2) +
  geom_line(data = g2.1d, aes(x=date_begin, y=sum.std, group=DEGURBA_L1_d, colour=DEGURBA_L1_d), alpha=alpha, size=0.6) +
  labs(title = 'B Inflow', x='Date', y = 'Relative mobility', colour='') + 
  scale_x_date(limits = c(as.Date('2019-11-01'), as.Date('2021-12-31')), date_breaks = '2 months', date_labels = "%b %y")
# p2.in

# ~~ merge plots ----

pdf('plots/GLH_line_admin0_Rt_rural_urban.pdf', width=12, height=15, onefile = T)
map.dm <- rbind(ggplotGrob(p0.case.all), ggplotGrob(p0.rt.all), ggplotGrob(p0.in.all), ggplotGrob(p2.domestic), size = 'last')
grid.draw(map.dm)
dev.off()

pdf('plots/GLH_line_admin0_rural_urban.pdf', width=12, height=12, onefile = T)
map.dm <- rbind(ggplotGrob(p0.case.all), ggplotGrob(p0.in.all), ggplotGrob(p2.domestic), size = 'last')
grid.draw(map.dm)
dev.off()


## outflow and inflow
pdf('plots/GLH_line_admin0_outflow and inflow_rural_urban.pdf', width=12, height=9, onefile = T)
map.dm <- rbind(ggplotGrob(p2.out), ggplotGrob(p2.in), size = 'last')
grid.draw(map.dm)
dev.off()

### Mapping domestic flows at admin 2 -------
## shapefile ---
s0 = read_sf(dsn="data/shapefile",layer="gadm36_IND_0")
s1 = read_sf(dsn="data/shapefile",layer="gadm36_IND_1")
s2 = read_sf(dsn="data/shapefile",layer="gadm36_IND_2")

### read admin 1/2 data
ind20base.1 <- fread("data/adm1_agg_epi_mobility_covid_base.csv", stringsAsFactors=F)
ind2021.1 <- fread("data/adm1_agg_epi_mobility_covid_2020_21.csv", stringsAsFactors=F)

ind20base.2 <- fread("data/adm2_agg_epi_mobility_covid_base.csv", stringsAsFactors=F)
ind2021.2 <- fread("data/adm2_agg_epi_mobility_covid_2020_21.csv", stringsAsFactors=F)

### merge 2019-2021 data for covid and year + week
# merge data with ISO year and week
g2 <- rbind(ind20base.2, ind2021.2)
remove(ind20base.2)
remove(ind2021.2)
g2 <- merge(g2, yw1, by='yearweek')
g2 <- g2[,c('year', 'week', "ISO3_s", 'GID_1_s', 'GID_2_s',"ISO3_d", 'GID_1_d', 'GID_2_d', "sum","mean","sd","median","q1","q3")]
g2 <- merge(yw2, g2, by=c('year', 'week'))
g2 <- g2[g2$date_begin >= date('2019-11-10') & g2$date_begin < date('2021-12-31'),]
g2 <- data.table(g2)


i=1 # India - 1

## plot theme
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


### ~~ India rural and urban classification -------
# crs(s2)
# st_crs(ind0)
# ind1 <- st_set_crs(ind1, crs(ind23))

ru1 = merge(s1, level1, by=c('GID_1'), all.x=T)
ru2 = merge(s2, level2, by=c('GID_2'), all.x=T)

### mapping rural and urban areas ---
map.ru1 <- case.map + 
  # geom_sf(shp0, mapping=aes(), fill='lightgrey', colour='darkgrey', size=0.01) +
  geom_sf(ru1, mapping = aes(fill = DEGURBA_L1), colour='grey', size=0.01) +
  ggtitle(label='A Admin level 1')

map.ru2 <- case.map + 
  # geom_sf(shp0, mapping=aes(), fill='lightgrey', colour='darkgrey', size=0.01) +
  geom_sf(ru2, mapping = aes(fill = DEGURBA_L1), colour='grey', size=0.01) +
  ggtitle(label='B Admin level 2')


map.ru1.2 <- case.map + 
  # geom_sf(shp0, mapping=aes(), fill='lightgrey', colour='darkgrey', size=0.01) +
  geom_sf(ru1, mapping = aes(fill = DEGURBA_L2), colour='grey', size=0.01) +
  ggtitle(label='C Admin level 1')
map.ru2.2 <- case.map + 
  # geom_sf(shp0, mapping=aes(), fill='lightgrey', colour='darkgrey', size=0.01) +
  geom_sf(ru2, mapping = aes(fill = DEGURBA_L2), colour='grey', size=0.01) +
  ggtitle(label='D Admin level 2')

# merge maps---
pdf('plots/urban_rural.pdf', width=12, height=14, onefile = T)
map.dm1 <- cbind( ggplotGrob(map.ru1), ggplotGrob(map.ru2), size = 'last')
map.dm2 <- cbind( ggplotGrob(map.ru1.2), ggplotGrob(map.ru2.2),  size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()

## ~~ before pandemic ----
# 15 weeks - from 10 Nov 2019 to the first 8 weeks in 2020 (by 22 Feb 2020)
g2b.dm <- g2[g2$date_begin <= as.Date('2020-02-22') & g2$ISO3_s == iso3[i] & g2$ISO3_d == iso3[i] 
             & g2$GID_2_s != g2$GID_2_d,] 
week.b <- length(unique(g2b.dm$date_begin))
g2b.dm.in <- g2b.dm[,list(sum_in_b = sum(sum, na.rm = T)/week.b), by='ISO3_d,GID_2_d']
g2b.dm.out <- g2b.dm[,list(sum_out_b = sum(sum, na.rm = T)/week.b), by='ISO3_s,GID_2_s']
g2b.dm <- merge(g2b.dm.in, g2b.dm.out, by.x=c('ISO3_d','GID_2_d'), by.y=c('ISO3_s','GID_2_s'), all=T)
g2b.dm$sum_net_b <- (g2b.dm$sum_in_b - g2b.dm$sum_out_b)*100000

## ~~ 1st lockdown ----
# (6 weeks) - 2020-03-22 to 2020-05-02
g2s.dm1 <- g2[g2$date_begin >= as.Date('2020-03-22') & g2$date_begin < as.Date('2020-05-03') & g2$ISO3_s == iso3[i] & g2$ISO3_d == iso3[i]
             & g2$GID_2_s != g2$GID_2_d,] 
week.s <- length(unique(g2s.dm1$date_begin))
g2s.dm1.in <- g2s.dm1[,list(sum_in_s = sum(sum, na.rm = T)/week.s), by='ISO3_d,GID_2_d']
g2s.dm1.out <- g2s.dm1[,list(sum_out_s = sum(sum, na.rm = T)/week.s), by='ISO3_s,GID_2_s']
g2s.dm1 <- merge(g2s.dm1.in, g2s.dm1.out, by.x=c('ISO3_d','GID_2_d'), by.y=c('ISO3_s','GID_2_s'), all=T)
g2s.dm1$sum_net_s <- (g2s.dm1$sum_in_s - g2s.dm1$sum_out_s)*100000

g2.dm1 <- merge(g2b.dm, g2s.dm1, by=c('ISO3_d','GID_2_d'), all=T)

high <- max(g2.dm1$sum_in_b, na.rm = T)
g2.dm1$sum_in_b <- g2.dm1$sum_in_b/high
g2.dm1$sum_in_s <- g2.dm1$sum_in_s/high
g2.dm1$sum_in_diff <- (g2.dm1$sum_in_b - g2.dm1$sum_in_s)/g2.dm1$sum_in_b*100

high <- max(g2.dm1$sum_out_b, na.rm=T)
g2.dm1$sum_out_b <- g2.dm1$sum_out_b/high
g2.dm1$sum_out_s <- g2.dm1$sum_out_s/high
g2.dm1$sum_out_diff <- (g2.dm1$sum_out_b - g2.dm1$sum_out_s)/g2.dm1$sum_out_b*100
g2.dm1$sum_out_diff[g2.dm1$sum_out_diff <0] <- 0
g2.dm1$sum_out_diff[g2.dm1$sum_out_diff >100] <- 100

# mapping Domestic outflows for each admin unit
g.dm1.s2 = merge(s2, g2.dm1, by.x=c('GID_0', 'GID_2'), by.y = c('ISO3_d', 'GID_2_d'), all.x=T)
# g.dm1.s2$sum_out_b[is.na(g.dm1.s2$sum_out_b)==T] <- min(g.dm1.s2$sum_out_b, na.rm=T)
g.dm1.s2$sum_out_s[is.na(g.dm1.s2$sum_out_s)==T & is.na(g.dm1.s2$sum_out_b)==F] <- min(g.dm1.s2$sum_out_s, na.rm=T)
g.dm1.s2$sum_out_s[is.na(g.dm1.s2$sum_out_b)==T] <- NA

g.dm1.s2$sum_out_diff[is.na(g.dm1.s2$sum_out_diff)==T & is.na(g.dm1.s2$sum_out_b)==F] <- max(g.dm1.s2$sum_out_diff, na.rm = T)
g.dm1.s2$sum_out_diff[is.na(g.dm1.s2$sum_out_b)==T] <- NA

g2_dm1_out_b <- case.map + geom_sf(g.dm1.s2, mapping = aes(fill = sum_out_b), 
                                   colour=NA, size=0.01, show.legend = F) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'RdYlBu', direction=-1, 
                       limits = c(min(g.dm1.s2$sum_out_s, na.rm=T), 1), 
                       na.value = 'darkgreen', trans="log10",
                       name= paste("Relative mobility", sep = '')) +
  ggtitle(label='D Before pandemic')

g2_dm1_out_s <- case.map + geom_sf(g.dm1.s2, mapping = aes(fill = sum_out_s),
                                   colour=NA, size=0.01, show.legend = F) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'RdYlBu', direction=-1, 
                       limits = c(min(g.dm1.s2$sum_out_s, na.rm=T), 1), 
                       na.value = 'darkgreen', trans="log10",
                       name= paste("Relative mobility", sep = '')) +
  ggtitle(label='E 1st lockdown')

g2_dm1_out_diff <- case.map + geom_sf(g.dm1.s2, mapping = aes(fill = sum_out_diff), colour=NA, size=0.01) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'Blues', direction=1, 
                       limits = c(0, 100),na.value = 'darkgreen',
                       name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle(label='A 1st lockdown')

## ~~ before 2nd lockdown ----
# 8 weeks - 2021-01-31 to 2021-03-27
g2s.dm2 <- g2[g2$date_begin >= as.Date('2021-01-31') & g2$date_begin < as.Date('2021-03-28') & g2$ISO3_s == iso3[i] & g2$ISO3_d == iso3[i]
              & g2$GID_2_s != g2$GID_2_d,] 
week.s <- length(unique(g2s.dm2$date_begin))
g2s.dm2.in <- g2s.dm2[,list(sum_in_s = sum(sum, na.rm = T)/week.s), by='ISO3_d,GID_2_d']
g2s.dm2.out <- g2s.dm2[,list(sum_out_s = sum(sum, na.rm = T)/week.s), by='ISO3_s,GID_2_s']
g2s.dm2 <- merge(g2s.dm2.in, g2s.dm2.out, by.x=c('ISO3_d','GID_2_d'), by.y=c('ISO3_s','GID_2_s'), all=T)
g2s.dm2$sum_net_s <- (g2s.dm2$sum_in_s - g2s.dm2$sum_out_s)*100000

g2.dm2 <- merge(g2b.dm, g2s.dm2, by=c('ISO3_d','GID_2_d'), all=T)

high <- max(g2.dm2$sum_in_b, na.rm = T)
g2.dm2$sum_in_b <- g2.dm2$sum_in_b/high
g2.dm2$sum_in_s <- g2.dm2$sum_in_s/high
g2.dm2$sum_in_diff <- (g2.dm2$sum_in_b - g2.dm2$sum_in_s)/g2.dm2$sum_in_b*100

high <- max(g2.dm2$sum_out_b, na.rm=T)
g2.dm2$sum_out_b <- g2.dm2$sum_out_b/high
g2.dm2$sum_out_s <- g2.dm2$sum_out_s/high
g2.dm2$sum_out_diff <- (g2.dm2$sum_out_b - g2.dm2$sum_out_s)/g2.dm2$sum_out_b*100
g2.dm2$sum_out_diff[g2.dm2$sum_out_diff <0] <- 0
g2.dm2$sum_out_diff[g2.dm2$sum_out_diff >100] <- 100

# mapping Domestic outflows for each admin unit
g.dm2.s2 = merge(s2, g2.dm2, by.x=c('GID_0', 'GID_2'), by.y = c('ISO3_d', 'GID_2_d'), all.x=T)
g.dm2.s2$sum_out_s[is.na(g.dm2.s2$sum_out_s)==T & is.na(g.dm2.s2$sum_out_b)==F] <- min(g.dm1.s2$sum_out_s, na.rm=T)
g.dm2.s2$sum_out_diff[is.na(g.dm2.s2$sum_out_diff)==T & is.na(g.dm2.s2$sum_out_b)==F] <- max(g.dm2.s2$sum_out_diff, na.rm = T)

g2_dm2_out_s <- case.map + geom_sf(g.dm2.s2, mapping = aes(fill = sum_out_s), colour=NA, size=0.01) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'RdYlBu', direction=-1, 
                       limits = c(min(g.dm1.s2$sum_out_s, na.rm=T), 1), 
                       na.value = 'darkgreen', trans="log10",
                       name= paste("Relative mobility", sep = '')) +
  ggtitle(label='Before 2nd lockdown')

g2_dm2_out_diff <- case.map + geom_sf(g.dm2.s2, mapping = aes(fill = sum_out_diff), 
                                      colour=NA, size=0.01, show.legend = F) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'Blues', direction=1, 
                       limits = c(0, 100),na.value = 'darkgreen',
                       name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle(label='B Before 2nd lockdown')

## ~~ 2nd lockdown ----
# 6 weeks - 2021-04-18 to 2021-05-29 (mobility < 0.6)
g2s.dm3 <- g2[g2$date_begin >= as.Date('2021-04-18') & g2$date_begin < as.Date('2021-05-30') & g2$ISO3_s == iso3[i] & g2$ISO3_d == iso3[i]
              & g2$GID_2_s != g2$GID_2_d,] 
week.s <- length(unique(g2s.dm3$date_begin))
g2s.dm3.in <- g2s.dm3[,list(sum_in_s = sum(sum, na.rm = T)/week.s), by='ISO3_d,GID_2_d']
g2s.dm3.out <- g2s.dm3[,list(sum_out_s = sum(sum, na.rm = T)/week.s), by='ISO3_s,GID_2_s']
g2s.dm3 <- merge(g2s.dm3.in, g2s.dm3.out, by.x=c('ISO3_d','GID_2_d'), by.y=c('ISO3_s','GID_2_s'), all=T)
g2s.dm3$sum_net_s <- (g2s.dm3$sum_in_s - g2s.dm3$sum_out_s)*100000

g2.dm3 <- merge(g2b.dm, g2s.dm3, by=c('ISO3_d','GID_2_d'), all=T)

high <- max(g2.dm3$sum_in_b, na.rm = T)
g2.dm3$sum_in_b <- g2.dm3$sum_in_b/high
g2.dm3$sum_in_s <- g2.dm3$sum_in_s/high
g2.dm3$sum_in_diff <- (g2.dm3$sum_in_b - g2.dm3$sum_in_s)/g2.dm3$sum_in_b*100

high <- max(g2.dm3$sum_out_b, na.rm=T)
g2.dm3$sum_out_b <- g2.dm3$sum_out_b/high
g2.dm3$sum_out_s <- g2.dm3$sum_out_s/high
g2.dm3$sum_out_diff <- (g2.dm3$sum_out_b - g2.dm3$sum_out_s)/g2.dm3$sum_out_b*100
g2.dm3$sum_out_diff[g2.dm3$sum_out_diff <0] <- 0
g2.dm3$sum_out_diff[g2.dm3$sum_out_diff >100] <- 100

# mapping Domestic outflows for each admin unit
g.dm3.s2 = merge(s2, g2.dm3, by.x=c('GID_0', 'GID_2'), by.y = c('ISO3_d', 'GID_2_d'), all.x=T)
g.dm3.s2$sum_out_s[is.na(g.dm3.s2$sum_out_s)==T & is.na(g.dm3.s2$sum_out_b)==F] <- min(g.dm1.s2$sum_out_s, na.rm=T)
g.dm3.s2$sum_out_s[is.na(g.dm3.s2$sum_out_b)==T] <- NA

g.dm3.s2$sum_out_diff[is.na(g.dm3.s2$sum_out_diff)==T & is.na(g.dm3.s2$sum_out_b)==F] <- max(g.dm3.s2$sum_out_diff, na.rm = T)
g.dm3.s2$sum_out_diff[is.na(g.dm3.s2$sum_out_b)==T] <- NA

g2_dm3_out_s <- case.map + geom_sf(g.dm3.s2, mapping = aes(fill = sum_out_s), 
                                   colour=NA, size=0.01, show.legend = T) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'RdYlBu', direction=-1, 
                       limits = c(min(g.dm1.s2$sum_out_s, na.rm=T), 1), 
                       na.value = 'darkgreen', trans="log10",
                       name= paste("Relative mobility", sep = '')) +
  ggtitle(label='F 2nd lockdown')

g2_dm3_out_diff <- case.map + geom_sf(g.dm3.s2, mapping = aes(fill = sum_out_diff), 
                                      colour=NA, size=0.01, show.legend = F) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'Blues', direction=1, 
                       limits = c(0, 100),na.value = 'darkgreen',
                       name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle(label='C 2nd lockdown')

## ~~ after 2nd lockdown Nov/Dec 2021 ----
# 8 weeks - 2021-11-07 to 2022-01-01 (mobility > 0.9)
g2s.dm4 <- g2[g2$date_begin >= as.Date('2021-11-07') & g2$date_begin < as.Date('2022-01-01') & g2$ISO3_s == iso3[i] & g2$ISO3_d == iso3[i]
              & g2$GID_2_s != g2$GID_2_d,] 
week.s <- length(unique(g2s.dm4$date_begin))
g2s.dm4.in <- g2s.dm4[,list(sum_in_s = sum(sum, na.rm = T)/week.s), by='ISO3_d,GID_2_d']
g2s.dm4.out <- g2s.dm4[,list(sum_out_s = sum(sum, na.rm = T)/week.s), by='ISO3_s,GID_2_s']
g2s.dm4 <- merge(g2s.dm4.in, g2s.dm4.out, by.x=c('ISO3_d','GID_2_d'), by.y=c('ISO3_s','GID_2_s'), all=T)
g2s.dm4$sum_net_s <- (g2s.dm4$sum_in_s - g2s.dm4$sum_out_s)*100000

g2.dm4 <- merge(g2b.dm, g2s.dm4, by=c('ISO3_d','GID_2_d'), all=T)

high <- max(g2.dm4$sum_in_b, na.rm = T)
g2.dm4$sum_in_b <- g2.dm4$sum_in_b/high
g2.dm4$sum_in_s <- g2.dm4$sum_in_s/high
g2.dm4$sum_in_diff <- (g2.dm4$sum_in_b - g2.dm4$sum_in_s)/g2.dm4$sum_in_b*100

high <- max(g2.dm4$sum_out_b, na.rm=T)
g2.dm4$sum_out_b <- g2.dm4$sum_out_b/high
g2.dm4$sum_out_s <- g2.dm4$sum_out_s/high
g2.dm4$sum_out_diff <- (g2.dm4$sum_out_b - g2.dm4$sum_out_s)/g2.dm4$sum_out_b*100
g2.dm4$sum_out_diff[g2.dm4$sum_out_diff <0] <- 0
g2.dm4$sum_out_diff[g2.dm4$sum_out_diff >100] <- 100

# mapping Domestic outflows for each admin unit
g.dm4.s2 = merge(s2, g2.dm4, by.x=c('GID_0', 'GID_2'), by.y = c('ISO3_d', 'GID_2_d'), all.x=T)
g.dm4.s2$sum_out_s[is.na(g.dm4.s2$sum_out_s)==T & is.na(g.dm4.s2$sum_out_b)==F] <- min(g.dm1.s2$sum_out_s, na.rm=T)
g.dm4.s2$sum_out_diff[is.na(g.dm4.s2$sum_out_diff)==T & is.na(g.dm4.s2$sum_out_b)==F] <- max(g.dm4.s2$sum_out_diff, na.rm = T)

g2_dm4_out_s <- case.map + geom_sf(g.dm4.s2, mapping = aes(fill = sum_out_s), colour=NA, size=0.01) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'RdYlBu', direction=-1, 
                       limits = c(min(g.dm1.s2$sum_out_s, na.rm=T), 1), 
                       na.value = 'darkgreen', trans="log10",
                       name= paste("Relative mobility", sep = '')) +
  ggtitle(label='Nov-Dec 2021')

g2_dm4_out_diff <- case.map + geom_sf(g.dm4.s2, mapping = aes(fill = sum_out_diff), 
                                      colour=NA, size=0.01, show.legend = F) +
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  scale_fill_distiller(palette = 'Blues', direction=1, 
                       limits = c(0, 100),na.value = 'darkgreen',
                       name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle(label='D Nov-Dec 2021')


## ~~ merge maps  -----
pdf(paste('plots/GLH_map_', iso3[i],'_district.pdf', sep=''), width=12, height=14, onefile = T)
## domestic outflow baseline / changes
# map.dm1 <- cbind( ggplotGrob(g2_dm1_out_b), ggplotGrob(g2_dm1_out_s), size = 'last')
map.dm1 <- cbind(ggplotGrob(g2_dm1_out_diff), ggplotGrob(g2_dm2_out_diff), size = 'last')
map.dm2 <- cbind(ggplotGrob(g2_dm3_out_diff), ggplotGrob(g2_dm4_out_diff),  size = 'last')
map.dm3 <- rbind(map.dm1, map.dm2, size = 'last')
grid.draw(map.dm3)

dev.off()


### segment and connectivity ------
# line size
size=0.5
alpha=0.6

# s2cell
s2cell <- fread('data/s2cell_2018_2022.csv', stringsAsFactors = F)
s2cell$lat <- round(s2cell$lat,1)
s2cell$lon <- round(s2cell$lon,1)
s2cell <- unique(s2cell)

### ~~ before pandemic ----
# 15 weeks: 10 Nov 2019 - 22 Feb 2020
week.b = 15
glh.b <- fread('Weekly/agg_epi_mobility_base_10Nov2019_22Feb2020.csv', stringsAsFactors = F)
glh.b <- merge(glh.b, s2cell, by.x = 'cell_s', by.y='cell', all.x=T)
colnames(glh.b) <- c("cell_s", "cell_d", "link", "sum","lat_s","lon_s")
glh.b <- merge(glh.b, s2cell, by.x = 'cell_d', by.y='cell', all.x=T)
colnames(glh.b) <- c("cell_d", "cell_s", "link", "sum","lat_s","lon_s", "lat_d","lon_d")

# Selecting destination countries
s2cell.iso <- fread('data/s2cell_iso3_GADM0.csv', stringsAsFactors = F)
s2cell.iso <- s2cell.iso[s2cell.iso$GID_0 !=''| is.na(s2cell.iso$GID_0) == F,]
glh.b <- merge(glh.b, s2cell.iso, by.x = 'cell_s', by.y='cell', all.x=T)
glh.b$GID_0_s <- glh.b$GID_0
glh.b$GID_0 <- NULL
glh.b <- merge(glh.b, s2cell.iso, by.x = 'cell_d', by.y='cell', all.x=T)
glh.b$GID_0_d <- glh.b$GID_0
glh.b$GID_0 <- NULL

# domestic flow
glh.b <- glh.b[glh.b$GID_0_d %in% iso3 & glh.b$GID_0_s %in% iso3,]
glh.b.iso <- glh.b[, list(link = sum(link, na.rm = T), 
                              sum = sum(sum, na.rm = T)),
                       by = 'lat_s,lon_s,lat_d,lon_d'] # no space between variables
glh.b.iso$link <- glh.b.iso$link/week.b 
glh.b.iso$sum <- glh.b.iso$sum/week.b 

### ~~ 1st lockdown ----
# 6 weeks: 2020-03-22 to 2020-05-02
week.s1 <- 6
glh.s1 <- read.weekdata('20200322', week.s1)
colnames(glh.s1) <- c('yearweek', 'cell_s', 'cell_d', 'move')
unique(glh.s1$yearweek)
glh.s1 <- glh.s1[, list(link = length(move),
                        sum = sum(move,na.rm = T)), 
                 by = 'cell_s,cell_d'] # no space between variables

glh.s1 <- merge(glh.s1, s2cell, by.x = 'cell_s', by.y='cell', all.x=T)
colnames(glh.s1) <- c("cell_s", "cell_d", "link", "sum","lat_s","lon_s")
glh.s1 <- merge(glh.s1, s2cell, by.x = 'cell_d', by.y='cell', all.x=T)
colnames(glh.s1) <- c("cell_d", "cell_s", "link", "sum","lat_s","lon_s", "lat_d","lon_d")

# Selecting destination countries
glh.s1 <- merge(glh.s1, s2cell.iso, by.x = 'cell_s', by.y='cell', all.x=T)
glh.s1$GID_0_s <- glh.s1$GID_0
glh.s1$GID_0 <- NULL
glh.s1 <- merge(glh.s1, s2cell.iso, by.x = 'cell_d', by.y='cell', all.x=T)
glh.s1$GID_0_d <- glh.s1$GID_0
glh.s1$GID_0 <- NULL

# domestic flow
glh.s1 <- glh.s1[glh.s1$GID_0_d %in% iso3 & glh.s1$GID_0_s %in% iso3,]
glh.s1.iso <- glh.s1[, list(link = sum(link, na.rm = T), 
                            sum = sum(sum, na.rm = T)),
                     by = 'lat_s,lon_s,lat_d,lon_d'] # no space between variables
glh.s1.iso$link <- glh.s1.iso$link/week.s1 
glh.s1.iso$sum <- glh.s1.iso$sum/week.s1 

## limits
move.max <- max(max(glh.b.iso$sum), max(glh.s1.iso$sum))
move.min <- min(min(glh.b.iso$sum), min(glh.s1.iso$sum))
limit.low <- sqrt(move.min/move.max)
glh.b.iso$sum <- glh.b.iso$sum/move.max
glh.b.iso <- glh.b.iso[order(glh.b.iso$sum, decreasing= F),]


## ~~~~ mapping data before pandemic ----
map.b = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.b.iso[glh.b.iso$lat_s != glh.b.iso$lat_d & glh.b.iso$lon_s != glh.b.iso$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  # geom_polygon(data=allshp0.points, aes(x=long, y=lat, group=group), colour="azure4", fill=NA) +
  scale_colour_distiller(name='Relative mobility', 
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('A Before pandemic')

# domestic top 30%
glh.b.iso.20 <- glh.b.iso[order(glh.b.iso$sum, decreasing= T),]
glh.b.iso.20 <- glh.b.iso.20[1:round(nrow(glh.b.iso.20)*0.3),]
glh.b.iso.20 <- glh.b.iso.20[order(glh.b.iso.20$sum, decreasing= F),]

map.b.20 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.b.iso.20[glh.b.iso.20$lat_s != glh.b.iso.20$lat_d & glh.b.iso.20$lon_s != glh.b.iso.20$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = T) +
  scale_colour_distiller(name='Relative mobility', 
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('A Top 30% flow before pandemic') 


# connections of inflow
glh.b.connect <- glh.b[glh.b$lat_s != glh.b$lat_d & glh.b$lon_s != glh.b$lon_d, 
                       list(link = sum(link, na.rm = T), 
                            sum = sum(sum, na.rm = T)),
                       by = 'lat_d,lon_d'] # no space between variables
glh.b.connect$link <- glh.b.connect$link/week.b

connect.b = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_hex(data=glh.b.connect, aes(x=lon_d, y=lat_d, weight=link), binwidth=c(1, 1),
           inherit.aes = FALSE, alpha=alpha, show.legend = T) +
  scale_fill_distiller(name='Connections', 
                       limits=c(0.1, 100000),
                       palette="Spectral", direction=-1, trans="log10") +
  ggtitle('A Before pandemic') 

## ~~~~ mapping data in 1st lockdown----
glh.s1.iso$sum <- glh.s1.iso$sum/move.max
glh.s1.iso <- glh.s1.iso[order(glh.s1.iso$sum, decreasing= F),]

map.s1 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s1.iso[glh.s1.iso$lat_s != glh.s1.iso$lat_d & glh.s1.iso$lon_s != glh.s1.iso$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  scale_colour_distiller(name='Relative mobility',
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('B 1st lockdown') 


## domestic top 30%
glh.s1.iso.20 <- glh.s1.iso[order(glh.s1.iso$sum, decreasing= T),]
glh.s1.iso.20 <- glh.s1.iso.20[1:round(nrow(glh.s1.iso.20)*0.3),]
glh.s1.iso.20 <- glh.s1.iso.20[order(glh.s1.iso.20$sum, decreasing= F),]

map.s1.20 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s1.iso.20[glh.s1.iso.20$lat_s != glh.s1.iso.20$lat_d & glh.s1.iso.20$lon_s != glh.s1.iso.20$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  scale_colour_distiller(name='Relative mobility', 
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('B Top 30% flow in 1st lockdown') 

# connections of inflow
glh.s1.connect <- glh.s1[glh.s1$lat_s != glh.s1$lat_d & glh.s1$lon_s != glh.s1$lon_d, 
                       list(link = sum(link, na.rm = T), 
                            sum = sum(sum, na.rm = T)),
                       by = 'lat_d,lon_d'] # no space between variables
glh.s1.connect$link <- glh.s1.connect$link/week.s1

connect.s1 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_hex(data=glh.s1.connect, aes(x=lon_d, y=lat_d, weight=link), binwidth=c(1, 1), 
           inherit.aes = FALSE, alpha=alpha, show.legend = F) +
  scale_fill_distiller(name='Connections', 
                       limits=c(0.1, 100000),
                       palette="Spectral", direction=-1, trans="log10") +
  ggtitle('B 1st lockdown') 


## changes of mobility and connections compared to pre-pandemic level
# all flows
glh.s1.change <- merge(glh.b.iso, glh.s1.iso, by = c('lat_s', 'lon_s', 'lat_d', 'lon_d'), all.x=T)
glh.s1.change$sum.y[is.na(glh.s1.change$sum.y) ==T] <- 0
glh.s1.change$link.y[is.na(glh.s1.change$link.y) ==T] <- 0
glh.s1.change$sum_reduction <- (glh.s1.change$sum.x - glh.s1.change$sum.y)/glh.s1.change$sum.x *100
glh.s1.change$link_reduction <- (glh.s1.change$link.x - glh.s1.change$link.y)/glh.s1.change$link.x *100
glh.s1.change <- glh.s1.change[order(glh.s1.change$sum_reduction, decreasing= T),]

map.s1.change = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s1.change[glh.s1.change$lat_s != glh.s1.change$lat_d & glh.s1.change$lon_s != glh.s1.change$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sum_reduction), 
               inherit.aes = FALSE, size=size, alpha=alpha/3) +
  scale_colour_distiller(palette = 'Blues', direction=1, 
                         limits = c(0, 100), 
                         # na.value = 'white',
                         name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle('1st lockdown')


### ~~ before 2nd lockdown ----
# 8 weeks: 2021-01-31 to 2020-03-27
week.s2 <- 8
glh.s2 <- read.weekdata('20210131', week.s2)
colnames(glh.s2) <- c('yearweek', 'cell_s', 'cell_d', 'move')
unique(glh.s2$yearweek)
glh.s2 <- glh.s2[, list(link = length(move),
                        sum = sum(move,na.rm = T)), 
                 by = 'cell_s,cell_d'] # no space between variables

glh.s2 <- merge(glh.s2, s2cell, by.x = 'cell_s', by.y='cell', all.x=T)
colnames(glh.s2) <- c("cell_s", "cell_d", "link", "sum","lat_s","lon_s")
glh.s2 <- merge(glh.s2, s2cell, by.x = 'cell_d', by.y='cell', all.x=T)
colnames(glh.s2) <- c("cell_d", "cell_s", "link", "sum","lat_s","lon_s", "lat_d","lon_d")

# Selecting destination countries
glh.s2 <- merge(glh.s2, s2cell.iso, by.x = 'cell_s', by.y='cell', all.x=T)
glh.s2$GID_0_s <- glh.s2$GID_0
glh.s2$GID_0 <- NULL
glh.s2 <- merge(glh.s2, s2cell.iso, by.x = 'cell_d', by.y='cell', all.x=T)
glh.s2$GID_0_d <- glh.s2$GID_0
glh.s2$GID_0 <- NULL

# domestic flow
glh.s2 <- glh.s2[glh.s2$GID_0_d %in% iso3 & glh.s2$GID_0_s %in% iso3,]
glh.s2.iso <- glh.s2[, list(link = sum(link, na.rm = T), 
                            sum = sum(sum, na.rm = T)),
                     by = 'lat_s,lon_s,lat_d,lon_d'] # no space between variables
glh.s2.iso$link <- glh.s2.iso$link/week.s2 
glh.s2.iso$sum <- glh.s2.iso$sum/week.s2 

## mapping
glh.s2.iso$sum <- glh.s2.iso$sum/move.max
glh.s2.iso <- glh.s2.iso[order(glh.s2.iso$sum, decreasing= F),]

map.s2 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s2.iso[glh.s2.iso$lat_s != glh.s2.iso$lat_d & glh.s2.iso$lon_s != glh.s2.iso$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  scale_colour_distiller(name='Relative mobility',
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('Before 2nd lockdown') 


## domestic top 30%
glh.s2.iso.20 <- glh.s2.iso[order(glh.s2.iso$sum, decreasing= T),]
glh.s2.iso.20 <- glh.s2.iso.20[1:round(nrow(glh.s2.iso.20)*0.3),]
glh.s2.iso.20 <- glh.s2.iso.20[order(glh.s2.iso.20$sum, decreasing= F),]

map.s2.20 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s2.iso.20[glh.s2.iso.20$lat_s != glh.s2.iso.20$lat_d & glh.s2.iso.20$lon_s != glh.s2.iso.20$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = T) +
  scale_colour_distiller(name='Relative mobility', 
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('Top 30% flow before 2nd lockdown') 

# connections of inflow
glh.s2.connect <- glh.s2[glh.s2$lat_s != glh.s2$lat_d & glh.s2$lon_s != glh.s2$lon_d, 
                         list(link = sum(link, na.rm = T), 
                              sum = sum(sum, na.rm = T)),
                         by = 'lat_d,lon_d'] # no space between variables
glh.s2.connect$link <- glh.s2.connect$link/week.s2

connect.s2 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_hex(data=glh.s2.connect, aes(x=lon_d, y=lat_d, weight=link), binwidth=c(1, 1), 
           inherit.aes = FALSE, alpha=alpha, show.legend = F) +
  scale_fill_distiller(name='Connections', 
                       limits=c(0.1, 100000),
                       palette="Spectral", direction=-1, trans="log10") +
  ggtitle('Before 2nd lockdown') 


## changes of mobility and connections compared to pre-pandemic level
# all flows
glh.s2.change <- merge(glh.b.iso, glh.s2.iso, by = c('lat_s', 'lon_s', 'lat_d', 'lon_d'), all.x=T)
glh.s2.change$sum.y[is.na(glh.s2.change$sum.y) ==T] <- 0
glh.s2.change$link.y[is.na(glh.s2.change$link.y) ==T] <- 0
glh.s2.change$sum_reduction <- (glh.s2.change$sum.x - glh.s2.change$sum.y)/glh.s2.change$sum.x *100
glh.s2.change$link_reduction <- (glh.s2.change$link.x - glh.s2.change$link.y)/glh.s2.change$link.x *100
glh.s2.change <- glh.s2.change[order(glh.s2.change$sum_reduction, decreasing= T),]

map.s2.change = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s2.change[glh.s2.change$lat_s != glh.s2.change$lat_d & glh.s2.change$lon_s != glh.s2.change$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sum_reduction), 
               inherit.aes = FALSE, size=size, alpha=alpha/3) +
  scale_colour_distiller(palette = 'Blues', direction=1, 
                         limits = c(0, 100), 
                         # na.value = 'white',
                         name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle('Before 2nd lockdown')


### ~~ 2nd lockdown ----
# 6 weeks: 2021-04-18 to 2021-05-29
week.s3 <- 6
glh.s3 <- read.weekdata('20210418', week.s3)
colnames(glh.s3) <- c('yearweek', 'cell_s', 'cell_d', 'move')
unique(glh.s3$yearweek)
glh.s3 <- glh.s3[, list(link = length(move),
                        sum = sum(move,na.rm = T)), 
                 by = 'cell_s,cell_d'] # no space between variables

glh.s3 <- merge(glh.s3, s2cell, by.x = 'cell_s', by.y='cell', all.x=T)
colnames(glh.s3) <- c("cell_s", "cell_d", "link", "sum","lat_s","lon_s")
glh.s3 <- merge(glh.s3, s2cell, by.x = 'cell_d', by.y='cell', all.x=T)
colnames(glh.s3) <- c("cell_d", "cell_s", "link", "sum","lat_s","lon_s", "lat_d","lon_d")

# Selecting destination countries
glh.s3 <- merge(glh.s3, s2cell.iso, by.x = 'cell_s', by.y='cell', all.x=T)
glh.s3$GID_0_s <- glh.s3$GID_0
glh.s3$GID_0 <- NULL
glh.s3 <- merge(glh.s3, s2cell.iso, by.x = 'cell_d', by.y='cell', all.x=T)
glh.s3$GID_0_d <- glh.s3$GID_0
glh.s3$GID_0 <- NULL

# domestic flow
glh.s3 <- glh.s3[glh.s3$GID_0_d %in% iso3 & glh.s3$GID_0_s %in% iso3,]
glh.s3.iso <- glh.s3[, list(link = sum(link, na.rm = T), 
                            sum = sum(sum, na.rm = T)),
                     by = 'lat_s,lon_s,lat_d,lon_d'] # no space between variables
glh.s3.iso$link <- glh.s3.iso$link/week.s3 
glh.s3.iso$sum <- glh.s3.iso$sum/week.s3 

## mapping
glh.s3.iso$sum <- glh.s3.iso$sum/move.max
glh.s3.iso <- glh.s3.iso[order(glh.s3.iso$sum, decreasing= F),]

map.s3 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s3.iso[glh.s3.iso$lat_s != glh.s3.iso$lat_d & glh.s3.iso$lon_s != glh.s3.iso$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = T) +
  scale_colour_distiller(name='Relative mobility',
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('C 2nd lockdown') 


## domestic top 30%
glh.s3.iso.20 <- glh.s3.iso[order(glh.s3.iso$sum, decreasing= T),]
glh.s3.iso.20 <- glh.s3.iso.20[1:round(nrow(glh.s3.iso.20)*0.3),]
glh.s3.iso.20 <- glh.s3.iso.20[order(glh.s3.iso.20$sum, decreasing= F),]

map.s3.20 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s3.iso.20[glh.s3.iso.20$lat_s != glh.s3.iso.20$lat_d & glh.s3.iso.20$lon_s != glh.s3.iso.20$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  scale_colour_distiller(name='Relative mobility', 
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('C Top 30% flow in 2nd lockdown') 

# connections of inflow
glh.s3.connect <- glh.s3[glh.s3$lat_s != glh.s3$lat_d & glh.s3$lon_s != glh.s3$lon_d, 
                         list(link = sum(link, na.rm = T), 
                              sum = sum(sum, na.rm = T)),
                         by = 'lat_d,lon_d'] # no space between variables
glh.s3.connect$link <- glh.s3.connect$link/week.s3

connect.s3 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_hex(data=glh.s3.connect, aes(x=lon_d, y=lat_d, weight=link), binwidth=c(1, 1), 
           inherit.aes = FALSE, alpha=alpha, show.legend = F) +
  scale_fill_distiller(name='Connections', 
                       limits=c(0.1, 100000),
                       palette="Spectral", direction=-1, trans="log10") +
  ggtitle('C 2nd lockdown') 


## changes of mobility and connections compared to pre-pandemic level
# all flows
glh.s3.change <- merge(glh.b.iso, glh.s3.iso, by = c('lat_s', 'lon_s', 'lat_d', 'lon_d'), all.x=T)
glh.s3.change$sum.y[is.na(glh.s3.change$sum.y) ==T] <- 0
glh.s3.change$link.y[is.na(glh.s3.change$link.y) ==T] <- 0
glh.s3.change$sum_reduction <- (glh.s3.change$sum.x - glh.s3.change$sum.y)/glh.s3.change$sum.x *100
glh.s3.change$link_reduction <- (glh.s3.change$link.x - glh.s3.change$link.y)/glh.s3.change$link.x *100
glh.s3.change <- glh.s3.change[order(glh.s3.change$sum_reduction, decreasing= T),]

map.s3.change = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s3.change[glh.s3.change$lat_s != glh.s3.change$lat_d & glh.s3.change$lon_s != glh.s3.change$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sum_reduction), 
               inherit.aes = FALSE, size=size, alpha=alpha/3) +
  scale_colour_distiller(palette = 'Blues', direction=1, 
                         limits = c(0, 100), 
                         # na.value = 'white',
                         name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle('2nd lockdown')

### ~~ after 2nd lockdown Nov/Dec 2021 ----
# 8 weeks: 2021-11-07 to 2022-01-01
week.s4 <- 8
glh.s4 <- read.weekdata('20211107', week.s4)
colnames(glh.s4) <- c('yearweek', 'cell_s', 'cell_d', 'move')
unique(glh.s4$yearweek)
glh.s4 <- glh.s4[, list(link = length(move),
                        sum = sum(move,na.rm = T)), 
                 by = 'cell_s,cell_d'] # no space between variables

glh.s4 <- merge(glh.s4, s2cell, by.x = 'cell_s', by.y='cell', all.x=T)
colnames(glh.s4) <- c("cell_s", "cell_d", "link", "sum","lat_s","lon_s")
glh.s4 <- merge(glh.s4, s2cell, by.x = 'cell_d', by.y='cell', all.x=T)
colnames(glh.s4) <- c("cell_d", "cell_s", "link", "sum","lat_s","lon_s", "lat_d","lon_d")

# Selecting destination countries
glh.s4 <- merge(glh.s4, s2cell.iso, by.x = 'cell_s', by.y='cell', all.x=T)
glh.s4$GID_0_s <- glh.s4$GID_0
glh.s4$GID_0 <- NULL
glh.s4 <- merge(glh.s4, s2cell.iso, by.x = 'cell_d', by.y='cell', all.x=T)
glh.s4$GID_0_d <- glh.s4$GID_0
glh.s4$GID_0 <- NULL

# domestic flow
glh.s4 <- glh.s4[glh.s4$GID_0_d %in% iso3 & glh.s4$GID_0_s %in% iso3,]
glh.s4.iso <- glh.s4[, list(link = sum(link, na.rm = T), 
                            sum = sum(sum, na.rm = T)),
                     by = 'lat_s,lon_s,lat_d,lon_d'] # no space between variables
glh.s4.iso$link <- glh.s4.iso$link/week.s4 
glh.s4.iso$sum <- glh.s4.iso$sum/week.s4 

## mapping
glh.s4.iso$sum <- glh.s4.iso$sum/move.max
glh.s4.iso <- glh.s4.iso[order(glh.s4.iso$sum, decreasing= F),]

map.s4 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s4.iso[glh.s4.iso$lat_s != glh.s4.iso$lat_d & glh.s4.iso$lon_s != glh.s4.iso$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  scale_colour_distiller(name='Relative mobility',
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('Nov-Dec 2021') 


## domestic top 30%
glh.s4.iso.20 <- glh.s4.iso[order(glh.s4.iso$sum, decreasing= T),]
glh.s4.iso.20 <- glh.s4.iso.20[1:round(nrow(glh.s4.iso.20)*0.3),]
glh.s4.iso.20 <- glh.s4.iso.20[order(glh.s4.iso.20$sum, decreasing= F),]

map.s4.20 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s4.iso.20[glh.s4.iso.20$lat_s != glh.s4.iso.20$lat_d & glh.s4.iso.20$lon_s != glh.s4.iso.20$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sqrt(sum)), 
               inherit.aes = FALSE, size=size, alpha=alpha, show.legend = F) +
  scale_colour_distiller(name='Relative mobility', 
                         limits = c(limit.low, 1), 
                         # palette="Spectral",direction=-1, 
                         palette="OrRd",direction=1, 
                         trans="log10") + # trans="log10"
  ggtitle('D Top 30% flow in Nov-Dec 2021') 

# connections of inflow
glh.s4.connect <- glh.s4[glh.s4$lat_s != glh.s4$lat_d & glh.s4$lon_s != glh.s4$lon_d, 
                         list(link = sum(link, na.rm = T), 
                              sum = sum(sum, na.rm = T)),
                         by = 'lat_d,lon_d'] # no space between variables
glh.s4.connect$link <- glh.s4.connect$link/week.s4

connect.s4 = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_hex(data=glh.s4.connect, aes(x=lon_d, y=lat_d, weight=link), binwidth=c(1, 1), 
           inherit.aes = FALSE, alpha=alpha, show.legend = F) +
  scale_fill_distiller(name='Connections', 
                       limits=c(0.1, 100000),
                       palette="Spectral", direction=-1, trans="log10") +
  ggtitle('D Nov-Dec 2021') 


## changes of mobility and connections compared to pre-pandemic level
# all flows
glh.s4.change <- merge(glh.b.iso, glh.s4.iso, by = c('lat_s', 'lon_s', 'lat_d', 'lon_d'), all.x=T)
glh.s4.change$sum.y[is.na(glh.s4.change$sum.y) ==T] <- 0
glh.s4.change$link.y[is.na(glh.s4.change$link.y) ==T] <- 0
glh.s4.change$sum_reduction <- (glh.s4.change$sum.x - glh.s4.change$sum.y)/glh.s4.change$sum.x *100
glh.s4.change$link_reduction <- (glh.s4.change$link.x - glh.s4.change$link.y)/glh.s4.change$link.x *100
glh.s4.change <- glh.s4.change[order(glh.s4.change$sum_reduction, decreasing= T),]

map.s4.change = case.map  +   
  geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
  geom_segment(data=glh.s4.change[glh.s4.change$lat_s != glh.s4.change$lat_d & glh.s4.change$lon_s != glh.s4.change$lon_d,], 
               aes(x=lon_s, y=lat_s, xend = lon_d, yend = lat_d, colour=sum_reduction), 
               inherit.aes = FALSE, size=size, alpha=alpha/3) +
  scale_colour_distiller(palette = 'Blues', direction=1, 
                         limits = c(0, 100), 
                         # na.value = 'white',
                         name= paste("Mobility reduction (%)", sep = '')) +
  ggtitle('Nov-Dec 2021')

### ~~~~ merge maps  ----
pdf('plots/GLH_map_flows.pdf', width=18, height=14, onefile = T)
map.dm1 <- cbind( ggplotGrob(map.b), ggplotGrob(map.s1), ggplotGrob(map.s3), size = 'last')
map.dm2 <- cbind( ggplotGrob(g2_dm1_out_b), ggplotGrob(g2_dm1_out_s),  ggplotGrob(g2_dm3_out_s), size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()

# 20% or 30% TOP flows
pdf('plots/GLH_map_flows_30.pdf', width=12, height=14, onefile = T)
map.dm1 <- cbind( ggplotGrob(map.b.20), ggplotGrob(map.s1.20),  size = 'last')
map.dm2 <- cbind( ggplotGrob(map.s3.20), ggplotGrob(map.s4.20), size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()

# flow mobility reductions
pdf('plots/GLH_map_flow_changes.pdf', width=12, height=14, onefile = T)
map.dm1 <- cbind( ggplotGrob(map.s1.change), ggplotGrob(map.s2.change),  size = 'last')
map.dm2 <- cbind( ggplotGrob(map.s3.change), ggplotGrob(map.s4.change), size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()

# hexgon connections adjusted by weeks
pdf('plots/GLH_map_inflow_connections.pdf', width=12, height=14, onefile = T)
map.dm1 <- cbind( ggplotGrob(connect.b), ggplotGrob(connect.s1),  size = 'last')
map.dm2 <- cbind( ggplotGrob(connect.s3), ggplotGrob(connect.s4), size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()


