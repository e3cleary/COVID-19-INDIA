## Visualise and explore mobility, covid, climate and socio-economic datasets
library(grid) #grid.draw
library(gridExtra) #grid.draw
library(gridBase)

# load packages and data

source("3_load_packages_data.R")

# load pre-defined grid of states for geofacet plots
grid <- fread("data/India_state_grid.csv")
state <- fread("data/States.csv")

# load shape file 
map_all <- read_sf("data/shapefile/gadm36_IND_2.shp")
map_all <- merge(map_all, state[,c('GID_1', 'region')], by='GID_1', all.x=T)

data.map <- merge(map, data, by='GID_2', all.x=T)
data.map <- data.map[order(data.map$date_begin, data.map$GID_2),]

### for mapping
case.map <- ggplot() +
  # xlim(-165, 165)+
  # ylim(-52, 78) +
  # xlab('') + ylab('') +  theme_bw() + 
  theme(legend.position = c(0.85,0.2), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme( axis.text  = element_blank(), axis.ticks = element_blank(), 
         axis.title = element_text(size = 15),
         title = element_text(size=18),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 

## Region and district and population -----
# Region  
plot1 <- ggplot(map_all)  +
  geom_sf(mapping = aes(fill = region), lwd = 0, color = NA, alpha =0.6, show.legend = T) +
  # geom_sf(mapping = aes(), lwd = 0.01, fill = NA, color = 'lightgrey', show.legend = F) +
  theme_void() + ggtitle('A Regions') +
  guides(fill=guide_legend(title="Region")) +
  theme(legend.position = c(0.8,0.3), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme( axis.text  = element_blank(), axis.ticks = element_blank(), 
         axis.title = element_text(size = 15),
         title = element_text(size=18),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 

## State 
plot2 <- ggplot(map_all) +
  geom_sf(mapping = aes(fill = GID_1), lwd = 0, color = NA, alpha =0.6, show.legend = F) +
  # geom_sf(mapping = aes(), lwd = 0.01, fill = NA, color = 'lightgrey', show.legend = F) +
  theme_void() + ggtitle('B States')+
  theme(legend.position = c(0.8,0.3), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme( axis.text  = element_blank(), axis.ticks = element_blank(), 
         axis.title = element_text(size = 15),
         title = element_text(size=18),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 

## pop sum
plot3 <- ggplot(data.map[data.map$weeknum ==1,]) +
  geom_sf(mapping = aes(fill= pop_sum/1000), lwd = 0, color = NA, show.legend = T) +
  theme_void() + ggtitle('C Total population') +
  scale_fill_distiller(name = paste0('Population\n(in thousands)'), 
                       direction = 1,
                       palette = "YlGnBu",
                       trans = "log10")+ 
  theme(legend.position = c(0.8,0.25), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme( axis.text  = element_blank(), axis.ticks = element_blank(), 
         axis.title = element_text(size = 15),
         title = element_text(size=18),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 

## pop density
plot4 <- ggplot(data.map[data.map$weeknum ==1,]) +
  geom_sf(mapping = aes(fill= pop_density), lwd = 0, color = NA, show.legend = T) +
  theme_void() + ggtitle('D Population density') +
  scale_fill_distiller(name = expression(paste("Population\nper ", km^2),sep=''), 
                       direction = 1,
                       palette = "PuBuGn",
                       trans = "log10")+ 
  theme(legend.position = c(0.8,0.25), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme( axis.text  = element_blank(), axis.ticks = element_blank(), 
         axis.title = element_text(size = 15),
         title = element_text(size=18),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 

pdf("figs/whole country/map_01_region_state_pop_by_week.pdf", width=8, height=10)
map.dm1 <- cbind( ggplotGrob(plot1), ggplotGrob(plot2),  size = 'last')
map.dm2 <- cbind( ggplotGrob(plot3), ggplotGrob(plot4),  size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()

## Temperature ------
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = t2m), lwd = 0, color = NA) +
  scale_fill_distiller(name = expression(paste("Temperature (",degree,"C)")), 
                       direction = -1,
                       palette = "RdYlBu"
                       # colours = brewer.pal(9, "Oranges"),
                       # trans = "log1p"
                       
                       ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_temp_by_week.pdf", height = 8, width = 12)
plot1
dev.off()

## Humidity -------
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = d2m), lwd = 0, color = NA) +
  scale_fill_distiller(name = 'Relative humidity (%)', 
                       direction = 1,
                       palette = "RdYlBu"
                       # colours = brewer.pal(9, "Oranges"),
                       # trans = "log1p",
                       # breaks = c(-10, 0, 10, 20, 30),
                       # labels = c(-10, 0, 10, 20, 30)
  ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_humidity_by_week.pdf", height = 8, width = 12)
plot1
dev.off()

## Precipitation ------
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = tp), lwd = 0, color = NA) +
  scale_fill_distiller(name = 'Precipitation (m)', 
                       direction = 1,
                       palette = "Oranges",
                       trans = "log1p"
  ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_prec_by_week.pdf", height = 8, width = 12)
plot1
dev.off()

## UV ------
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = uv/1000), lwd = 0, color = NA) +
  scale_fill_distiller(name = expression(paste("UV (KJ/ ", m^2, " per hour)")), 
                       direction = 1,
                       palette = "Purples",
                       trans = "log1p"
  ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_UV_by_week.pdf", height = 8, width = 12)
plot1
dev.off()

## Intervention stringency -----
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = Stringency), lwd = 0, color = NA) +
  scale_fill_distiller(name = 'Stringency Index (%)', 
                       direction = 1,
                       palette = "Blues"
  ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_Stringency_by_week.pdf", height = 8, width = 12)
plot1
dev.off()

## Internal mobility ----
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = mean_intra), lwd = 0, color = NA) +
  scale_fill_distiller(name = 'Relative mobility', 
                       direction = -1,
                       palette = "RdYlBu",
                       trans = "log1p",
                       breaks = c(0.25, 0.5, 1, 1.5, 2),
                       labels = c(0.25, 0.5, 1, 1.5, 2)
                       
  ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_Mobility_by_week.pdf", height = 8, width = 12)
plot1
dev.off()

## case rates ----
plot1 <- ggplot(data.map) +
  geom_sf(mapping = aes(fill = cases_rate*10), lwd = 0, color = NA) +
  scale_fill_distiller(name = 'Cases per 1000 people', 
                       direction = 1,
                       palette = "Reds",
                       trans = "log1p",
                       breaks = c(0.1, 1, 10, 25, 100),
                       labels = c(0.1, 1, 10, 25, 100)
  ) +
  # ggtitle('Week') +
  facet_wrap(~ week, ncol = 5)+
  theme_void() + theme(legend.position = c(0.7,0.15))

pdf("figs/whole country/map_01_case_rate_by_week.pdf", height = 8, width = 12)
plot1
dev.off()
