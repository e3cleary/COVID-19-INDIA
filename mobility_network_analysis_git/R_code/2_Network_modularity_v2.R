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
library(cowplot) # inset/join basic maps

# library(ggsci) ## color scheme
# Sys.setlocale(category = "LC_ALL", locale = "chs") # read Chinese
# Sys.setlocale("LC_TIME", "English") # CN to EN for date

setwd('C:/Users/sl4m18/Documents/India/Mobility_network_analysis')

### data preparation -------
# countries
iso3 <- c('IND')
country <- c('India')
i=1

# shapefile 
s1 = read_sf(dsn="data/shapefile",layer="gadm36_IND_1")
s2 = read_sf(dsn="data/shapefile",layer="gadm36_IND_2")

# mobility data
# g1 <- fread("data/India_adm1_GLH.csv", stringsAsFactors=F)
g2 <- fread("data/India_adm2_GLH.csv", stringsAsFactors=F)

#
iso3.remove <- c('IND.1.1_1', 'IND.18.1_1', 'IND.3.3_1')

g2 <- g2[g2$ISO3_s == iso3[i] & g2$ISO3_d == iso3[i] & g2$GID_2_s != g2$GID_2_d,]
g2 <- g2[!(g2$ISO3_s %in% iso3.remove | g2$ISO3_d %in% iso3.remove),]
g2$sum <- g2$sum/min(g2$sum, na.rm = T)*100


### Network structure detection -----
### domestic flows between admin units before and since pandemic
date_begin <- c('2019-11-10', '2020-03-22', '2021-01-31', '2021-04-18', '2021-11-07')
date_end <- c('2020-02-22', '2020-05-02', '2021-03-27', '2021-05-29', '2022-01-01')

d <- data.frame(date_begin = as.Date(date_begin), date_end = as.Date(date_end))

### ~~ admin 2 -----
# all nodes

names <- unique(s2$GID_2[!(s2$GID_2 %in% iso3.remove)])
g2.nodes <- as.data.frame(names)
network_metric <- data.frame()

n=1

for(n in 1:nrow(d)){
  g2.1 <- g2[g2$date_begin >= d$date_begin[n] & g2$date_begin <= d$date_end[n], ]
  wk.no <- length(unique(g2.1$date_begin))
  
  g2.1 <- g2.1[, list(weight = sum(sum, na.rm = T)/wk.no), by='GID_2_s,GID_2_d'] 
  # all nodes
  nodes <- unique(c(g2$GID_2_s, g2$GID_2_d))
  nodes <- as.data.frame(nodes)
  
  nodes_period <- length(unique(c(g2.1$GID_2_s, g2.1$GID_2_d)))
  period.n <- paste0(d$date_begin[n], '_', d$date_end[n])
  
  print(period.n)
  print(nrow(nodes))
  print(nodes_period)
  
  ### turning data frame into igraph objects
  net <- graph_from_data_frame(d=g2.1, vertices=nodes, directed=F) 
  net <- simplify(net, edge.attr.comb=list(weight="sum","ignore"))

  ## Finding community structure by multi-level optimization of modularity https://arxiv.org/abs/0803.0476
  c1 =  cluster_louvain(net) # for undirected graphs 
  
  # save modularity features for nodes
  g2.net <- as.data.frame(cbind(names = c1$names, membership = c1$membership))
  colnames(g2.net)[2] <- paste0('membership', '_', n) # by period
  g2.nodes <- merge(g2.nodes, g2.net, by='names', all.x=T)
  
  # modularity metrics
  modularity(c1) # modularity measure
  length(c1) # number of communities
  sizes(c1)  # size of communities

  # measures of direct network connectivity 
  net.d <- graph_from_data_frame(d=g2.1, vertices=nodes, directed=T) 
  
  edge_density(net.d) # a measure of how interconnected a network is: proportion of edges that actually do exist in a network out of all those that potentially exist between every pair of vertices
  diameter(net.d)
  mean_distance(net.d) # average path length -  the mean lengths between the shortest paths of all pairs of vertices in the network
  transitivity(net.d) # global transitivity: probability that the adjacent vertices of a given vertex are connected
  reciprocity(net.d) # The reciprocity of a directed network reflects the proportion of edges that are symmetrical.
  
  ## save metrics
  network_1 <- data.frame(period = period.n,
                          nodes_all = nrow(nodes),
                          nodes_period = nodes_period,
                          edges_all = nrow(nodes) * (nrow(nodes)-1),
                          edges_period = nrow(g2.1),
                          modularity = modularity(c1), 
                          communities = length(c1), 
                          modularity_algorithm = c1$algorithm,
                          edge_density = edge_density(net.d),
                          diameter = diameter(net.d),
                          mean_distance = mean_distance(net.d),
                          transitivity = transitivity(net.d),
                          reciprocity = reciprocity(net.d))
  
  network_metric <- rbind(network_metric, network_1)
  
}

fwrite(g2.nodes, file =  paste0('data/India_adm2_GLH_community', '_', n,'_periods.csv'), row.names = F)

fwrite(network_metric, file = 'data/India_adm2_GLH_network_metric.csv', row.names = F)


### mapping community ----
# read data
g2.nodes <- fread(paste0('data/India_adm2_GLH_community', '_', n,'_periods.csv'), stringsAsFactors = F)
network_metric <- fread('data/India_adm2_GLH_network_metric.csv', stringsAsFactors = F)

# number of communities
colMax <- function(data) sapply(data, max, na.rm = TRUE)
g2.com.no <-colMax(g2.nodes[, 2:ncol(g2.nodes)])

Period = network_metric$period
g2.com <- as.data.frame(cbind(Period = 1:nrow(network_metric), 
                              com = network_metric$communities))

# barchar
pdf('plots/India_community_number_adm2.pdf', width=5, height=5, onefile = T)

ggplot()+
  geom_bar(data=g2.com, aes(x=Period, weight=com)) + 
  xlab('Periods') + ylab('Number of communities') + ggtitle('Number of communities') +
  theme_bw() + theme(legend.position = c(0.8,0.8), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  theme(axis.title = element_text(size = 15),
         title = element_text(size=16),
         legend.title = element_text(size = 14),
         legend.text = element_text(size=12)) 
  
dev.off()


## ~~ setting colours for admin level 2 ----
library(RColorBrewer)
n <- 31
# cols = rainbow(n)
# cols = c(cols[seq(1,n, by=2)], cols[seq(2,n, by=2)])
# pie(rep(1,n), col=cols)

qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
cols = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
cols = cols[c(1:7,9:15,17:36,55:66)]
par(mar = c(3, 3, 1, 1)) 
pie(rep(1,length(cols)), col=cols)
cols = cols[-c(12,39,23,30,45,21,25,44,16,11,9,14,26,12,18,28)]
pie(rep(1,length(cols)), col=cols)

g2.topcity <- fread('community/India_adm2_GLH_community_top15_colour.csv', stringsAsFactors = F)
g2.topcity$col <- cols[1:nrow(g2.topcity)]
g2.topcity$NAME_2[g2.topcity$GID_2 == 'IND.20.17_1'] <- 'Mumbai'
g2.topcity$NAME_2[g2.topcity$GID_2 == 'IND.25.1_1'] <- 'Delhi'

cols <- cols[-(1:nrow(g2.topcity))]

if((nrow(d)==5)){
  g2.nodes$col_membership_1 <- g2.nodes$membership_1
  g2.nodes$col_membership_2 <- g2.nodes$membership_2
  g2.nodes$col_membership_3 <- g2.nodes$membership_3
  g2.nodes$col_membership_4 <- g2.nodes$membership_4
  g2.nodes$col_membership_5 <- g2.nodes$membership_5
  m=1
  for(m in 1:length(g2.topcity$GID_2)){
    cmm <- g2.nodes$membership_1[g2.nodes$names == g2.topcity$GID_2[m]]
    g2.nodes$col_membership_1[g2.nodes$col_membership_1 == cmm] <- g2.topcity$col[m]
    
    cmm <- g2.nodes$membership_2[g2.nodes$names == g2.topcity$GID_2[m]]
    g2.nodes$col_membership_2[g2.nodes$col_membership_2 == cmm] <- g2.topcity$col[m]
    
    cmm <- g2.nodes$membership_3[g2.nodes$names == g2.topcity$GID_2[m]]
    g2.nodes$col_membership_3[g2.nodes$col_membership_3 == cmm] <- g2.topcity$col[m]
    
    cmm <- g2.nodes$membership_4[g2.nodes$names == g2.topcity$GID_2[m]]
    g2.nodes$col_membership_4[g2.nodes$col_membership_4 == cmm] <- g2.topcity$col[m]
    
    cmm <- g2.nodes$membership_5[g2.nodes$names == g2.topcity$GID_2[m]]
    g2.nodes$col_membership_5[g2.nodes$col_membership_5 == cmm] <- g2.topcity$col[m]
    
    cmm <- g2.nodes$membership_6[g2.nodes$names == g2.topcity$GID_2[m]]
    g2.nodes$col_membership_6[g2.nodes$col_membership_6 == cmm] <- g2.topcity$col[m]
  }

    other <- unique(g2.nodes$col_membership_1[is.na(g2.nodes$col_membership_1)==F & !(g2.nodes$col_membership_1 %in% g2.topcity$col)])
    for(i in 1:length(other)){
          g2.nodes$col_membership_1[g2.nodes$col_membership_1 == other[i]] <- cols[i]
    }
    
    other <- unique(g2.nodes$col_membership_2[is.na(g2.nodes$col_membership_2)==F & !(g2.nodes$col_membership_2 %in% g2.topcity$col)])
    for(i in 1:length(other)){
      g2.nodes$col_membership_2[g2.nodes$col_membership_2 == other[i]] <- cols[i]
    }
    
    other <- unique(g2.nodes$col_membership_3[is.na(g2.nodes$col_membership_3)==F & !(g2.nodes$col_membership_3 %in% g2.topcity$col)])
    for(i in 1:length(other)){
      g2.nodes$col_membership_3[g2.nodes$col_membership_3 == other[i]] <- cols[i]
    }
    
    other <- unique(g2.nodes$col_membership_4[is.na(g2.nodes$col_membership_4)==F & !(g2.nodes$col_membership_4 %in% g2.topcity$col)])
    for(i in 1:length(other)){
      g2.nodes$col_membership_4[g2.nodes$col_membership_4 == other[i]] <- cols[i]
    }
    
    other <- unique(g2.nodes$col_membership_5[is.na(g2.nodes$col_membership_5)==F & !(g2.nodes$col_membership_5 %in% g2.topcity$col)])
    for(i in 1:length(other)){
      g2.nodes$col_membership_5[g2.nodes$col_membership_5 == other[i]] <- cols[i]
    }
    
    other <- unique(g2.nodes$col_membership_6[is.na(g2.nodes$col_membership_6)==F & !(g2.nodes$col_membership_6 %in% g2.topcity$col)])
    for(i in 1:length(other)){
      g2.nodes$col_membership_6[g2.nodes$col_membership_6 == other[i]] <- cols[i]
    }
}

# mapping Domestic inflows for each admin unit
s2.com = merge(s2, g2.nodes, by.x=c('GID_2'), by.y = c('names'), all.x=T)

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

# ~~ Choropleth maps ----
pdf(paste0('plots/GLH_community_admin2_', nrow(d),'_periods.pdf'), width=18, height=7, onefile = T)
m=1
for(m in 1:nrow(d)){

  ## level 2
  # p2 <- case.map + geom_sf(s0, mapping=aes(), fill='lightgrey', colour='darkgrey', size=0.1)
  p2 <- case.map  
  
  if(m==1){ p2 <- p2  + geom_sf(s2.com, mapping = aes(fill = I(col_membership_1)), colour=NA, size=0.1, show.legend = F)  }
  if(m==2){ p2 <- p2  + geom_sf(s2.com, mapping = aes(fill = I(col_membership_2)), colour=NA, size=0.1, show.legend = F)  }
  if(m==3){ p2 <- p2  + geom_sf(s2.com, mapping = aes(fill = I(col_membership_3)), colour=NA, size=0.1, show.legend = F)  }
  if(m==4){ p2 <- p2  + geom_sf(s2.com, mapping = aes(fill = I(col_membership_4)), colour=NA, size=0.1, show.legend = F)  }
  if(m==5){ p2 <- p2  + geom_sf(s2.com, mapping = aes(fill = I(col_membership_5)), colour=NA, size=0.1, show.legend = F)  }

  p2 <- p2 + ggtitle(label= paste0('Level 2', ' (', g2.com.no[m], ')'))
  
  print(p2)
}
dev.off()

# ~~ regions in India -----
ggplot() + 
  geom_sf(s0, mapping=aes(), fill='lightgrey', colour='darkgrey', size=0.1) +
  geom_sf(s1.com, mapping=aes(fill=ZONE, group=ZONE), colour='grey', size=0.1) +
  geom_text_repel(data=s1.com, mapping = aes(label=state, geometry=geometry), 
                  stat = "sf_coordinates",
                  min.segment.length = 0, 
                  # size=3, max.overlaps =5,
                  box.padding = unit(0.3, "lines"),
                  point.padding = unit(0.3, "lines"),
                  colour = "blue",
                  segment.colour = "magenta") +
  xlab('') + ylab('') + 
  theme_bw() + theme(legend.position = c(0.7,0.2), legend.background = element_blank()) +
  theme(panel.grid.major = element_line(colour = 'transparent')) 

# ggsave('plots/India_adm1_state_region.pdf', width=10, height=12)

### ~~ community and connectivity at level 2 ------
## coordinates of each admin unit
# choose a point on the surface of each geometry
s2_points <- sf::st_point_on_surface(s2.com)

# retrieve the coordinates
s2_coords <- as.data.frame(sf::st_coordinates(s2_points))
s2_coords$GID_2 <- s2_points$GID_2

level_2 <- cbind(s2_points[, c("GID_1", "NAME_1", "GID_2", "NAME_2", "membership_1", "membership_2",
                               "membership_3", "membership_4", "membership_5")],
                 s2_coords[, c('X', 'Y')])
# fwrite(level_2, 'community/India_adm2_GLH_community_colour.csv', row.names = F)

# ~~ mapping level 2 network structure -----
map.dm.all <- list()
n=1
panel.title <- c('B', 'C', 'D', 'E', 'F')
for(n in 1:nrow(d)){
  g2.nodes.n <- subset(g2.nodes, select=c('names', paste0('col_membership_', n)))
  colnames(g2.nodes.n) <- c('names', 'membership')
  # edges
  wk.no <- length(unique(g2$date_begin[g2$date_begin>= d$date_begin[n] & g2$date_begin < d$date_end[n]]))
  g2.1 <- g2[g2$date_begin>= d$date_begin[n] & g2$date_begin < d$date_end[n] & g2$GID_2_s != g2$GID_2_d, 
             list(weight = sum(sum, na.rm = T)/wk.no), by='GID_2_s,GID_2_d'] 
  g2.1 <- merge(g2.1, s2_coords, by.x='GID_2_s', by.y='GID_2', all.x=T)
  colnames(g2.1) <- c("GID_2_s", "GID_2_d", "weight",  "X_s", "Y_s")
  g2.1 <- merge(g2.1, s2_coords, by.x='GID_2_d', by.y='GID_2', all.x=T)
  colnames(g2.1) <- c("GID_2_s", "GID_2_d", "weight",  "X_s", "Y_s", "X_d", "Y_d")
  g2.1 <- merge(g2.1, g2.nodes.n, by.x='GID_2_s', by.y='names', all.x=T)
  
  g2.1 <- g2.1[order(g2.1$weight, decreasing= T),]
  g2.1 <- g2.1[1:round(nrow(g2.1)*0.5),]
  g2.1 <- g2.1[order(g2.1$weight, decreasing= F),]
  
  # nodes
  g2.1.s <- g2[g2$date_begin>= d$date_begin[n] & g2$date_begin < d$date_end[n] & g2$GID_2_s != g2$GID_2_d, 
               list(weight = sum(sum, na.rm = T)/wk.no), by='GID_2_s'] 
  g2.1.s <- merge(s2_coords, g2.1.s,  by.x='GID_2', by.y='GID_2_s', all.x=T)
  g2.1.s <- merge(g2.1.s, g2.nodes.n, by.x='GID_2', by.y='names', all.x=T)
  
  g2.1.s <- g2.1.s[order(g2.1.s$weight, decreasing= F),]

  # maps
  map.dm <- case.map +  
    geom_sf(s1, mapping=aes(), fill=NA, colour='azure4', size=0.01) +
    geom_segment(data=g2.1, aes(x=X_s, y=Y_s, xend = X_d, yend = Y_d, colour=I(membership)), 
                 inherit.aes = FALSE, size=0.1, alpha=0.8, show.legend = F) +
    geom_point(data=g2.1.s, aes(x=X, y=Y, size = sqrt(weight), fill=I(membership)),
               shape=21, colour='black', stroke=0.1, alpha=0.8, show.legend = F) + 
    geom_point(data=g2.1.s[is.na(g2.1.s$weight)==T,], aes(x=X, y=Y), size=0.2, colour='black', fill='white', show.legend = F) + 
    scale_size("Relative number of travellers",limits = c(0,sqrt(18000000))) + 
    geom_text_repel(data = g2.topcity[1:6,], aes(x = X, y = Y, label = NAME_2), colour='black',
                    fontface = "bold", nudge_x = c(1, -1.5, 2, 2, -1), 
                    nudge_y = c(0.25, -0.25, 0.5, 0.5, -0.5))+
    # geom_text_repel(data = g2.topcity[1:6,], aes(x = X, y = Y, label = NAME_2), colour='black', 
    #                 box.padding = unit(0.3, "lines"),  point.padding = unit(0.3, "lines")) +
    ggtitle(label= paste(panel.title[n], g2.com.no[n], 'communities')) 
    # ggtitle(label= paste(panel.title[n], g2.com.no[n], 'Communities from', d$date_begin[n], 'to', d$date_end[n]-1)) 
    # theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title=element_blank(), 
    #       axis.text=element_blank(), axis.ticks=element_blank(), panel.background = element_blank(), 
    #       axis.line = element_line(colour = "white"), plot.title = element_text(face = 'bold',size = 15)) 
  
  # map.dm.all[[n]] <-  ggdraw() + draw_plot(map.dm)
  map.dm.all[[n]] <-  map.dm
  
  
}

### ~~~~ India rural and urban classification -------
## urban and rural classification data
level2 <- read_excel('data/DEGURBA/gadm36_L3_2_1_CLASS_opr_IND.xlsx', sheet = 'INDIA_CLASS_L2')
level2$DEGURBA_L1 <- factor(level2$DEGURBA_L1, levels = c('1', '2', '3'),
                            labels = c('Rural', 'Suburban', 'Urban'))
level2$DEGURBA_L2 <- factor(level2$DEGURBA_L2, levels = c('30', '23', '22', '21', '13', '12', '11'),
                            labels = c('Urban center', 'Dense urban cluster', 'Semi-dense urban cluster',
                                       'Suburban', 'Rural cluster', 'Low density rural', 'Very low density'))

ru2 = merge(s2, level2, by=c('GID_2'), all.x=T)

### mapping rural and urban areas ---
map.ru2 <- case.map + 
  geom_sf(ru2, mapping = aes(fill = DEGURBA_L1), colour='grey', size=0.01) +
  ggtitle(label='A')

## ~~~~merge maps----
pdf(paste0('plots/GLH_community_network_admin2_', nrow(d),'_periods.pdf'), width=18, height=14, onefile = T)
map.dm1 <- cbind( ggplotGrob(map.ru2), ggplotGrob(map.dm.all[[1]]), ggplotGrob(map.dm.all[[2]]), size = 'last')
map.dm2 <- cbind( ggplotGrob(map.dm.all[[3]]), ggplotGrob(map.dm.all[[4]]),  ggplotGrob(map.dm.all[[5]]), size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()
## 
pdf(paste0('plots/GLH_community_network_admin2_and rural_urban.pdf'), width=12, height=14, onefile = T)
map.dm1 <- cbind(ggplotGrob(map.ru2 + ggtitle(label='A')), 
                 ggplotGrob(map.dm.all[[1]] + ggtitle(label='B')), size = 'last')
map.dm2 <- cbind(ggplotGrob(map.dm.all[[2]] + ggtitle(label='C')), 
                 ggplotGrob(map.dm.all[[4]] + ggtitle(label='D')), size = 'last')
map.dm <- rbind(map.dm1, map.dm2)
grid.draw(map.dm)
dev.off()
## one by one
pdf(paste0('plots/GLH_community_network_admin2_one_by_one.pdf'), width=6, height=7, onefile = T)
map.dm.all[[1]] + ggtitle(label='')
map.dm.all[[2]] + ggtitle(label='')
map.dm.all[[3]] + ggtitle(label='')
map.dm.all[[4]] + ggtitle(label='')
map.dm.all[[5]] + ggtitle(label='')
dev.off()
