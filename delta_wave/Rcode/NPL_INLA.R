#setwd("C:/Users/cwr1g15")
setwd("C:/Users/Cori")

#Load the package for building the map and import the shapefile
library(maptools)
library(spdep)
library(plyr)
library(ggplot2)
library(rgdal)
library(INLA)
library(stats)
library(car)
library(gridExtra)
#--- The data ---#

#Individual Level Information
npl_resp<-read.csv("Google Drive/Manuscripts/NPL Adolescent Births/Adolescent Births Research/NPL Adolescent Motherhood/NPL_Respondents_Admin3.csv",header=TRUE,fill=TRUE,na.strings="NA")

#Admin III Shapefile
npl_map<-readShapePoly("Google Drive/Manuscripts/NPL Adolescent Births/Adolescent Births Research/NPL Adolescent Motherhood/Boundaries/NPL_adm3.shp",
                      proj4string=CRS("+proj=longlat +ellps=WGS84"))





#Create covariates and outcome

#Sample size per district
npl_map$N=0
for (i in 1:dim(npl_map)[1])
{
  subN = subset(npl_resp,POLYID==i)
  npl_map$N[i] = (dim(subN)[1])
}


#Outcomes
#y1 - LT16
npl_map$LT16=0
for (i in 1:dim(npl_map)[1])
{
  suby1 = subset(npl_resp,POLYID==i)
  npl_map$LT16[i] = (sum(suby1$LT16))
}

#y2 - 16 to 17
npl_map$x_1617=0
for (i in 1:dim(npl_map)[1])
{
  suby2 = subset(npl_resp,POLYID==i)
  npl_map$x_1617[i] = (sum(suby2$X_16TO17))
}


#y3 - 18 to 19
npl_map$x_1819=0
for (i in 1:dim(npl_map)[1])
{
  suby3 = subset(npl_resp,POLYID==i)
  npl_map$x_1819[i] = (sum(suby3$X_18TO19))
}

#y4 - Mean Age at First Birth
npl_map$AGEFSTBIRT=0
for (i in 1:dim(npl_map)[1])
{
  suby4 = subset(npl_resp,POLYID==i)
  npl_map$AGEFSTBIRT[i] = (mean(suby4$AGEFSTBIRT,na.rm = T))
}


#y5 - LT20
npl_map$LT20=0
for (i in 1:dim(npl_map)[1])
{
  suby5 = subset(npl_resp,POLYID==i)
  npl_map$LT20[i] = (sum(suby5$LT20))
}


#Covariates
#x1 - % Rural
npl_map$ruralprop=0
for (i in 1:dim(npl_map)[1])
{
  subx1 = subset(npl_resp,POLYID==i)
  npl_map$ruralprop[i] = (length(which(subx1$URBAN_RURA=="R"))/(length(subx1$URBAN_RURA)))
}

#x2 - Education
npl_map$noed=0
for (i in 1:dim(npl_map)[1])
  
  #% with No Education 
{
  subx2 = subset(npl_resp,POLYID==i)
  npl_map$noed[i] = ((length(which(subx2$HIGHEST_ED==0)))/(length(subx2$HIGHEST_ED)))   
}


#x3 - Wealth
npl_map$wealth=0
for (i in 1:dim(npl_map)[1])
  
  #% within bottom two quintiles
{
  subx3 = subset(npl_resp,POLYID==i)
  npl_map$wealth[i] = ((length(which(subx3$WEALTH_IND==1|subx3$WEALTH_IND==2)))/(length(subx3$WEALTH_IND))) 
}


#In this section we create the adjacency graph
temp <- poly2nb(npl_map)                    #Polygons
nb2INLA("Nepal.graph", temp)

#This create a file called ``Nepal.adj'' with the graph for INLA
Nepal.adj <- paste(getwd(),"/Nepal.graph",sep="")


#The order of the areas needs to be the same between the data 
#and the spatial polygon object obtained importing the shapefile, so we re-order the data.

npl_map$ID=1:dim(npl_map)[1]
npl_map$ID2 <- npl_map$ID



# # Combine spatial effect + random effect (bym Model)

#LT16
formula.y1 <- npl_map$LT16 ~ 1 + f(ID, model="bym", graph=Nepal.adj) + ruralprop + noed + wealth
result.y1<-inla(formula.y1,family="binomial",data=npl_map@data, Ntrials=N,
               control.compute=list(dic=TRUE,cpo=TRUE))

summary(result.y1) 


#16 to 17
formula.y2 <- npl_map$x_1617 ~ 1 + f(ID, model="bym", graph=Nepal.adj) + ruralprop + noed + wealth
result.y2<-inla(formula.y2,family="binomial",data=npl_map@data, Ntrials=N,
                control.compute=list(dic=TRUE,cpo=TRUE))

summary(result.y2) 


#18 to 19
formula.y3 <- npl_map$x_1819 ~ 1 + f(ID, model="bym", graph=Nepal.adj) + ruralprop + noed + wealth
result.y3<-inla(formula.y3,family="binomial",data=npl_map@data, Ntrials=N,
                control.compute=list(dic=TRUE,cpo=TRUE))

summary(result.y3) 


#Mean age at first birth

formula.y4 <- npl_map$AGEFSTBIRT ~ 1 + f(ID, model="bym", graph=Nepal.adj) + ruralprop + noed + wealth
result.y4<-inla(formula.y4,data=npl_map@data, Ntrials=N,
                control.compute=list(dic=TRUE,cpo=TRUE))

summary(result.y4) 


#LT20
formula.y5 <- npl_map$LT20 ~ 1 + f(ID, model="bym", graph=Nepal.adj) + ruralprop + noed + wealth
result.y5<-inla(formula.y5,family="binomial",data=npl_map@data, Ntrials=N,
                control.compute=list(dic=TRUE,cpo=TRUE))

summary(result.y5) 


#Take mean of posterior distribution to get point estimate at Admin II level
npl_map$preds.LT16<-result.y1$summary.fitted.values$mean
npl_map$preds.1617<-result.y2$summary.fitted.values$mean
npl_map$preds.1819<-result.y3$summary.fitted.values$mean
npl_map$preds.AGEFSTBIRT<-result.y4$summary.fitted.values$mean
npl_map$preds.LT20<-result.y5$summary.fitted.values$mean

summary(npl_map$preds.LT16)
summary(npl_map$preds.1617)
summary(npl_map$preds.1819)
summary(npl_map$preds.AGEFSTBIRT)
summary(npl_map$preds.LT20)

npl_map$CIwidth.LT16 <- (result.y1$summary.fitted.values$`0.975quant` - result.y1$summary.fitted.values$`0.025quant`)
npl_map$CIwidth.1617<-(result.y2$summary.fitted.values$`0.975quant` - result.y1$summary.fitted.values$`0.025quant`)
npl_map$CIwidth.1819<-(result.y3$summary.fitted.values$`0.975quant` - result.y1$summary.fitted.values$`0.025quant`)
npl_map$CIwidth.LT20<-(result.y5$summary.fitted.values$`0.975quant` - result.y1$summary.fitted.values$`0.025quant`)

#Write csv
#write.csv(npl_map@data, file = "Google Drive/Manuscripts/NPL Adolescent Births/Adolescent Births Research/NPL Adolescent Motherhood/Figures/npl_Admin3_Preds.csv")



# 
# 
# #####################################################################################################################################
# #                                                                                                                                   #
# #                                                         GGPLOT                                                                    #
# #                                                                                                                                   #
# #####################################################################################################################################
# 
# 
# 
# ##### LT 16 #####
# npl_map@data$id = rownames(npl_map@data)
# nepal.coords = fortify(npl_map,region = "id")
# nepal.df = join(nepal.coords,npl_map@data, by = "id")
# 
# nepal.plot <- ggplot() + 
#   geom_polygon(aes(x=long,y=lat,group=group,fill=preds.LT16),colour="#6B6B6B",size=.2,data=nepal.df)+ 
#   #scale_fill_gradient(low="#edf8fb",high="#005824",name="Predicted Prevalence") +
#   scale_fill_gradientn(limits=c(0,.15),breaks=c(.02,.04,.06,.08,.1),colours=c("#2b83ba","#abdda4","#ffffbf","#fdae61","#d7191c"),name="Prev") +
#   labs(title = "Nepal") +
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),                                                                                
#         panel.background = element_blank(),
#         plot.title = element_blank(),
#         axis.title=element_blank(), 
#         axis.text=element_blank(), 
#         axis.ticks=element_blank(),                                                                            
#         legend.title = element_text(face = "italic"),
#         legend.key.size = unit(.5, "cm"),
#         #legend.position = "right",
#         plot.margin=unit(c(0,-.5,-.5,-.5), "cm"),
#         axis.line = element_line(colour = "white")) + coord_equal()
# nepal.plot
# 
# 
# 
# ##### Box plot #####
# 
# preds.LT16 <- data.frame(nepal.df$preds.LT16)
# preds.LT16$Var = "Less than 16"
# colnames(preds.LT16) = c("preds","Var")
# 
# preds.1617 <- data.frame(nepal.df$preds.1617)
# preds.1617$Var = "16 to 17"
# colnames(preds.1617) = c("preds", "Var")
# 
# preds.1819 <- data.frame(nepal.df$preds.1819)
# preds.1819$Var = "18 to 19"
# colnames(preds.1819) = c("preds", "Var")
# 
# preds <- rbind(preds.LT16,preds.1617,preds.1819)
# 
# preds$Var = factor(preds$Var)
# preds$Var = factor(preds$Var,levels(preds$Var)[c(3,1,2)])
# 
# cbPalette <- c("#C0C0C0", "#C0C0C0", "#C0C0C0", "#C0C0C0")
# ggplot(preds, aes(Var, preds,fill=Var)) + geom_boxplot() +  
#   ylab("Prevalence") +
#   xlab("Age at First Birth")+
#   scale_fill_manual(values=cbPalette)+
#   theme_bw() + theme(legend.position="none", axis.text.x = element_text(family="serif",colour="black",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
#                      axis.text.y = element_text(family="serif",colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
#                      axis.title.x = element_text(family="sans",colour="black",size=16,angle=0,hjust=.5,vjust=0,face="bold"),
#                      axis.title.y = element_text(family="sans",colour="black",size=16,angle=90,hjust=.5,vjust=.5,face="bold"))