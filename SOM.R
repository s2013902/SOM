library(kohonen)
library(ggplot2)
library(rgdal)
library(gridExtra)
library(grid)
library(viridis) 
library(dplyr)

#read in the boundary data for the Edinburgh area, already contains the SIMD attributes 
ed_map <- readOGR("SG_SIMD_2016_EDINBURGH.shp", stringsAsFactors = FALSE)

#convert the object into latitude and longitude for easier use with ggmap 
ed_map <- spTransform(ed_map, CRS("+proj=longlat +ellps=WGS84 
+datum=WGS84 +no_defs"))	 

#convert the data contains in the shapfile to data fram
ed_data <- as.data.frame(ed_map) 

#convert spatial polygon to dataframe including columns of spatial information
ed_fort <- fortify(ed_map, region= "DataZone") 

#merge the new dataframe with the irish census data using their shared column 
ed_fort <- merge(ed_fort, ed_data, by.x="id", by.y="DataZone")

#define index1 to standardlize correlation value
index1 <- function(x) (x-min(x))/diff(range(x))
#define index2 to standardize negative correlation value
index2 <- function(x) (max(x)-x)/diff(range(x))

#for health
#set weights of different indexes 
wts <- c(0.07, 0.1, 0.06, 0.37, 0.22, 0.15, 0.02)

#select health data
health <- select(ed_data, HlthCIF, HlthAlcSR, HlthDrugSR, HlthSMR, HlthDprsPc, HlthLBWTPc, HlthEmrgSR)

#get the standardized value
st_health <- apply(health, 2, index1)

# get sum of weighted rows 
weight_health <- t(t(st_health) * wts)
final_health<-rowSums(weight_health[,c(1,2,3,4,5,6,7)])

#for Education
#set weights of different indexes
wts1 <- c(0.21, 0.23, 0.12)
wts2 <- c(0.29, 0.15)

#select education data 
education <- select(ed_data, EduAttend, EduAttain, EduNoQuals, EduNEET, EduHESA)

#get the standardized value
st1_education <- apply(education[,c(1,2,5)], 2, index2)
st2_education <- apply(education[,c(3,4)], 2, index1)

# get sum of weighted rows
weight1_education <- t(t(st1_education) * wts1)
weight2_education <- t(t(st2_education) * wts2)
final_education <- rowSums(cbind(weight1_education, weight2_education))

#for housing
#set weights of different indexes
wts <- c(0.5, 0.5)

#select housing data 
housing <- select(ed_data, HouseOCrat, HouseNCrat)

#get the standardized value
st_housing <- apply(housing, 2, index1)

# get sum of weighted rows
weight_housing <- t(t(st_housing) * wts)
final_housing <- rowSums(weight_housing[,c(1,2)])

#for access
#set weights of different indexes
wts <- c(0.16, 0.1, 0.14, 0.073, 0.1, 0.093, 0.1567, 0.093, 0.083)

#select access data 
access <- select(ed_data, GAccPetrol, GAccDTGP, GAccDTPost, GAccDTPsch, GAccDTRet, GAccDTSsch, GAccPTGP, GAccPTPost, GAccPTRet)

#get the standardized value
st_access <- apply(access[,c(1,2,3,4,5,6,7,8,9)], 2, index1)

# get sum of weighted rows
weight_access <- t(t(st_access) * wts)
final_access <- rowSums(weight_access[,c(1,2,3,4,5,6,7,8,9)])



income <- ed_data[,10]
employment <- ed_data[,13]
crime <- ed_data[,41]


#Bringing all the variables into a data frame
final_cluster<-as.data.frame(matrix(c(income,employment,final_health,final_education,final_access,crime,final_housing),nrow=length(final_access)))
names(final_cluster) <- c("Income", "Employment", "Health","Education&Training", "Access", "Crime", "Housing")



#standardise the data creating z-scores and convert to a matrix
data_train_matrix <- as.matrix(scale(final_cluster))

#keep the column names of final_cluster as names in our new matrix
names(data_train_matrix) <- names(final_cluster)

#define the size, shape and topology of the som grid
som_grid <- somgrid(xdim = 11, ydim=11, topo="hexagonal")

# Train the SOM model
som_model <- som(data_train_matrix, grid=som_grid, rlen=600, alpha=c(0.1,0.01), keep.data = TRUE )

dev.off()
# Plot of the training progress - how the node distances have stabilised over time. 
plot(som_model, type = "changes") 

# load custom palette, created by Shane Lynn
source('coolBlueHotRed.R')

#counts within nodes 
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed) 

#map quality 
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed) 

#code spread 
plot(som_model, type = "codes",palette.name=rainbow) 

# Plot the heatmap for a variable at scaled / normalised values
par(mfrow=c(4,2))
for(i in 1:7){
  plot(som_model, type = "property", property = getCodes(som_model)[,i], cex.main=1.5,main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed, shape="straight",  border="white")
}

par(mfrow=c(1,1))

# show the WCSS metric for kmeans for different clustering sizes. 
# Can be used as a "rough" indicator of the ideal number of clusters 
mydata <- getCodes(som_model) 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) wss[i] <- sum(kmeans(mydata,	 centers=i)$withinss)	 

# Form clusters on grid
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 7) 

# Colour palette definition. This is a palette designed for discrete data that is colour blind friendly 
cbPalette <- c("#CC79A7", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00","orange3") 

#show the same plot with the codes instead of just colours 
plot(som_model, type="codes",palette.name=rainbow, bgcol = cbPalette[som_cluster], main = 
       "SIMD Cluster",  shape="straight",  border="white") 

add.cluster.boundaries(som_model, som_cluster)


#create dataframe of the small area id and of the cluster unit
cluster_details <- data.frame(id=ed_data$DataZone, cluster=som_cluster[som_model$unit.classif])

#we can just merge our cluster details onto the fortified spatial polygon dataframe we created earlier
mappoints <- merge(ed_fort, cluster_details, by.x="id", by.y="id")

# Colour palette definition
class_colors <- c("coral3","orange3", "darkseagreen4", "khaki3", "lightpink3","mediumpurple2", "aquamarine3","peru")

# Finally map the areas and colour by cluster
ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) + 
  geom_polygon(colour="transparent")  + theme_void() +
  coord_equal() + 
  scale_fill_manual(name="Clusters", values = cbPalette) 


# combine map with cluster details
ed_map <- merge(ed_map, cluster_details, by.x="DataZone", by.y="id")

# save as an esri shapefile
writeOGR(obj=ed_map, dsn="edinburgh_cluster", layer="edinburgh_map_clustered", driver="ESRI Shapefile")
