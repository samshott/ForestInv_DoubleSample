library(VoxR)
library(lidR)
library(lidRplugins)
library(sf)
library(rgeos)

#tls <- data.table::fread(system.file("extdata", "Tree_t0.asc", package="VoxR"))


las <- readLAS(choose.files())

epsg(las) <- 6424 #Ellwood

#tls1 <- data.frame(Z = las@data$X,Y = las@data$Y,Z =las@data$Z)

las <- normalize_height(las, knnidw())

plot(las)

CHM <- grid_canopy(las, res=.5, p2r())
#Filter Point cloud ground-ish points
las_noground <- filter_poi(las, Z >=0.3, Classification != 2L)

plot(las_noground)

#get geometry (points) for tree tops

htWdth <- function(x){abs(x*0.293718 - 3.81076)} # created just for Ellwood in excel

ttops20 <- locate_trees(las = las_noground ,lmf(htWdth, hmin = 20))


# OR try and get tree deliniation on pointcloud with LayerStacking - if possible.
# next stip is to crown_metrics for points and/or polygons
LayerStackTrees <- find_trees(las_noground, LayerStacking(1.5, .5, 9, 4.5, 1.8, hmin = 15)) # hmin between 20 and 30 seem to work well




lasPlot <- plot(las_noground)

add_treetops3d(lasPlot, ttops20)

ttopsLi2012 <- segment_trees(las_noground, li2012(dt1 = 10, dt2 = 30, Zu = 50, hmin = 15, speed_up = 30 )) 
# output with large circular boundaries to each tree
# probably better for conifers

segtrees <- segment_trees(las_noground, silva2016(CHM, locMaxttops, max_cr_factor = 0.6, exclusion = .50))
add_treetops3d(plot(segtrees, color = "treeID"), locMaxttops)


#Delineate crown polygons



# use crown polygon "treeID" to loop through chunks of point cloud, 
# at each chunk run VoxR::tree_metrics to get tree information and append
# them to polygon attributes 

for (treePoly in crownPolys$treeID){
  
  
  indvTreePointCloud <- clip_roi(las_noground, treePoly)
  
  indvTreeTable <- data.table::data.table(X = indvTreePointCloud$X, Y = indvTreePointCloud$Y, Z = indvTreePointCloud$Z)
  indvTreeMets <- VoxR::tree_metrics(indvTreeTable)
  
  
  
  
}



VoxR::tree_metrics(tls1)
