specimens_loc <- data_FC %>% 
  filter(file_name == file_name[2])

View(specimens_loc)
# get unique coordinates
sites <- unique(specimens_loc[,c("lon_dd", "lat_dd")])

# calculate distances
x <- st_distance(sites)  

# to remove the units (st_distance output is a matrix with units, by setting to null we can cluster using kmeans)
units(x) <- NULL

# assign distance vector - will hold the maximum distance given a number of clusters
max_dist_results <- vector(length = nrow(specimens_loc))
min_dist_results <- vector(length = nrow(specimens_loc))

#Loop over 1 - 100 clusters
for (i in 1:(nrow(x)-1)) {
  #cluster locations into i groups
  clust <- kmeans(x, centers = i)
  #set up variable to record the maximum
  max_dist <- 0
  min_dist <- Inf
  #loop over the j number of clusters
  for (j in 1:i) {
    #work out the maximum distance in the jth cluster
    new_max <- max(x[clust$cluster == j, clust$cluster == j])
    #if this new cluster has a greater distance than the previous record then record it
    if (new_max > max_dist) {
      max_dist <- new_max
    }
    
    for(k in 1:i){
      if(k != j){
      new_min <- min(x[clust$cluster == j, clust$cluster == k])
      if(new_min < min_dist){
        min_dist <- new_min
      } 
      }
    }
  }
  #save maximum distance given i clusters
  max_dist_results[i] <- max_dist
  min_dist_results[i] <- min_dist 
}

#plot the max distance vs the number of clusters
plot(max_dist_results)
points(min_dist_results, col = "red")

min_dist_results

plot(dist_results * 1 : length(dist_results))

scatter.smooth(dist_results )

#cluster by site


#plot shows 19 is sweet spot, below and the distance is much higher, above and there is little change is distance
clust <- kmeans(x, centers = 3)

#join the clusters to the unique sites data frame
sites$cluster <- clust$cluster
sites$cluster <- pamk.best[["pamobject"]]$clustering

#add data to the main dataframe
all_data_clust <- st_join(specimens_loc, sites)

#check the join worked and the longitudes are the same
all(all_data_clust$lon_dd.x == all_data_clust$lon_dd.y)

  #map_brazil <- st_read('2.data_raw/other_map_files/Biomas_Brasil.shp')

extent <- all_data_clust %>%
  group_by(cluster) %>%
  summarise(extent = st_as_sfc(st_bbox(geometry)))


# plot map and points
ggplot() +
  geom_sf(data = map_brazil, 
          (aes(fill = NOME))) +
  scale_fill_brewer(palette = "Pastel1")+
  # add study locations onto map 
  geom_sf(data = all_data_clust, aes(color = cluster), size = 4) +
  geom_sf(data = extent, aes(color = cluster), fill = NA)+
  # geom_text(data = all_data, aes(x= lon_dd, y= lat_dd, label = Locality), size = 2, check_overlap = TRUE)+
  coord_sf() +
  theme_classic()

# plot map and points
ggplot() +
  geom_sf(data = map_brazil, 
          (aes(fill = NOME))) +
  scale_fill_brewer(palette = "Pastel1")+
  # add study locations onto map 
  geom_sf(data = all_data_clust, aes(color = cluster), size = 4) +
  geom_sf(data = specimens_extent)+
  # geom_text(data = all_data, aes(x= lon_dd, y= lat_dd, label = Locality), size = 2, check_overlap = TRUE)+
  coord_sf() +
  theme_classic()




#data_FC$n_cluster <- NA

for (i in 1:nrow(specimens_loc)){
  print(i)
  
    
    # get the cluster
    n_cluster <- extent$cluster[i]
    
    # calculate if specimens points intersect with i cluster
    intersect_loc <- st_intersects(extent$extent[i], data_FC$geometry, sparse = FALSE)
    
    data_FC$n_cluster[intersect_loc] <- n_cluster
    
  
}
i=2


#-----------------------
library(fpc)
pamk.best <- pamk(x, krange = 1:5)

pamk.best$nc[1]
pamk.best$crit
