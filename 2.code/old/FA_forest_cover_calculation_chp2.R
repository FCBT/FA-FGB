library(sf)
dir()

setwd('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1_PROJECT/5_CRIS_BOX')

# Calculating forest cover


# Load point sites and convert to South America Albers equal area conic
saaeac <- '+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 +y_0=0 +ellps=aust_SA +units=m +no_defs'
#sites <- read.csv('./1.2.2.3.Data_cleaned/occupancy_data/occupancy.csv', row.names=1)
my_sites <- read.csv('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2/1.2.2.3.Data_cleaned/LSR_1991-2017_lat-lon-FA_20200623.csv', stringsAsFactors = FALSE)

# drop the occupancy data to keep things simple
#sites <- sites[, 1:5]
#my_sites <- my_sites[,c(2,5:7)]

#sites <- st_as_sf(sites, coords=c('Lon','Lat'), crs=4326)
my_sites <- my_sites %>%
  # get reid of NA in Lat, Lon
  drop_na(Lat, Lon) %>%
  # coerce into character to be able to transform into simple feature in the next step
  mutate(Lat = as.character(Lat),
         Lon = as.character(Lon))

my_sites <- st_as_sf(my_sites,coords=c('Lon','Lat'), crs=4326)

#sites <- st_transform(sites, saaeac)

my_sites <- st_transform(my_sites, saaeac)
my_sites$Bird_ID <-as.factor(my_sites$Bird_ID)

# Get a 600 metre circular buffer around points
radius <- 600
#sites_buffer <- st_buffer(sites, radius)
my_sites_buffer <- st_buffer(my_sites, radius)

# Load 2011 forest cover data, which handily is already in SAAEAC

fc2011 <- st_read('./1.2.2.3.Data_cleaned/GIS_layers/Atlantic Forest cover maps/2010-2011/frags_sos.shp')
fc2011 <- st_transform(fc2011, saaeac)
# get the intersection of the forest cover and the buffer,
# reasonably quick (<1 minute) on reasonable laptop
# - these produce multiple rows per buffer, one for each intersecting fragment
#sites_buffer_fc_2011 <- st_intersection(fc2011, sites_buffer)
my_sites_buffer_fc_2011 <-st_intersection(fc2011, my_sites_buffer)

# get the areas of the intersections
#sites_buffer_fc_2011$fc_2011_area <- as.numeric(st_area(sites_buffer_fc_2011))
my_sites_buffer_fc_2011$fc_2011_area <-as.numeric(st_area(my_sites_buffer_fc_2011))

# aggregate and convert to proportions
#sites_total_fc_2011 <- aggregate(fc_2011_area ~ study + Sites, data=sites_buffer_fc_2011, FUN=sum)
my_sites_total_fc_2011 <- aggregate(fc_2011_area ~ Bird_ID + Species, data = my_sites_buffer_fc_2011, FUN=sum)

# for some reason, the step above doubles values... look into this
my_sites_total_fc_2011$fc_2011_area <-ifelse((my_sites_total_fc_2011$fc_2011_area > (pi*radius^2)), my_sites_total_fc_2011$fc_2011_area/2, my_sites_total_fc_2011$fc_2011_area)

#sites_total_fc_2011$fc_2011_prop <- sites_total_fc_2011$fc_2011_area / (pi * radius ^2)
my_sites_total_fc_2011$fc_2011_prop <- my_sites_total_fc_2011$fc_2011_area /(pi * radius ^2)
#a <- pi*600^2
# merge that back into sites
#sites <- merge(sites, sites_total_fc_2011, by=c('study', 'Sites'), all.x=TRUE)
my_sites <-merge(my_sites, my_sites_total_fc_2011, by=c('Bird_ID', 'Species'), all.x=TRUE)

# CONVERT NAs into zero. The NA means that the buffered point did not 
# intersect with any fragments
my_sites$fc_2011_area <-ifelse(is.na(my_sites$fc_2011_area), 0, my_sites$fc_2011_area)
my_sites$fc_2011_prop <-ifelse(is.na(my_sites$fc_2011_prop), 0, my_sites$fc_2011_prop)

# save a simple csv of the site forest covers
st_geometry(my_sites) <- NULL

fn <- sprintf('C:/Users/Flavia/OneDrive - Imperial College London/1_PHD/1.2.R-dir/1.2.2.chapter-2/1.2.2.3.Data_cleaned/LSR_FA_forest_cover_IF_%s.csv', format(Sys.time(), '%Y%m%d'))
write.csv(my_sites, fn)


#--------------------------------------------------------------------------------------------
########## acho que para mim acaba aqui ##################

# Load 2014 forest cover data, which is split by state

fc_2014_files <- dir('./GIS_layers/Atlantic Forest cover maps/2013-2014/', 
                     pattern='*.shp$', full.names=TRUE)
fc_2014 <- lapply(fc_2014_files, st_read)

# check same projection and fields
sapply(fc_2014, st_crs)
sapply(fc_2014, names)

# combine the states and reproject, 
# reasonably quick (< 1 minute)
fc_2014_all <- do.call(rbind, fc_2014)
fc_2014_all <- st_transform(fc_2014_all, saaeac)

# get the intersection of the forest cover and the buffer,
# reasonably quick (<1 minute) on reasonable laptop
# - these produce multiple rows per buffer, one for each intersecting fragment
sites_buffer_fc_2014 <- st_intersection(fc_2014_all, sites_buffer)

# get the areas of the intersections
sites_buffer_fc_2014$fc_2014_area <- as.numeric(st_area(sites_buffer_fc_2014))
# aggregate and convert to proportions
sites_total_fc_2014 <- aggregate(fc_2014_area ~ study + Sites, data=sites_buffer_fc_2014, FUN=sum)
sites_total_fc_2014$fc_2014_prop <- sites_total_fc_2014$fc_2014_area / (pi * radius ^2)

# add that back into sites
sites <- merge(sites, sites_total_fc_2014, by=c('study', 'Sites'), all.x=TRUE)

# CONVERT NAs into zero. The NA means that the buffered point did not 
# intersect with any fragments
sites$fc_2011_area <- ifelse(is.na(sites$fc_2011_area), 0, sites$fc_2011_area)
sites$fc_2011_prop <- ifelse(is.na(sites$fc_2011_prop), 0, sites$fc_2011_prop)
sites$fc_2014_area <- ifelse(is.na(sites$fc_2014_area), 0, sites$fc_2014_area)
sites$fc_2014_prop <- ifelse(is.na(sites$fc_2014_prop), 0, sites$fc_2014_prop)

my_sites$fc_2011_area <-ifelse(is.na(my_sites$fc_2011_area), 0, sites$fc_2011_area)
my_sites$fc_2011_prop <-ifelse(is.na(my_sites$fc_2011_prop), 0, sites$fc_2011_prop)

# compare to the existing FC600
pdf('FC_comparison.pdf', width=4, height=11)

par(mfrow=c(3,1), mar=c(3,3,2,1), mgp=c(1.8,0.8,0))

cor_2011 <- cor.test(~ FC600 + fc_2011_prop, data=sites)
cor_2014 <- cor.test(~ FC600 + fc_2014_prop, data=sites)
r_2011 <- cor_2011$estimate
r_2014 <- cor_2014$estimate

plot(FC600 ~ fc_2011_prop, data=sites, xlim=c(0,1), ylim=c(0,1),
     main=bquote('FC600 v 2011 Forest cover, ' ~ r_p == .(round(cor_2011$estimate, 3))))
abline(0,1)
plot(FC600 ~ fc_2014_prop, data=sites, xlim=c(0,1), ylim=c(0,1),
     main=bquote('FC600 v 2014 Forest cover, ' ~ r_p == .(round(cor_2014$estimate, 3))))
abline(0,1)
plot(fc_2011_prop ~ fc_2014_prop, data=sites, xlim=c(0,1), ylim=c(0,1),
     main='2011 v 2014')
abline(0,1)

dev.off()

# save a simple csv of the site forest covers
st_geometry(sites) <- NULL
write.csv(sites, 'forest_cover_IF.csv')



