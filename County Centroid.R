install.packages("sf")
library(sf)
data1 <- read.csv("C:\\Users\\khush\\Downloads\\12be4f9f574e92413ea3f92ce1bc58e6-3f18230058afd7431a5d394dab7eeb0aafd29d81\\us_county_latlng.csv")
sf_data1 <- st_as_sf(data1, coords = c("lng", "lat"), crs = 4326)
sf_data1 <- st_transform(sf_data1, 4326)
centroid1 <- st_centroid(sf_data1)
centroid_coords1 <- st_coordinates(centroid1)
centroid_latitude1 <- centroid_coords1[1]
centroid_longitude1 <- centroid_coords1[2]

# Calculate distances from centroid to each county
distances <- st_distance(centroid1, sf_data1)

# Extract distances as a separate column in the data frame
sf_data1$distance_to_centroid <- distances
View(sf_data1)

