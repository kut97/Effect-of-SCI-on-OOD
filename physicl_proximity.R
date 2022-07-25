###### R script for death distance proximity###
####  death physical proximity of the ego zip code ###
####is the average OOD in alter counties weighted by there distance from the ego zip code##

#### deahts_physical_proximity= /sum_j/q_j/*{1/1+{d_{i,j}}###
### d_{i,j}= geographical distance between two locations###

library(geodist)
library(tidyverse)
#final_df <- final_df %>% mutate(deaths_per_capita = deaths/population)


##Extract latitude and longitude 

df <- final_df %>% dplyr::select(zipcode, lat, lng)
df <- df[order(df$zipcode),]
colnames(df)[2] <- "latitude"
colnames(df)[3] <- "longitude"
df <- df[,c(1,3,2)]

##Creating Distance Matrix

distance_matrix <- geodist(df, measure = 'geodesic' )/1000 #converting it to km
distance_matrix

##Converting it into the form of (1/1+d_i_j)

distance_matrix <- (1+distance_matrix)
distance_matrix<- distance_matrix^(-1)
distance_matrix

##naming zip codes

colnames(distance_matrix) <- df$zipcode
rownames(distance_matrix) <- df$zipcode
d_i_j<- data.frame(distance_matrix)

### dataframe for the matrix and diagnoling the elements###

zip_distance_proximity<- d_i_j
colnames(zip_distance_proximity) <- df$zipcode
rownames(zip_distance_proximity) <- df$zipcode
diag(zip_distance_proximity) <- 0
zip_distance_proximity 

#### deaths per capita for zip codes

v <- final_df$deaths_per_capita
v

### sweep function to construct deaths for social proximity

zip_physical_proximity_dij <- sweep(zip_distance_proximity,2,v,FUN="*")
zip_physical_proximity <- rowSums(zip_physical_proximity_dij)
zip_physical_proximity
