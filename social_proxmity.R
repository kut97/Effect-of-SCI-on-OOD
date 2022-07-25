#### R script for deaths social proximity###
#### death social proximity of the ego ZIP code is the average of OOD###
###in alter counties weighted by their social connectedness with the ego county##
#### deaths_social_proximity = frac{/sum_j/*SCI_{i,j}}{/sum_h/*SCI_{i,h}}
### here SCI_{i,h} is the measure to formualte how intensively two geographic locations are connected with each other###


library(tidyverse)
library(igraph)
library(readr)

### reading the sci tsv file. Availbale at Facebook's data for good###
df_0 <- read_tsv ('zcta_zcta_shard1.tsv')

### converting the sci into relative probabilities by dividing with the highest value of connections

df_0 <- df_0 %>% dplyr::mutate(probabilites=scaled_sci/(1000000000)) 


#### reading the data frame that has insights about demographc, geographic information about OOD events####

final_df <- read.csv('final_df.csv')

#### extracting OOD events ZIP CODES##

zip_code <- final_df$zipcode

#### extracting SCI probabilities for available zip codes###
df_1 <- df_0 %>% dplyr::filter(user_loc %in% zip_code & fr_loc %in% zip_code)
```
### Finding for what zip codes SCI proability is not available###

left_over_zip_codes <- zip_code[!(zip_code %in% df_1$user_loc)]

## droping rows from the main dataframe for which SCI is not available##
final_df <- final_df[ ! final_df$zipcode %in% left_over_zip_codes, ]



## avoiding duplication using distinct function
df_1 <- df_1 %>% distinct(probabilites,.keep_all = TRUE)


##zip code wise death counts###
q_i <- final_df[,c(2,31)]


`## renaming the zip code column as fr_loc to facilitate merge
colnames(q_i)[1] <- "fr_loc"


## merging data frame to perfrom weighted average
df_s <- merge(df_1,q_i,by ="fr_loc")


### weighted deaths alter counties ###
df_s <- df_s %>% mutate(wt_sci=probabilites*deaths_per_capita)
#### creating adjacency matrix for wweighted sci for each column and diagonlising the matrix with 0
dataframe_for_matrix <- df_s %>% dplyr::select(c(user_loc,fr_loc,wt_sci))
nodes <- df_s %>% distinct(user_loc)
g <- graph.data.frame(dataframe_for_matrix, directed=F, vertices=nodes)
sci_proximity <- as_adjacency_matrix(g,attr = "wt_sci",sparse = F)
diag(sci_proximity) <- 0

### to find the sum of pair of zip codes with respect to given pairwise probabilities
df_for_matrix_probability <- df_s %>% dplyr::select(c(user_loc,fr_loc,probabilites))
df_for_matrix_probability
k <- graph.data.frame(df_for_matrix_probability, directed=F, vertices=nodes)
cumulative_sci <- as_adjacency_matrix(k,attr="probabilites",sparse=F)
row_wise_sum_sci <- rowSums(cumulative_sci)
row_wise_sum_sci


#### using sweep function to calculate deaths_social_proximity 
## here sci_proximity_zip=deaths_social_proximity
sci_proximity <- sweep(sci_proximity,2,row_wise_sum_sci,FUN="/")
#sci_proximity <- mapply("/",sci_proximity,row_wise_sum_sci)
sci_proximity_zip <- rowSums(sci_proximity)
sci_proximity_zip
```

