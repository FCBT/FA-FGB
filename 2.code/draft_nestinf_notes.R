library(tidyverse)
#simulate some data

df <- data.frame(id = rep(1:10, each = 5), x = runif(50) , y =runif(50) ) %>%
        group_by(id) %>%
        mutate(x = cumsum(x), y = cumsum(y))

#use nest/unnnest to apply function to groups

#nest creates a column with data (columns not used in group_by) 
df_nested <- df %>%
    group_by(id) %>%
    nest()

#you can also supply the columns directly to the nest function
df %>%
    nest(data = c(x,y))

#we can see the nested column is just a list of dataframes (tibbles to be precise)
df_nested %>% pull(data)

#once nested we can use map to apply a function to each nested dataframes

#define function takes a dataframe of coordiantes and returns dataframe of distances
dist_df <- function(df){
    
    #assign new dataframe
    df_new <- data.frame(dist = rep(NA,nrow(df)-1))

    #loop over old dataframe (dont need the last point)
    for(i in 1:nrow(df)-1){
        # calculate distance between i and i+1
        df_new$dist[i] <- sqrt((df$x[i] - df$x[i+1])^2 + (df$y[i] - df$y[i+1])^2) 
    }

    #reuturn new df
    return(df_new)
}

#apply function to nested dataframes and unnest the new column
df_unnested <- df_nested %>%
    mutate(data_transformed = map(data, dist_df)) %>%
    unnest(data_transformed) %>%
    select(-data)


