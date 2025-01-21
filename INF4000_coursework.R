#----------------------------------Packages---------------------------------------------
library(tidyverse)
#install.packages("hexbin")
library(hexbin)
#install.packages("fmsb")
library(fmsb)
#install.packages("viridis")
library(viridis)
#install.packages("ggpattern")
library(ggpattern) 
#----------------------------------Loading the data-------------------------------------

data_ <- read_csv("dataset.csv")

data_ <- select(data_,-1) # removing the index column

#----------------------------------Handling missing value-------------------------------

sum(is.na(data_$track_id) * 1) # to see if there is any missing value in track_id column

missing_values <- data_[rowSums(is.na(data_) * 1)>0,]

nrow(missing_values) # shows the number of the rows

data_clean <- data_[!data_$track_id %in% missing_values$track_id,] # removing rows that their track_id is in the missing_values data frame

#----------------------------------Scatter Plot (Popularity/genre)-------------------------------

GP_SP <- data_clean[,c("track_name","popularity","track_genre")] # Genre popularity data set for Scatter Plot

GP_SP_summation <- GP_SP %>%                            # summation of the popularity to identify most popular and least popular
  group_by(track_genre) %>%
  summarize(total_popularity = sum(popularity, na.rm = TRUE))

view(GP_SP_summation)
GP_SP_summation <- arrange(GP_SP_summation,desc(total_popularity))
GP_SP_summation$track_genre[1:4] # top 4
tail(GP_SP_summation$track_genre,2) #bottom 2

#dividing them to top 4 categories and last 2 , everything else

GP_SP$track_genre <-                       
  case_when(
    GP_SP$track_genre %in% c("chill","k-pop", "sad" , "pop-film") ~ "top4",
    GP_SP$track_genre %in% c("iranian", "romance") ~ "bottom2",
    TRUE ~ "other"
  )


# visualization of the song popularity distribution by genre

ggplot(GP_SP, aes(x = track_name, y = popularity, color = track_genre)) + 
  geom_point(  size = 1) + 
  scale_color_manual(values = c("top4" = rgb(0,0,1), "bottom2" = rgb(1,0,0), "other" = rgb(0.82,0.82,0.82,0.25) ) ) + 
  labs(x="Songs", y="Popularity",
       title="Song Popularity Distribution by Genre",
       caption="Spotify DataSet") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),    
    axis.ticks.x = element_blank(),  
    axis.text.y = element_text(size = 12),       
    axis.title.x = element_text(size = 14),      
    axis.title.y = element_text(size = 14),      
    plot.title = element_text(size = 16, hjust = 0.5),  
    legend.title = element_text(size = 12),      
    legend.text = element_text(size = 10)        
  )



#----------------------------------Hexbin Plot for track / artist poplarity-----

#Data Preparation

H_TR <- data_clean[, c("artists","popularity", "track_name" )]

Artist_popularity_variance <- H_TR %>%
  separate_rows(artists, sep = ";") %>%
  group_by(artists) %>%
  summarise(Average_Popularity = var(popularity))

view(Artist_popularity_variance)

Artist_popularity <- H_TR %>%
  separate_rows(artists, sep = ";") %>%
  group_by(artists) %>%
  summarise(Average_Popularity = mean(popularity))

H_TR_V <- merge(H_TR, Artist_popularity, by.x = "artists", by.y = "artists")


#Visualization

hexbin_ <- hexbin(y = H_TR_V$popularity, x = H_TR_V$Average_Popularity, xbins = 25)

ggplot(data = H_TR_V, aes(x = Average_Popularity, y = popularity)) +
  geom_hex(bins = 25) +
  scale_fill_viridis_c(option = "A", direction = -1) +
  labs(title = "Hexbin Plot for Track Popularity vs Artist Popularity",
       x = "Artist Popularity",
       y = "Track Popularity") +
  theme_minimal()



#----------------------------------Spider Plot for 10genre/rest-----------------

#preparing the data

Spider_data <- data_clean

Spider_data_to10 <- Spider_data %>%                            # summation of the popularity to identify most popular genres
  group_by(track_genre) %>%
  summarize(total_popularity = sum(popularity))

Spider_data_to10 <- arrange(Spider_data_to10,desc(total_popularity))
Spider_data_to10$track_genre[1:5] # top 4

Spider_data$track_genre <-                       
  case_when(
    Spider_data$track_genre %in% c("pop-film","k-pop","chill","sad") ~ "top4",
    TRUE ~ "Rest"
  )

Spider_data <- Spider_data[, -1:-5]

#Normalizing the data between 1 ad 0 for visualiztion

Spider_data$explicit <- Spider_data$explicit * 1

for (col in 1:14) {
  Spider_data[[col]] <- (Spider_data[[col]] - min(Spider_data[[col]])) / (max(Spider_data[[col]]) - min(Spider_data[[col]]))
}

# calculating the mean for each characteristic

Spider_data <- Spider_data %>%                            
  group_by(track_genre) %>%
  summarize(duration_ms = mean(duration_ms),
            explicit = mean(explicit),
            danceability = mean(danceability), 
            energy = mean(energy), 
            key = mean(key), 
            loudness = mean(loudness), 
            mode = mean(mode),
            speechiness = mean(speechiness), 
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness), 
            liveness = mean(liveness), 
            valence = mean(valence),
            tempo = mean(tempo),
            time_signature = mean(time_signature) )



# spider pot visualization

spider_V <- rbind(
  rep(1, ncol(Spider_data)-1),  
  rep(0, ncol(Spider_data)-1),  
  Spider_data[,-1]  
)

radarchart(spider_V,
           axistype = 1,                           
           pcol = c("blue", "red"),                
           pfcol = c(rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.3)),  
           plwd = 4,                               
           cglcol = "grey",                        
           cglty = 1,                              
           axislabcol = "black",                   
           caxislabels = seq(0, 1, 0.2),           
           vlcex = 0.8,                            
           title = "Comparison of top10 and Rest Genres"
)
view(spider_V)

colors <- viridis(2, alpha = 0.7)
radarchart(spider_V,
           axistype = 1,                           
           pcol = colors,                         
           pfcol = adjustcolor(colors, alpha.f = 0.3),  
           plwd = 4,                               
           cglcol = "grey",                        
           cglty = 1,                              
           axislabcol = "black",                   
           caxislabels = seq(0, 1, 0.2),           
           vlcex = 0.8,                            
           title = "Comparison of Top 10 and Rest Genres"
)



#----------------------------------Stacked bar chart for top 10 artists-------
SB_data <- data_clean[,- c(1,3,20,4)]

SB_data_top10 <- SB_data %>%                            # summation of the popularity to identify most popular Artists
  group_by(artists) %>%
  summarize(sum_popularity = sum(popularity))

SB_data_top10 <- arrange(SB_data_top10,desc(sum_popularity))
SB_data_top10$artists[1:10]

SB_data_V <- SB_data[SB_data$artists %in% c("The Beatles", "Linkin Park", "BTS", "Prateek Kuhad",
                                            "Elvis Presley", "Arctic Monkeys", "Charlie Brown Jr.", 
                                            "Red Hot Chili Peppers", "CoComelon", "Rammstein"),]

for (col in 2:ncol(SB_data_V)) {
  SB_data_V[[col]] <- (SB_data_V[[col]] - min(SB_data_V[[col]])) / (max(SB_data_V[[col]]) - min(SB_data_V[[col]]))
}

SB_data_V <- SB_data_V %>%                            
  group_by(artists) %>%
  summarize(duration_ms = mean(duration_ms),
            explicit = mean(explicit),
            danceability = mean(danceability), 
            energy = mean(energy), 
            key = mean(key), 
            loudness = mean(loudness), 
            mode = mean(mode),
            speechiness = mean(speechiness), 
            acousticness = mean(acousticness),
            instrumentalness = mean(instrumentalness), 
            liveness = mean(liveness), 
            valence = mean(valence),
            tempo = mean(tempo),
            time_signature = mean(time_signature) )

view(SB_data_V)

SB_data_V_long <- SB_data_V %>%
  pivot_longer(cols = danceability:time_signature, names_to = "characteristic", values_to = "value")


ggplot(SB_data_V_long, aes(x = artists, y = value, fill = characteristic)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Stacked Bar Chart of Artist Characteristics",
    x = "Artists",
    y = "Average Value",
    fill = "Characteristic"
  ) +
  scale_fill_viridis_d() +  #
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  s
    axis.text.y = element_text(size = 12),                       
    axis.title.x = element_text(size = 14),                       
    axis.title.y = element_text(size = 14),                       
    plot.title = element_text(size = 16, hjust = 0.5),            
    legend.title = element_text(size = 12),                       
    legend.text = element_text(size = 10)                         
  )


#----------------------------------Line chart-------------------------------
P_Data <- data_clean

P_Data <- arrange(P_Data,desc(popularity))
P_Data$artists[1:100] # top 100

artists_list <- P_Data$artists[1:100]

artists_split <- strsplit(artists_list, ";") %>% unlist() # strsplit(artists_list, ";") this returns a list where each element is an vector and unlist makes the whole thins into a big vecotor


artist_freq_table <- table(artists_split)

artist_freq_df <- as.data.frame(artist_freq_table)

colnames(artist_freq_df) <- c("Artist", "Frequency")

artist_popularity <- P_Data %>%
  separate_rows(artists, sep = ";") %>%
  group_by(artists) %>%
  summarise(Average_Popularity = mean(popularity))

artist_Freq_popularity <- merge(artist_freq_df, artist_popularity, by.x = "Artist", by.y = "artists")


ggplot(artist_summary, aes(x = Average_Popularity, y = Frequency)) +
  geom_line(color = "yellow", size = 1) +
  geom_point(color = "darkblue", size = 3) +
  theme_minimal() +
  labs(
    title = "Line Graph: Artist Popularity vs. Song Frequency",
    x = "Average Popularity",
    y = "Song Frequency"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


