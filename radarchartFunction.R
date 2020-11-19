library(plotly)
library(tidyverse) 
library(kableExtra)
library(DT)
library(corrplot) 
library(gridExtra) 
library(treemap)
library(viridisLite) 
library(fmsb) 
library(cowplot) 
library(factoextra)
library(formattable)
library(GGally)



# Sys.setenv(SPOTIFY_CLIENT_ID = '383cfac3d8434244a38c4e279a04ce47')
# Sys.setenv(SPOTIFY_CLIENT_SECRET = '486c6f539f8e45a2b82f03a30cfed26f')

access_token <- get_spotify_access_token()

spotifyViz <- function(User, Playlist) {
  # First we get all user playlists
  playlists <- get_user_playlists(User)
  # Filter for playlist selected
  filtered <- dplyr::filter(playlists, name == Playlist)
  # Get all features of that playlist with name and uri as documentation requires
  features <- get_playlist_audio_features(filtered[1,6], substr(filtered[1, 11], 18, nchar(playlists[1,11])))
  # Some quick cleaning
  features_clean <- features %>% filter(!is.na(track.name) & !is.na(track.album.name))
  features_clean$year <- as.numeric(substring(features_clean$track.album.release_date,1,4))
  features_clean <- features_clean %>% dplyr::select(-track.id,-track.album.id,-playlist_id)
  # Reorder columns so numerical data first, sorted by year descending
  features_clean <- features_clean %>% relocate(year, danceability, energy, key, loudness, mode, speechiness, acousticness,
                                                instrumentalness, liveness, valence, tempo, track.duration_ms, 
                                                track.popularity, track.artists, 
                                                track.name, track.album.name) %>% arrange(desc(year))
  # Making a quick correlogram, energy and loudness only significant correlation. Acousticness negatively correlated w/ 
  # loudness and energy. Track duration slightly negative with valence = -0.4 which is interesting
  corrplot <- features_clean %>% dplyr::select(year, danceability, energy, key, loudness, mode, speechiness, acousticness,
                                   instrumentalness, liveness, valence, tempo, track.duration_ms) %>% ggcorr(method = "pairwise",
                                                                                                             label = T)
  ggsave("/Users/dunk/Projects/SpotifyStats/Plots/Correlogram.png", corrplot)
  # Making a density plot
  correlated_density <- features_clean %>% select(energy, danceability, valence, acousticness, speechiness, liveness) %>% 
    ggplot() +
    geom_density(aes(energy, fill ="energy", alpha = 0.1)) + 
    geom_density(aes(danceability, fill ="danceability", alpha = 0.1)) + 
    geom_density(aes(valence, fill ="valence", alpha = 0.1)) + 
    geom_density(aes(acousticness, fill ="acousticness", alpha = 0.1)) + 
    geom_density(aes(speechiness, fill ="speechiness", alpha = 0.1)) + 
    geom_density(aes(liveness, fill ="liveness", alpha = 0.1)) + 
    scale_x_continuous(name = "Energy, Danceability, Valence, Acousticness, Speechiness, and Liveness") +
    scale_y_continuous(name = "Density") +
    ggtitle("Density plot of Energy, Danceability, Valence, Acousticness, Speechiness, and Liveness") +
    theme_bw() +
    theme(plot.title = element_text(size = 10, face = "bold"),
          text = element_text(size = 10)) +
    theme(legend.title=element_blank()) +
    scale_fill_brewer(palette="Accent")
  
  ggsave("/Users/dunk/Projects/SpotifyStats/Plots/DensityPlot.png", correlated_density)
  
  # Making a radar chart with valence, danceability, energy, speechiness, and liveliness
  radarChartData <- features_clean %>% select(valence, danceability, energy, speechiness, liveness) %>%
    summarise_all(list(mean), na.rm = T)
  radarChartData <- rbind(rep(1, 5), rep(0, 5), radarChartData)
  fmsb::radarchart(radarChartData, axistype=1, 
             
             #custom polygon
             pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4, 
             
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1,.2), cglwd=0.8,
             
             #custom labels
             vlcex=0.8 )
  
  # Test code for now
  # p <- ggplot(features_clean, aes(year, track.popularity)) + geom_point()
  # ggplotly(p)
}

# Run program
spotifyViz("dungates", "Good")





# Here's a table maker for later
# head(features_clean, 20) %>%
#   datatable(options = list(scrollCollapse = TRUE,scrollX = TRUE,
#                            columnDefs = list(list(className = 'dt-center', targets = 1:4))
#   ))




