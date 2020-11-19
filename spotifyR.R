library(spotifyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(ggjoy)
library(kableExtra)

Sys.setenv(SPOTIFY_CLIENT_ID = '383cfac3d8434244a38c4e279a04ce47')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '486c6f539f8e45a2b82f03a30cfed26f')

access_token <- get_spotify_access_token()

beatles <- get_artist_audio_features('the beatles')

beatles %>% 
  dplyr::count(key_mode, sort = TRUE) %>% 
  head(5) %>% 
  kbl(format = "html")

# Get 5 most recently played
access_token <- get_spotify_authorization_code(scope = 'user-read-recently-played')
get_my_recently_played(limit = 5, authorization = access_token) %>% 
  dplyr::mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = lubridate::as_datetime(played_at)) %>% 
  dplyr::select(track.name, artist.name, track.album.name, played_at) %>% 
  kbl(format = "html") %>% kable_material_dark(full_width = F, html_font = "Cambria")

# Long term favorite artists
setwd("/Users/dunk/Projects/SpotifyStats")
favoriteartists <- get_my_top_artists_or_tracks(type = 'artists', time_range = 'short_term', limit = 20) %>% 
  select(name, popularity, genres, uri) %>% 
  rowwise %>% 
  mutate(genres = paste(genres, collapse = ', ')) %>% 
  ungroup 

# Short term favorite artists
get_my_top_artists_or_tracks(type = 'tracks', time_range = 'short_term', limit = 20) %>% 
  mutate(artist.name = map_chr(artists, function(x) x$name[1])) %>% 
  select(name, artist.name, album.name) %>% 
  kable("html") %>% kable_styling(bootstrap_options = c("striped", "hover")) %>%
  cat(., file = "df.html") # This prints as an html dataframe not super useful just interesting

# Most recently played songs
get_my_recently_played(limit = 5, authorization = access_token) %>% 
  mutate(artist.name = map_chr(track.artists, function(x) x$name[1]),
         played_at = lubridate::as_datetime(played_at)) %>% 
  select(track.name, artist.name, track.album.name, played_at)

# Using valence as a measure on Kanye
joy <- get_artist_audio_features('Kanye West')
joy %>% 
  dplyr::arrange(-valence) %>% 
  dplyr::select(track_name, valence) %>% 
  head(5) %>% 
  kbl(format = "html")

# Plotting valence
ggplot(joy, aes(x = valence, y = album_name, fill = ..y..)) + 
  geom_joy() + 
  theme_joy() +
  ggtitle("Joyplot of Joy Division's joy distributions", subtitle = "Based on valence pulled from Spotify's Web API with spotifyr")


# Making a dataframe based on my own Spotify
my_id <- 'dungates'
my_plists <- get_user_playlists(my_id)

# Filters for in top 50
# my_plists2 <- my_plists %>%
#   filter(name %in% c('Taiwan Top 50', 'France Top 50', 'Bolivia Top 50', 'U.S. Top 50'))

# Honestly not sure what access token vs authorization code is probably not necessary to run since authorization is default set to get_sp
# otify access token frequently
# access_token <- get_spotify_access_token() 


# Taking main playlist by groups of 100 cause that is the max then appending
tracks1 <- get_playlist_tracks(playlist_id = "6slH6T3IWAPgHzTJBk8ot9")
tracks2 <- get_playlist_tracks(playlist_id = "6slH6T3IWAPgHzTJBk8ot9", offset = 100)
tracks3 <- get_playlist_tracks(playlist_id = "6slH6T3IWAPgHzTJBk8ot9", offset = 200)
tracks4 <- get_playlist_tracks(playlist_id = "6slH6T3IWAPgHzTJBk8ot9", offset = 300)
tracks <- do.call("rbind", list(tracks1, tracks2, tracks3, tracks4))

# Getting track audio features by groups of 100 then appending cause that is the max
features1 <- get_track_audio_features(tracks1$track.id)
features2 <- get_track_audio_features(tracks2$track.id)
features3 <- get_track_audio_features(tracks3$track.id)
features4 <- get_track_audio_features(tracks4$track.id)
features <- do.call("rbind", list(features1, features2, features3, features4))

GoodData <- tracks %>%
  select(playlist_name, track.name, artist_name, track.album.name, track_uri) %>%
  left_join(features, by = "track_uri")

feats <- feats %>%
  filter(str_detect(playlist_name, "'")) %>%
  separate(playlist_name, into = c("month", "year"), sep = " '") %>%
  mutate(month = str_trim(month))


# pluck out only the months we've run through in 2018
feats <- feats %>%
  filter(
    year %in% c(17, 18),
    !month %in% c("September", "October", "November", "December")
  )

feats$month <- factor(feats$month, levels = month.name[1:8], labels = month.abb[1:8])

# Make a radar chart

library(fmsb)
library(radarchart)


data <- features %>% select(danceability, energy, key, loudness, mode, speechiness, acousticness, instrumentalness,
                            liveness, valence, tempo) %>% select(-key, -tempo)
colMax <- function(data) sapply(data, max, na.rm = TRUE)
colMax(data)

# data <- rbind(rep(60,12) , rep(0,12) , data)

fmsb::radarchart(data, axistype=1, title = paste0(my_id, "'s audio features"),
           
           #custom polygon
           pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
           
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,60,12), cglwd=0.8,
           
           #custom labels
           vlcex=0.8
)










# New Stuff
playlists <- get_user_playlists("dungates") # to get playlist uri
features <- get_playlist_audio_features("Good", "6slH6T3IWAPgHzTJBk8ot9")


library(rvest)
url <- "https://spotifycharts.com/regional/ca/daily/"
timevalues <- seq(as.Date("2020/09/28"), as.Date("2020/10/08"), by = "day")

unitedata<- function(x){
  full_url <- paste0(url, x)
  full_url
}

finalurl <- unitedata(timevalues)


SpotifyScrape <- function(x){
  page <- x
  rank <- page %>% read_html() %>% html_nodes('.chart-table-position') %>% html_text() %>% as.data.frame()
  track <- page %>% read_html() %>% html_nodes('strong') %>% html_text() %>% as.data.frame()
  artist <- page %>% read_html() %>% html_nodes('.chart-table-track span') %>% html_text() %>% as.data.frame()
  streams <- page %>% read_html() %>% html_nodes('td.chart-table-streams') %>% html_text() %>% as.data.frame()
  dates <- page %>% read_html() %>% html_nodes('.responsive-select~ .responsive-select+ .responsive-select .responsive-select-value') %>% html_text() %>% as.data.frame()
  
  #combine, name, and make it a tibble
  chart <- cbind(rank, track, artist, streams, dates)
  names(chart) <- c("Rank", "Track", "Artist", "Streams", "Date")
  chart <- as.tibble(chart)
  return(chart)
}


spotify <- map_df(finalurl, SpotifyScrape)



# Quick cleaning
spotify %<>% 
  mutate(Artist = gsub("by ", "", Artist), 
         Streams = gsub(",", "", Streams), 
         Streams = as.numeric(Streams), 
         Date = as.Date(spotify$Date, "%m/%d/%Y"))

# Quick analysis

spotify %>% 
  group_by(Artist) %>% 
  summarise(Total = sum(Streams)) %>% 
  arrange(desc(Total)) %>%
  top_n(25, Total) %>%
  ggplot() +
  geom_col(aes(x = reorder(Artist, Total), y = Total), fill = "forest green") +
  coord_flip() + 
  scale_y_continuous(labels = unit_format("B", 1e-9))
















