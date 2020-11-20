library(spotifyr)
library(plyr)
library(tidyverse)
library(httr)
library(rvest)
library(stringr)
library(ggthemes)
library(tidytext)
library(wordcloud)
library(ggridges)
library(wesanderson)
library(yarrr)
library(knitr)
library(kableExtra)
library(radarchart)


# set up Spotify client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = '383cfac3d8434244a38c4e279a04ce47')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '486c6f539f8e45a2b82f03a30cfed26f')


#using spoitfyr
kanye <- get_artist_audio_features('kanye west')

kanyealbums <- unique(kanye$album_name)
kanyealbums <- kanyealbums[-c(11,12,6,7,1,2)]

kanye <- kanye %>% filter(album_name %in% kanyealbums)
# The Spotify data for Kanye West changed a little in the week between my pulling it and posting this code.



# Getting artist ID on Genius
token <- "QZWrzUmE0Kg2QvTjynbk2gqnIV9S_FaE_Cax36z54N_FcvDgZHGHB2sngC5-JYiD"
genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('kanye west')



# Getting track urls
baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}



# Filtering to get urls only for tracks on which Kanye West is the primary artist
filtered_track_lyric_urls <- c()
filtered_track_lyric_titles <- c()
index <- c()


for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$primary_artist$name == "Kanye West") {
    filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
    filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
    
    index <- append(index, i)
    test <- data.frame(filtered_track_lyric_titles, filtered_track_lyric_urls)
    return(test)
  }
}

test$filtered_track_lyric_titles <- as.character(test$filtered_track_lyric_titles)
test <- test %>% distinct(.keep_all = T)
test$filtered_track_lyric_titles[699] <- "Through The Wire"
test$filtered_track_lyric_titles[760] <- "We Don't Care"
test$filtered_track_lyric_titles[338] <- "I'll Fly Away"
test$filtered_track_lyric_titles[624] <- "Slow Jamz"
test$filtered_track_lyric_titles[595] <- "School Spirit Skit 1"
test$filtered_track_lyric_titles[596] <- "School Spirit Skit 2"
test$filtered_track_lyric_titles[417] <- "Lil Jimmy Skit"
# test$filtered_track_lyric_titles[699] <- "Last Call" #Missed this one when scraping ):
test$filtered_track_lyric_titles[615] <- "Skit #1 (Kanye West/Late Registration)"
test$filtered_track_lyric_titles[616] <- "Skit #2 (Kanye West/Late Registration)"
test$filtered_track_lyric_titles[617] <- "Skit #3 (Kanye West/Late Registration)"
test$filtered_track_lyric_titles[618] <- "Skit #4 (Kanye West/Late Registration)"
test$filtered_track_lyric_titles[718] <- "Touch The Sky"
test$filtered_track_lyric_titles[292] <- "Heard 'Em Say"
test$filtered_track_lyric_titles[161] <- "Diamonds From Sierra Leone - Remix"
test$filtered_track_lyric_titles[158] <- "Diamonds From Sierra Leone - Bonus Track"
test$filtered_track_lyric_titles[114] <- "Can't Tell Me Nothing"
test$filtered_track_lyric_titles[45] <- "All Of The Lights"
test$filtered_track_lyric_titles[45] <- "All Of The Lights (Interlude)"
test$filtered_track_lyric_titles[154] <- "Devil In A New Dress"
test$filtered_track_lyric_titles[302] <- "Hell Of A Life"
test$filtered_track_lyric_titles[427] <- "Lost In The World"
test$filtered_track_lyric_titles[779] <- "Who Will Survive In America	"
test$filtered_track_lyric_titles[598] <- "See You In My Nightmares"
test$filtered_track_lyric_titles[94] <- "Blood On The Leaves"
test$filtered_track_lyric_titles[329] <- "I Am A God"
test$filtered_track_lyric_titles[345] <- "I'm In It"
test$filtered_track_lyric_titles[215] <- "Father Stretch My Hands Pt. 1"
test$filtered_track_lyric_titles[229] <- "Frank's Track"
test$filtered_track_lyric_titles[481] <- "No More Parties In LA"
test$filtered_track_lyric_titles[787] <- "Wouldn't Leave"


kanye_lyric_titles <- test$filtered_track_lyric_titles %>% str_to_title() %>% as_tibble() %>%
  right_join(kanye, by = c("value" = "track_name")) %>% distinct(value, .keep_all = T) %>% view()

kanye_lyrics <- left_join(kanye_lyric_titles, test, by = c("value" = "filtered_track_lyric_titles")) %>% distinct(value, .keep_all = T) %>%
  dplyr::rename(track_name = value) %>% arrange(album_release_date) %>% relocate(filtered_track_lyric_urls, .after = track_name) %>%
  drop_na(filtered_track_lyric_urls)

# Fixing inconsistencies between track names on Spotify and Genius

# Do this later

# (some indexes might need to be changed since Spotify data has changed)

# Webscraping lyrics using rvest after making a NA column and changing urls to characters
kanye_lyrics$filtered_track_lyric_urls <- as.character(kanye_lyrics$filtered_track_lyric_urls)
kanye_lyrics$lyric_text <- rep(NA, nrow(kanye_lyrics))

# Function to scrape lyrics
scrape <- function(x) {
  read_html(x) %>% 
    html_nodes(".lyrics p") %>% 
    html_text()
}
# scrape lyrics based on genius url
kanye_lyrics$lyric_text <- purrr::map(kanye_lyrics$filtered_track_lyric_urls, scrape) # rewrite with purrr later
kanye_lyrics$lyric_text <- as.character(kanye_lyrics$lyric_text)
# Function for Cleaning and standardizing lyrics

# Consider readding tolower() for better analysis
kanye_lyrics <- kanye_lyrics %>% mutate(lyric_text = gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text)) %>%
                                          mutate(lyric_text = gsub("\n", " ", lyric_text)) %>%
                                        mutate(lyric_text = gsub("\\[.*?\\]", " ", lyric_text)) %>%
                                        mutate(lyric_text = gsub(" {2,}", " ", lyric_text))
  

genius_data <- data.frame(track_name = kanye_lyrics$track_name, lyrics = kanye_lyrics$lyric_text)
genius_data$track_name <- as.character(genius_data$track_name)
genius_data$lyrics <- as.character(genius_data$lyrics)

# readr::write_rds(kanye_lyrics, "/Users/dunk/Projects/SpotifyStats/kanye-lyrics.rds")
kanye_lyrics_csv <- kanye_lyrics %>% select(track_name, lyric_text)
# readr::write_csv(kanye_lyrics_csv, "/Users/dunk/Projects/SpotifyStats/kanye-lyrics.csv")

# Removing tracks that would interfere with lyric analysis
# taylor <- taylor[!(taylor$track_name == "Forever & Always - Piano Version" | 
#                      taylor$track_name == "Treacherous - Original Demo Recording" |
#                      taylor$track_name == "Teardrops on My Guitar - Pop Version"),]


# joining Spotify and Genius data
spotify_genius <- full_join(genius_data, kanye, by = "track_name") %>% distinct(track_name, .keep_all = T)


# adding "ordered_albums", with album names as factors
# spotify_genius$album_name[spotify_genius$album_name == "Fearless Platinum Edition"] <- "Fearless"

ordered_albums <- factor(spotify_genius$album_name)
ordered_albums <- factor(ordered_albums,levels(ordered_albums)[c(5,3,2,1,4,8,6,7)])
spotify_genius$ordered_albums <- ordered_albums

options("digits" = 3)


# valence ridge plot (I used fig.height = 6, fig.width = 6 in an rmd)
spotify_genius %>% ggplot(aes(x = valence, y = ordered_albums, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "maroon3") + 
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  ggtitle("Kanye's Valence By Album: A Clear Path of Misery") + 
  theme(legend.position = "none")

# graph of valence v. energy

ggplot(spotify_genius, aes(x = valence, y = energy, color = album_name, alpha = tempo)) + 
  geom_text(aes(label = track_name)) + 
  geom_vline(xintercept = 0.5) +
  geom_hline(yintercept = 0.5) +
  annotate(geom = "text", label = "Happy", x = 0.9, y = 0.9) +
  annotate(geom = "text", label = "Angry", x = 0.1, y = 0.9) +
  annotate(geom = "text", label = "Calm", x = 0.9, y = 0.1) +
  annotate(geom = "text", label = "Sad", x = 0.1, y = 0.1) +
  facet_wrap( ~ ordered_albums) +
  scale_fill_gradient(low = "white", high = "maroon3") + 
  theme_fivethirtyeight() + 
  labs(x = "Valence (Happiness)", y = "Energy", color = "Album", alpha = "Tempo") + scale_alpha(guide = "none") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  ggtitle("Kanye's Energy and Valence") + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

geom_text(aes(label = ifelse(`Share of top 5% of wealth` > 50, as.character(Country),'')))

# table: album by mean valence
spotify_genius %>% 
  group_by(album_name) %>% 
  summarise(mean(valence)) %>% 
  arrange(desc(`mean(valence)`)) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  kableExtra::row_spec(row = 1:6, background = "#fffce4", color = "red")

# table: top 5 songs by valence
spotify_genius %>% 
  select(track_name, album_name, valence) %>% 
  top_n(5) %>% 
  arrange(-valence) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "deeppink")

# sonic score graph
pirateplot(valence + danceability + energy ~ album_release_year, spotify_genius,
           pal = c(wes_palettes$GrandBudapest2, wes_palettes$Moonrise3[1:2]), 
           xlab = "album", ylab = "sonic score",
           theme = 0, point.o = 0.7, avg.line.o = 1, jitter.val = .05, 
           bty = "n", cex.axis = 0.6, xaxt = "n") 
axis(1, cex.axis = 0.6, lwd = 0)
legend("topright", c("1: Kanye West", "2: Fearless", "3: Speak Now", "4: Red", "5: 1989", "6: reputation"), bty = "n", cex = 0.6) 


# 1989 sonic scores
spotify_genius %>% 
  mutate(sonic_score = valence + danceability + energy) %>% 
  select(album_name, track_name, sonic_score) %>% 
  arrange(desc(sonic_score)) %>% 
  filter(album_name == "1989") %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:13, background = "seashell", color = "#b39db2")


# album by danceability
spotify_genius %>% 
  group_by(album_name) %>% 
  summarise(mean(danceability)) %>% 
  arrange(desc(`mean(danceability)`)) %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1, background = "seashell", color = "#b39db2")


# tokenized and cleaned datasets of lyrics for textual analysis
tidy_kanye <- spotify_genius %>% unnest_tokens(word, lyrics)
tidier_kanye <- tidy_kanye %>% anti_join(rbind(stop_words[1], "uh", "yeah", "hey", "baby", "ooh", "wanna", "gonna", "ah", "ahh", "ha", "la", "mmm", "whoa", "haa"))
tidier_kanye$word[tidier_kanye$word == "don" | tidier_kanye$word == "didn"] <- NA
tidier_kanye$word[tidier_kanye$word == "ain"] <- NA
tidier_kanye$word[tidier_kanye$word == "isn"] <- NA
tidier_kanye$word[tidier_kanye$word == "usin"] <- "using"
tidier_kanye$word[tidier_kanye$word == "wouldn"] <- "wouldn't"
tidier_kanye$word[tidier_kanye$word == "couldn"] <- "couldn't"
tidier_kanye$word[tidier_kanye$word == "shouldn"] <- "shouldn't"
tidier_kanye$word[tidier_kanye$word == "won"] <- "won't"
tidier_kanye$word[tidier_kanye$word == "ve" | tidier_kanye$word == "ll"] <- NA
tidier_kanye$word[tidier_kanye$word == "ileft"] <- "left"


# wordcloud: all
word_count <- tidier_kanye %>%
  dplyr::count(word, sort = TRUE) %>% 
  dplyr::mutate(word = reorder(word, n)) %>%
  dplyr::ungroup()

wordcloud(words = word_count$word, freq = word_count$n,
          max.words=100, random.order=FALSE, 
          colors= c(wes_palettes$Moonrise3[c(1:2,5)], wes_palettes$Royal2[5]))


# how many tracks does the word "kanye" appear in?
tidier_kanye %>% 
  select(track_name, word) %>% 
  filter(word == "kanye") %>% 
  unique() %>% 
  select(track_name) %>% kbl(format = "html") %>% kable_styling()

# wordcloud: make this function work by album later
word_count_ts <- tidier_kanye %>%
  filter(album_name == "ye") %>% 
  dplyr::count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count_ts$word, freq = word_count_ts$n,
          max.words=25, random.order=FALSE, 
          colors= c(wes_palettes$GrandBudapest2[3:1]))

# wordcloud: reputation
word_count_rep <- tidier_kanye %>%
  filter(album_name == "My Beautiful Dark Twisted Fantasy") %>% 
  dplyr::count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count_rep$word, freq = word_count_rep$n,
          max.words=25, random.order=FALSE, 
          colors= c(wes_palettes$GrandBudapest2[3:1]))


# more cleaning, can be done earlier
tidier_kanye$album_release_year <- as.character(tidier_kanye$album_release_year)
tidier_kanye$album_release_year <- as.numeric(substr(tidier_kanye$album_release_year, 1, 4))

tidy_kanye$album_release_year <- as.character(tidy_kanye$album_release_year)
tidy_kanye$album_release_year <- as.numeric(substr(tidy_kanye$album_release_year, 1, 4))


# creating a "lexical diversity" dataset
lexical_diversity <- tidy_kanye %>% dplyr::group_by(track_name, album_release_year) %>% 
  dplyr::mutate(lex_div = length(unique(word))/length(word)) %>% 
  select(track_name, lex_div, album_release_year) %>% 
  distinct()


# lexical diversity plot
pirateplot(lex_div ~ album_release_year, lexical_diversity,
           pal = c("cyan3", "darkgoldenrod1", "maroon4", "red3", "#b39db2", "black", "forestgreen", "skyblue"),
           xlab = "album", ylab = "lexical diversity",
           theme = 0, point.o = 0.5, avg.line.o = 1, jitter.val = .05, 
           bty = "n", cex.axis = 0.6, xaxt = "n")
axis(1, cex.axis = 0.6, lwd = 0)
legend("topright", c("1: The College Dropout", "2: Late Registration", "3: Graduation", 
                     "4: 808s & Heartbreak", "5: My Beautiful Dark Twisted Fantasy", "6: Yeezus", "7: The Life of Pablo",
                     "8: ye"), bty = "n", cex = 0.6)


# least lexically diverse tracks
tidy_kanye %>% dplyr::group_by(track_name, album_name) %>% 
  dplyr::mutate(lex_div = length(unique(word))/length(word)) %>% 
  select(track_name, lex_div, album_name) %>% 
  arrange(lex_div) %>% 
  distinct() %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "palevioletred")



# joining the tokenized, tidied lyric dataset with sentiment lexicons
kanye_nrc_sub <- tidier_kanye %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

kanye_AFINN <- tidier_kanye %>% 
  inner_join(get_sentiments("afinn"))

kanye_bing <- tidier_kanye %>% 
  inner_join(get_sentiments("bing"))


colfunc <- colorRampPalette(c("skyblue", "deepskyblue4"))

# sentiment scores using AFINN
dim <- kanye_AFINN %>% 
  dplyr::count(album_name)
kanye_AFINN %>%
  dplyr::group_by(ordered_albums) %>% 
  dplyr::summarise(sum(value)) %>% 
  dplyr::mutate(scaled = `sum(value)` * 229 / dim$n) %>% 
  ggplot(aes(x = ordered_albums, y = scaled, fill = ordered_albums)) +
  geom_bar(stat = "identity") +
  ylim(-200,200) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  theme(legend.position="none")



# 1989 pyramid plot
kanye_pyramid <- kanye_bing %>%
  unique() %>% 
  dplyr::group_by(track_name, sentiment, album_name) %>%
  dplyr::count(track_name, sentiment)

for(i in 1:24) {
  if(kanye_pyramid$sentiment[i] == "negative")
    kanye_pyramid$n[i] <- -kanye_pyramid$n[i]
}

kanye_pyramid %>% 
  ggplot(aes(x = track_name, y = n, fill = sentiment)) + 
  geom_bar(subset = .(sentiment == "positive"), stat = "identity") + 
  geom_bar(subset = .(sentiment == "negative"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  coord_flip() +
  theme_fivethirtyeight() +
  ylim(-20,10) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("palevioletred", "olivedrab3")) +
  theme(legend.position="none")



# all-album radar chart
sentiment_nrc <- kanye_nrc_sub %>%
  dplyr::group_by(ordered_albums, sentiment) %>%
  dplyr::count(ordered_albums, sentiment) %>% 
  select(ordered_albums, sentiment, sentiment_total = n)

album_nrc <- kanye_nrc_sub %>%
  dplyr::count(ordered_albums) %>% 
  select(ordered_albums, album_total = n)

radar_chart <- sentiment_nrc %>% 
  inner_join(album_nrc, by = "ordered_albums") %>% 
  mutate(percent = round((sentiment_total/album_total * 100), 3)) %>% 
  select(-sentiment_total, -album_total) %>%
  spread(ordered_albums, percent)

radar_chart <- radar_chart[c(2,7,5,8,4,3,1,6), c(1, 7:2)]

# Cool Radar Chart of Spotify Data
chartJSRadar(radar_chart, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 25,
             colMatrix = matrix(c(0, 255, 255, 255, 185, 15, 139, 0, 139, 
                                  255, 0, 0, 201, 167, 198, 0, 0, 0), byrow = F, nrow = 3))

text_wordcounts <- tidy_kanye %>% select(track_name, album_name, word) 
text_wordcounts$word <- as.factor(text_wordcounts$word)
text_wordcounts <- text_wordcounts %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% ungroup

# Ranking most common words in Kanye Songs
text_wordcounts %>% dplyr::mutate(word = reorder(word, n)) %>% anti_join(stop_words) %>% 
  filter(n > 100) %>%
  ggplot(aes(word, n)) + geom_col() + coord_flip() + 
  labs(x = "Word", y = "Count", title = "Most Frequent Words in Kanye West's Songs") + 
  geom_text(aes(label = n), hjust = 1.2, color = "white", fontface = "bold") + 
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face = "bold", color = "forestgreen", size = 12),
        axis.title.y = element_text(face = "bold", color = "forestgreen", size = 12))

# plotly::ggplotly(ggplot2::ggplot(text_wordcounts, aes(x=word, y=n, fill=word)) + 
#                    geom_bar(width = 0.75, stat = "identity", colour = "black", size = 1) + 
#                    xlab("") + ylab("") + ggtitle("Word Frequency") + theme(legend.position = "none") + 
#                    labs(x = NULL, y = NULL) + theme(plot.subtitle = element_text(vjust = 1), plot.caption = element_text(vjust = 1), 
#                                                     axis.text.x = element_text(angle = 90)) + 
#                    theme(panel.background = element_rect(fill = "honeydew1"), 
#                          plot.background = element_rect(fill = "antiquewhite"))) %>% 
#   config(displaylogo = F) %>% config(showLink = F)
text_wordcounts %>% dplyr::mutate(word = reorder(word, n)) %>% anti_join(stop_words) %>% 
  filter(n > 50) %>% ggplot2::ggplot(aes(x=word, y=n, fill=word)) + 
  geom_bar(width = 0.75,  stat = "identity", colour = "black", size = 1) + 
  coord_polar(theta = "x") + xlab("") + ylab("") + 
  ggtitle("Word Frequency") + theme(legend.position = "none") + labs(x = NULL, y = NULL)
