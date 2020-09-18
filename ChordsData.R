library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

setwd("/Users/dunk/SpotifyStats")
bb <- read_csv("datasets/bb_chords.csv")

bb_count <- bb %>% count(chord) %>% arrange(desc(n))
# Displaying the top 20 chords
head(bb_count, 20)

# Creating a bar plot from bb_count
bb_count %>%
  slice(1:20) %>%
  mutate(share = n/sum(n),
         chord = reorder(chord, share)) %>%
  ggplot(aes(x = chord, y = share, fill = chord)) +
  geom_col() +
  coord_flip() +
  xlab("Share of Total Chords") +
  ylab("Chord") +
  theme(legend.position = "none")



# A chord change is simply a bigram — a two-"word" phrase — composed of a starting chord and a following chord.
# Wrangling and counting bigrams
bb_bigram_count <- bb %>% mutate(next_chord = lead(chord, 1), next_title = lead(title, 1)) %>%
  mutate(bigram = str_c(chord, next_chord, sep = " ")) %>% filter(title == next_title) %>%
  count(bigram) %>% arrange(desc(n))

# Displaying the first 20 rows of bb_bigram_count
head(bb_bigram_count, 20)


# Visualizing the most common chord progressions

# Creating a column plot from bb_bigram_count
bb_bigram_count %>%
  slice(1:20) %>%
  mutate(share = n/sum(n),
         bigram = reorder(bigram, share)) %>% arrange(desc(n)) %>%
  ggplot(aes(share, bigram, fill = bigram)) +
  geom_col() +
  coord_flip() +
  xlab("Share of Chord Changes") +
  ylab("Chord Changes") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# Finding the most common artists
# Finding 30 artists with the most songs in the corpus
bb_30_artists <- bb %>% select(artist, title) %>% unique() %>% count(artist, sort = T)
#.... YOUR CODE FOR TASK 6 ....

# Displaying 30 artists with the most songs in the corpus
head(bb_30_artists, 30)

# Dataframe of artists by instrument
tags <- tibble(
  artist = c('Abba', 'Billy Joel', 'Elton John', 'Stevie Wonder', 'The Rolling Stones', 'The Beatles', 'Eric Clapton'),
  instrument = c('piano', 'piano', 'piano', 'piano', 'guitar', 'guitar', 'guitar'))

# Creating a new dataframe bb_tagged that includes a new column instrument from tags
bb_tagged <- bb %>% inner_join(tags, by = "artist")
# .... YOUR CODE FOR TASK 7 ....

# Displaying the new data frame
bb_tagged 

# The top 20 most common chords
top_20 <- bb_count$chord[1:20]

# Comparing the frequency of the 20 most common chords in piano- and guitar-driven songs
bb_tagged %>%
  filter(chord %in% top_20) %>%
  count(chord, instrument, sort = T) %>%
  ggplot(aes(x = chord, y = n, fill = chord)) +
  geom_col() +
  facet_grid( ~ instrument) +
  coord_flip() +
  xlab("Chords") +
  ylab("Frequency of Chords") +
  theme(legend.position = "none") + ggtitle("Chords by Instrument")

bb_tagged %>%
  filter(chord %in% top_20) %>%
  count(chord, artist, sort = T) %>%
  ggplot(aes(x = chord, y = n, fill = chord)) +
  geom_col() +
  facet_grid( ~ artist) +
  coord_flip() +
  xlab("Chords") +
  ylab("Frequency of Chords") +
  theme(legend.position = "none") + ggtitle("Chords by Artist")


# The top 20 most common bigrams
top_20_bigram <- bb_bigram_count$bigram[1:20]

# Creating a faceted plot comparing guitar- and piano-driven songs for bigram frequency
bb_tagged %>% mutate(next_chord = lead(chord, 1), next_title = lead(title, 1), 
                     bigram = str_c(chord, next_chord, sep = " ")) %>% 
  filter(title == next_title) %>% count(bigram, instrument, sort = T) %>%
  filter(bigram %in% top_20_bigram) %>%
  ggplot(aes(x = bigram, y = n, fill = bigram))+
  geom_col()+
  coord_flip() +
  xlab("Bigram") +
  ylab("Instruments") +
  facet_grid(~instrument) +
  theme(legend.position = "none")
# .... MODIFIED CODE FROM TASK 4 .... 
# .... MODIFIED CODE FROM TASK 8 ....


# Displaying the first 20 rows of bb_bigram_count
head(bb_bigram_count, 20)





