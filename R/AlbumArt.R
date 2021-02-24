library(imager)
library(magick)
library(scales)
library(tidyverse)
library(spotifyr)
library(ggimage)
library(here)
library(TSP)

Sys.setenv(SPOTIFY_CLIENT_ID = "383cfac3d8434244a38c4e279a04ce47")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "486c6f539f8e45a2b82f03a30cfed26f")

## Function to get n number of colours out of your image. (optionally you can specify different colour space)
get_colorPal <- function(im, n = 1, cs = "RGB") {
  # print(cs)
  tmp <- im %>%
    image_resize("100") %>%
    image_quantize(max = n, colorspace = cs) %>% ## reducing colours! different colorspace gives you different result
    magick2cimg() %>% ## I'm converting, becauase I want to use as.data.frame function in imager package.
    RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
    as.data.frame(wide = "c") %>% # 3 making it wide makes it easier to output hex colour
    mutate(
      hex = hsv(rescale(c.1, from = c(0, 360)), c.2, c.3),
      hue = c.1,
      sat = c.2,
      value = c.3
    ) %>%
    count(hex, hue, sat, value, sort = T) %>%
    mutate(colorspace = cs)

  return(tmp %>% select(colorspace, hex, hue, sat, value, n)) ## I want data frame as a result.
}

# Organize dataframe for variables needed
top50_albumart <- get_my_top_artists_or_tracks(type = "tracks", limit = 49, time_range = "short_term") %>%
  relocate(name, .before = artists) %>%
  dplyr::rename(track.name = name) %>%
  relocate(album.name, .after = track.name) %>%
  unnest(cols = artists, .id = "name") %>%
  dplyr::rename(artist.name = name) %>%
  dplyr::relocate(artist.name, .after = track.name) %>%
  dplyr::group_by(track.name) %>%
  dplyr::mutate(artists = list(artist.name)) %>%
  dplyr::ungroup() %>%
  select(-artist.name) %>%
  relocate(c("artists", "duration_ms", "explicit", "popularity", "album.release_date", "album.images"),
    .after = album.name
  ) %>%
  distinct(track.name, .keep_all = T) %>%
  select("track.name", "album.name", "artists", "album.images") %>%
  unnest(cols = album.images) %>%
  dplyr::filter(width == 300) %>%
  select(url, album.name, artists, track.name)

# Extract album colors
top50_albumart_colors <- top50_albumart %>%
  dplyr::mutate(colors = map(url, ~ get_colorPal(image_read(.)) %>% pull(hex))) %>%
  dplyr::mutate(value = map(url, ~ get_colorPal(image_read(.)) %>% pull(hue))) %>%
  dplyr::mutate(colors = as.character(colors))

# Make album locations
top50_albumart_colors_xy <- top50_albumart_colors %>%
  mutate(value = as.numeric(value)) %>%
  arrange(desc(value)) %>%
  mutate(
    x = c(rep(1, 7), rep(2, 7), rep(3, 7), rep(4, 7), rep(5, 7), rep(6, 7), rep(7, 7)),
    y = c(rep(1:7, 7))#,
    # x2 = 1,
    # y2 = row_number()
  )
rgb <- col2rgb(top50_albumart_colors_xy$colors)
colors_hsv <- t(rgb2hsv(rgb)) %>%
  as.data.frame()
lab <- convertColor(t(rgb), 'sRGB', 'Lab')

ordered_albums <- top50_albumart_colors_xy %>%
  bind_cols(select(as_tibble(lab), L), select(colors_hsv, h)) %>%
  arrange(desc(h)) # Can do hue, hsv, or value here

# Test plot
# ggplot(ordered_albums, aes(x = x, y = y, color = colors)) +
#   geom_point(size = 5) +
#   scale_color_manual(values = c(ordered_albums$colors)) +
#   ggtitle("Albums Organized by Color") +
#   theme_void() +
#   theme(legend.position = "none",
#         plot.title = element_text(hjust = 0.5))

# Plot data
ggplot(ordered_albums, aes(x = x, y = y)) +
  geom_image(aes(image = url), size = 0.14) +
  scale_size_identity() +
  theme_void() +
  coord_cartesian(clip = "off") +
  scale_x_discrete(breaks = c(1,2,3,4,5)) +
  scale_y_discrete()
  # scale_x_discrete(breaks = c(1,2,3,4,5), expand = c(0.1, 2.6)) +
  # theme_void() #+
  # theme(plot.margin = margin(r = -11, l = -11, unit = "cm"))

ggsave(here("Images/short_term_albums.png"))
knitr::plot_crop(here("Images/short_term_albums.png")) # Crops out whitespace
