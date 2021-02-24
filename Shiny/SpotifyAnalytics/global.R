library(httr)
library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(shinymaterial)
library(tibble)
library(highcharter)
library(RColorBrewer)
library(shinycssloaders)
library(htmltools)
library(lubridate)
library(lazyeval)
library(spotifyr)
library(gt)
library(RCurl)
library(jsonlite)

rm(list = ls())

Sys.setenv(SPOTIFY_CLIENT_ID = "383cfac3d8434244a38c4e279a04ce47")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "486c6f539f8e45a2b82f03a30cfed26f")

source("helpers.R")

jscode <-
  '$(document).on("shiny:connected", function(e) {
  var jsWidth = screen.width;
  Shiny.onInputChange("GetScreenWidth",jsWidth);
});
'
base_url <- "https://api.spotify.com/v1/"

neon_colors <- c(
  "#84DE02",
  "#FF4466",
  "#4BC7CF",
  "#FF85CF",
  "#FFDF46",
  "#391285",
  "#E88E5A",
  "#DDE26A",
  "#C53151",
  "#B05C52",
  "#FD5240",
  "#FF4681",
  "#FF6D3A",
  "#FF404C",
  "#A0E6FF"
)

pca_vars <- c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo", "duration_ms")

# Determine if local or deployed for redirect uri
# testing url
options(shiny.port = 8100)
REDIRECT_URI <- "http://127.0.0.1:8100"
# deployed URL
# redirect_uri <- "https://dungates.shinyapps.io/SpotifyAnalytics/"


# Personal spotify info

CLIENT_ID <- Sys.getenv("SPOTIFY_CLIENT_ID")
CLIENT_SECRET <- Sys.getenv("SPOTIFY_CLIENT_SECRET")

# Redirect URL creation

ShinyGetTokenURL <- function() {
  url <- paste0(
    "https://accounts.spotify.com/authorize?", #
    "client_id=", CLIENT_ID, "&",
    "response_type=code&",
    "redirect_uri=", REDIRECT_URI
  )
  return(url)
}

## gets the token from Google once you have the code that is in the return URL
ShinyGetToken <- function(code) {

  # cat(paste("this is the code:", code))

  token <- MErga.authenticate(auth_code = code)
  # cat(paste("this is the token:", token))
  return(token)
}

## posts your code to spotify to get the current refresh
MErga.authenticate <- function(auth_code) {
  opts <- list(verbose = FALSE)
  # cat(paste("this is the auth_code", auth_code))
  # cat(paste("this is the client_id:"), CLIENT_ID)
  # cat(paste("this is the client_secret:"), CLIENT_SECRET)
  # cat(paste("this is the redirect_uri:"), REDIRECT_URI)
  raw.data <- postForm("https://accounts.spotify.com/api/token",
    .opts = opts,
    code = auth_code,
    client_id = CLIENT_ID,
    client_secret = CLIENT_SECRET,
    redirect_uri = REDIRECT_URI,
    grant_type = "authorization_code",
    style = "POST"
  ) # Uses RCURL to get submit html form

  token.data <- fromJSON(raw.data)
  now <- as.numeric(Sys.time())
  # cat(paste("add", token.data))
  token <- c(token.data, timestamp = c("first" = now, "refresh" = now))
  # cat(paste("this is the token:", token, "\n"))

  return(token)
}

# Get color palette of image

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
