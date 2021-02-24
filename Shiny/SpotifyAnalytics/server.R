#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinymaterial)
library(shinyWidgets)
library(shinyjs)
library(rintrojs)
library(tidyverse)
library(highcharter)
library(png)
library(DT)
library(visNetwork)
library(gt)
library(spotifyr)
library(imager)
library(magick)
library(scales)
library(lubridate)



shinyServer(function(input, output, session) {
    
    ### MY ACCESS TOKEN
    spotify_access_token <- reactive({
        get_spotify_access_token()
    })
    
    # Checks for presence of code in URL
    AuthCode <- reactive({
        ## gets all the parameters in the URL. Your authentication code should be one of them
        pars <- parseQueryString(session$clientData$url_search)
        if(length(pars$code) > 0) {
            return(pars$code)
        }
    })
    
    # Creates a token once a code is available
    AccessToken <- reactive({ 
        shiny::validate(
            need(AuthCode(), "Authenticate To See")
        ) # authenticate necessitate
        auth_code <- AuthCode()
        access_token <- ShinyGetToken(code = auth_code)
        token <- access_token$access_token
        token
    })
    
    # output$text <- renderText({
    #     paste(AccessToken())
    # })
    
    # Button for spotify authorization
    observeEvent(req(input$spotify_authorize), {
        # Do something on click, probably link
        url <- ShinyGetTokenURL()
            # "https://accounts.spotify.com/authorize?client_id=383cfac3d8434244a38c4e279a04ce47&response_type=code&redirect_uri=http://127.0.0.1:8100"
        runjs(paste0("window.location.href='", url, "'")) # Opens new window
    })
    
    observeEvent(input$spotify_authorize, {
        # Hide button
        # shinyjs::hide("spotify_authorize")
        if(exists(AccessToken() == T)){
            # Hide box
            shinyjs::hide("personal_box")
        }
    })
    
    # spotify_authorization_code <- reactive(input$spotify_authorize {
    #     if(exists(AccessToken() == T)) {
    #         get_spotify_authorization_code(client_id = "", client_secret = "")
    #     }
    # })
    
    ### TOP 50 ARTISTS
    # top50artists <- get_my_top_artists_or_tracks(type = "artists", limit = 50, authorization = AccessToken()) %>% 
    #     relocate(name, .after = genres) %>% 
    #     relocate(followers.total, .after = name) %>%
    #     relocate(c("popularity", "images"), .after = followers.total)
    
    ### TABLE OF MOST RECENT FAVORITES CODE
    top50tracks_gt <- eventReactive(exists(AccessToken()), {
        # cat("is this working")
        cat(paste(AuthCode(), "\n"))
        cat(AccessToken())
        
        ### TOP 50 TRACKS
        top50tracks_gt <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, authorization = AuthCode()) %>%
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
        relocate(c("artists", "duration_ms","explicit", "popularity", "album.release_date", "album.images"),
                 .after = album.name) %>%
        distinct(track.name, .keep_all = T) %>%# Join images from artists to here for tables?
        dplyr::select(1:8) %>%
        unnest(cols = album.images) %>%
        dplyr::filter(width == 64) %>% # Could also filter this prior to unnesting, note that height and width is 64 using this, useful for imageR
        select(-width, -height) %>%
        relocate(c("url", "album.name"), .after = artists) %>%
        mutate(Rank = row_number()) %>%
        relocate(Rank, .before = track.name) %>%
        dplyr::mutate(duration_ms = case_when(duration_ms < 60000 ~
                                                  lubridate::seconds(seconds_to_period((`duration_ms`/(1000)))),
                                              duration_ms >= 60000 & duration_ms < 3600000 ~
                                                  ms(seconds_to_period((`duration_ms`/(1000)))),
                                              duration_ms >= 3600000 ~
                                                  hms(seconds_to_period((`duration_ms`/(1000)))))) %>%
        dplyr::mutate(duration_ms = str_replace(duration_ms, "M ", ":"),
                      duration_ms = str_remove(duration_ms, "S")) %>%
        separate(duration_ms, sep = ":", into = c("A", "B")) %>%
        mutate(B = as.numeric(B),
               B = floor(B),
               B = as.character(B),
               B = ifelse(str_length(B) == 1,
                          paste0(0, B),
                          B)) %>%
        unite("duration_ms", A:B, sep = ":") %>%
        dplyr::mutate(colors = map(url, ~ dput(get_colorPal(image_read(.)) %>% pull(hex)))) %>%
        dplyr::mutate(colorss = as.character(colors)) %>%
            gt()
        top50tracks_gt
        # colorsss <- as_vector(top50tracks_gt$colorss) # List of colors
        # cat(colorsss[1])
    })
    
    top50colors <- eventReactive(exists(AccessToken()), {
        top50tracks_gt <- get_my_top_artists_or_tracks(type = "tracks", limit = 50, authorization = get_spotify_authorization_code()) %>%
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
            relocate(c("artists", "duration_ms","explicit", "popularity", "album.release_date", "album.images"),
                     .after = album.name) %>%
            distinct(track.name, .keep_all = T) %>%# Join images from artists to here for tables?
            dplyr::select(1:8) %>%
            unnest(cols = album.images) %>%
            dplyr::filter(width == 64) %>% # Could also filter this prior to unnesting, note that height and width is 64 using this, useful for imageR
            select(-width, -height) %>%
            relocate(c("url", "album.name"), .after = artists) %>%
            mutate(Rank = row_number()) %>%
            relocate(Rank, .before = track.name) %>%
            dplyr::mutate(duration_ms = case_when(duration_ms < 60000 ~
                                                      lubridate::seconds(seconds_to_period((`duration_ms`/(1000)))),
                                                  duration_ms >= 60000 & duration_ms < 3600000 ~
                                                      ms(seconds_to_period((`duration_ms`/(1000)))),
                                                  duration_ms >= 3600000 ~
                                                      hms(seconds_to_period((`duration_ms`/(1000)))))) %>%
            dplyr::mutate(duration_ms = str_replace(duration_ms, "M ", ":"),
                          duration_ms = str_remove(duration_ms, "S")) %>%
            separate(duration_ms, sep = ":", into = c("A", "B")) %>%
            mutate(B = as.numeric(B),
                   B = floor(B),
                   B = as.character(B),
                   B = ifelse(str_length(B) == 1,
                              paste0(0, B),
                              B)) %>%
            unite("duration_ms", A:B, sep = ":") %>%
            dplyr::mutate(colors = map(url, ~ dput(get_colorPal(image_read(.)) %>% pull(hex)))) %>%
            dplyr::mutate(colorss = as.character(colors))
        colorsss <- as_vector(top50tracks_gt$colorss) # List of colors
        colorsss
    })
    
    output$table <- render_gt(
        top50tracks_gt() %>%
        tab_header(
            title = "Most Played Spotify Tracks",
            subtitle = "Top 50 Most Recent Shown"
        ) %>%
        cols_label(
            track.name = "Track",
            artists = "Artist",
            url = "",
            album.name = "Album",
            duration_ms = "Duration",
            explicit = "Explicit",
            popularity = "Popularity (0-100)",
            album.release_date = "Album Release Date"
        ) %>%
        fmt(
            columns = vars(explicit),
            fns = function(x) {
                str_to_title(x)
            }
        ) %>%
        cols_hide(
            columns = vars(colorss, colors)
        ) %>%
        cols_align(
            align = "auto"
        ) %>%
        text_transform(
            locations = cells_body(vars(url)),
            fn = function(x) {
                web_image(
                    url = x,
                    height = as.numeric(x)
                )
            }
        ) %>%
        tab_options(
            heading.background.color = "#1D5C95",
            column_labels.background.color = "#4486B5"
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[1], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[1]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[2], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[2]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[3], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[3]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[4], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[4]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[5], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[5]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[6], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[6]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[7], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[7]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[8], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[8]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[9], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[9]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[10], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[10]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[11], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[11]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[12], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[12]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[13], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[13]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[14], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[14]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[15], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[15]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[16], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[16]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[17], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[17]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[18], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[18]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[19], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[19]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[20], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[20]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[21], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[21]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[22], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[22]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[23], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[23]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[24], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[24]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[25], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[25]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[26], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[26]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[27], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[27]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[28], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[28]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[29], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[29]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[30], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[30]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[31], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[31]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[32], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[32]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[33], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[33]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[34], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[34]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[35], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[35]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[36], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[36]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[37], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[37]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[38], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[38]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[39], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[39]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[40], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[40]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[41], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[41]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[42], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[42]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[43], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[43]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[44], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[44]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[45], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[45]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[46], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[46]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[47], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[47]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[48], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[48]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[49], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[49]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = top50colors()[50], alpha = 0.8),
            locations = cells_body(
                rows = colorss == top50colors()[50]
            )
        )
    )
    
    
    ###### ARTIST CODE
    artist_info <- reactive({
        req(input$artist_search != '')
        search_spotify(input$artist_search, 'artist', authorization = spotify_access_token()) %>% 
            filter(!duplicated(name))
    })
    
    observeEvent(input$artist_search, {
        choices <- artist_info()$name
        names(choices) <- choices
        update_material_dropdown(session, 'select_artist', value = artist_info()$name[1], choices = choices)
    })
    
    output$select_artist_ui <- renderUI({
        req(nrow(artist_info()) > 0)
        tagList(
            htmlOutput('artist_img'),
            withBusyIndicatorUI(
                actionButton('tracks_go', 'Generate plot', class = 'btn-primary')
            ),
            material_switch('artist_autoplay', 'Play song preview on hover', color = '#1ed760'),
            br(),
            uiOutput('artist_chart_song_ui')
        )
    })
    
    selected_artist <- reactive({
        req(nrow(artist_info()) > 0)
        artist_info() %>% 
            filter(name == input$select_artist) %>% 
            filter(popularity == max(popularity))
    })
    
    observeEvent(input$select_artist, {
        
        req(nrow(artist_info()) > 0)
        
        artist_img <- ifelse(!is.na(selected_artist()$images[[1]]$url[1]), selected_artist()$images[[1]]$url[1], 'https://pbs.twimg.com/profile_images/509949472139669504/IQSh7By1_400x400.jpeg')
        
        output$artist_img <- renderText({
            HTML(str_glue('<img src={artist_img} height="200">'))
        })
        
    })
    
    artist_audio_features <- eventReactive(input$tracks_go, {
        df <- get_artist_audio_features(selected_artist()$name, authorization = spotify_access_token()) %>% 
            mutate(album_img = map_chr(1:nrow(.), function(row) {
                .$album_images[[row]]$url[1]
            }))
        if (nrow(df) == 0) {
            stop("Sorry, couldn't find any tracks for that artist's albums on Spotify.")
        }
        return(df)
    })
    
    output$artist_quadrant_chart <- renderHighchart({
        artist_quadrant_chart(artist_audio_features()) %>% 
            hc_add_event_point(event = 'mouseOver')
    })
    
    output$artist_chart_song_ui <- renderUI({
        
        req(input$artist_quadrant_chart_mouseOver, input$artist_autoplay == TRUE)
        
        artist_track_hover <- input$artist_quadrant_chart_mouseOver
        track_preview_url <- artist_audio_features() %>% filter(
            album_name == artist_track_hover$series,
            valence == artist_track_hover$x,
            energy == artist_track_hover$y
        ) %>% pull(track_preview_url)
        
        if (!is.na(track_preview_url)) {
            tagList(
                tags$audio(id = 'song_preview', src = track_preview_url, type = 'audio/mp3', autoplay = NA, controls = NA),
                tags$script(JS("myAudio=document.getElementById('song_preview'); myAudio.play();"))
            )
        } else {
            h5('No preview for this track on Spotify')
        }
    })
    
    observeEvent(input$tracks_go, {
        output$artist_plot <- renderUI({
            if (input$GetScreenWidth >= 800) {
                withSpinner(highchartOutput('artist_quadrant_chart', width = '820px', height = '800px'), type = 5, color = '#1ed760')
            } else {
                withSpinner(highchartOutput('artist_quadrant_chart'), type = 5, color = '#1ed760')
            }
        })
    })

})
