#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    ## Function to get n number of colours out of your image. (optionally you can specify different colour space)
    get_colorPal <- function(im, n=1, cs="RGB"){
        #print(cs) 
        tmp <-im %>% image_resize("100") %>% 
            image_quantize(max=n, colorspace=cs) %>%  ## reducing colours! different colorspace gives you different result
            magick2cimg() %>%  ## I'm converting, becauase I want to use as.data.frame function in imager package.
            RGBtoHSV() %>% ## i like sorting colour by hue rather than RGB (red green blue)
            as.data.frame(wide="c") %>%  #3 making it wide makes it easier to output hex colour
            mutate(hex=hsv(rescale(c.1, from=c(0,360)),c.2,c.3),
                   hue = c.1,
                   sat = c.2,
                   value = c.3) %>%
            count(hex, hue, sat,value, sort=T) %>% 
            mutate(colorspace = cs)
        
        return(tmp %>% select(colorspace,hex,hue,sat,value,n)) ## I want data frame as a result.
        
    }
    
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    output$value_client <- renderText({ input$spotify_client })
    output$value_secret <- renderText({ input$spotify_secret })
    
    # Sys.setenv(SPOTIFY_CLIENT_ID = input$spotify_client)
    # Sys.setenv(SPOTIFY_CLIENT_SECRET = input$spotify_secret)
    
    top50tracks_gt <- top50tracks %>%
        select(1:8) %>%
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
    
    output$table <- render_gt(
        top50tracks_gt %>%
        gt() %>%
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
            align = "auto",
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
            style = cell_fill(color = colorsss[1], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[1]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[2], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[2]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[3], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[3]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[4], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[4]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[5], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[5]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[6], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[6]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[7], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[7]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[8], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[8]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[9], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[9]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[10], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[10]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[11], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[11]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[12], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[12]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[13], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[13]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[14], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[14]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[15], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[15]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[16], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[16]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[17], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[17]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[18], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[18]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[19], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[19]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[20], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[20]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[21], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[21]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[22], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[22]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[23], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[23]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[24], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[24]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[25], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[25]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[26], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[26]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[27], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[27]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[28], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[28]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[29], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[29]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[30], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[30]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[31], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[31]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[32], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[32]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[33], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[33]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[34], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[34]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[35], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[35]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[36], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[36]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[37], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[37]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[38], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[38]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[39], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[39]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[40], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[40]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[41], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[41]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[42], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[42]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[43], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[43]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[44], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[44]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[45], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[45]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[46], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[46]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[47], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[47]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[48], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[48]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[49], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[49]
            )
        ) %>%
        tab_style(
            style = cell_fill(color = colorsss[50], alpha = 0.8),
            locations = cells_body(
                rows = colorss == colorsss[50]
            )
        )
    )

})
