#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# https://dungates.shinyapps.io/SpotifyAnalytics/

source("carouselPanel.R")

# Panel div for visualization
# override the currently broken definition in shinyLP version 1.1.0
panel_div <- function(class_type, content) {
    div(class = sprintf("panel panel-%s", class_type),
        div(class = "panel-body", content)
    )
}

shinyUI(navbarPage(title = img(src="SpotifyWrapped.png", height = "40px"), id = "navBar",
                   theme = "paper.css",
                   collapsible = TRUE,
                   inverse = TRUE,
                   windowTitle = "Spotify Data Analytics",
                   position = "fixed-top",
                   footer = includeHTML("./www/include_footer.html"),
                   header = tags$style(
                       ".navbar-right {
                       float: right !important;
                       }",
                       "body {padding-top: 75px;}"),
                   
                   tabPanel("HOME", value = "home",
                            
                            shinyjs::useShinyjs(),
                            
                            tags$head(tags$script(HTML('
                                                       var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                                                       '))),
                            fluidRow(
                                HTML("
                                     
                                     <section class='banner'>
                                     <h2 class='parallax'>Spotify Data Upgraded</h2>
                                     <p class='parallax_description'>A tool for analysis, art, discovery, and more</p>
                                     </section>
                                     ")
                            ),
                            
                            # tags$head(includeScript("google-analytics.js")),
                            
                            # A header level row for the title of the app (if needed)  
                            # fluidRow(
                            #     shiny::HTML("<br><br><center> <h1></h1> </center>
                            #                 <br>
                            #                 <br>"),
                            #     style = "height:250px;"),
                            
                            # fluidRow(
                            #     style = "height:250px; padding: 125px 0px;",
                            #     shiny::HTML("<center> <h1>Welcome to the Career PathFinder</h2></center>"),
                            #     shiny::HTML("<center> <h5><i>Like stops on a map, a career path pinpoints your next job, 
                            #                 the job after that, and beyond.</i></h5> </center>")
                            # ),
                            
                            # fluidRow(
                            #     
                            #     style = "height:25px;"),
                            
                            # fluidRow(
                            #     column(2),
                            #     
                            #     column(3,
                            #            div(style="display: inline-block;padding: 100px 0px;",
                            #                HTML("<h3>What <span style='font-weight:bold'>career planning</span> questions are you looking to answer?</h3>")
                            #            )
                            #     ),
                            #     
                            #     column(5,
                            #            
                            #            carouselPanel(
                            #                # tags$a(href = "#FAQ", 
                            #                #        tags$img(src = "screen_capture_absenteeism_2.jpg", width = "615px")), # experiment diff size img - fixed height 1080px and width 1900px
                            #                tags$img(src = "original1.svg", class = "img-responsive center-block"),
                            #                tags$img(src = "original2.svg", class = "img-responsive center-block"),
                            #                tags$img(src = "original3.svg", class = "img-responsive center-block"),
                            #                tags$img(src = "original4.svg", class = "img-responsive center-block"),
                            #                tags$img(src = "original5.svg", class = "img-responsive center-block")
                            #                # tags$a(href = "https://geom.shinyapps.io/word", tags$img(src = "screen_capture_word_2.jpg", width = "615px"))
                            #                
                            #            )
                            #     )
                            # ),
                            # 
                            # fluidRow(
                            #     
                            #     style = "height:50px;"),
                            # 
                            # fluidRow(
                            #     style = "height:250px;",
                            #     shiny::HTML("<center> <h4><i>Are you looking to plan a career with the County?</i></h4> </center>"),
                            #     shiny::HTML("<center> <h4><i>Are you curious about the paths real County employees have taken?</i></h4></center>"),
                            #     shiny::HTML("<center> <h4><i>Then you're in the right place.</i></h4></center>")
                            # ),
                            # 
                            # # PAGE BREAK
                            # tags$hr(),
                            
                            # WHAT
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Tools Available on this Site</h1> </center><br>"),
                                       shiny::HTML("<h5>Several different ways to interact with Spotify data, either
                                                   proprietary or for a specific album/artist of interest. Each of the listed header 
                                                   tabs allows analysis and download of different kinds of data from the Spotify API.
                                                   Additionally all images generated can be downloaded for future reference.
                                                   </h5>")
                                ),
                                column(3)
                            ),
                            
                            # HOW TO START
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Personal Data</h1> </center><br>"),
                                       shiny::HTML("<h5>In this section you can take a deeper look at the music
                                                   you have most recently listened to, and see Spotify's own metrics on what it
                                                   thinks of the music you listen to, and its scores in relevant areas.</h5>")
                                ),
                                column(3)
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Artist Data</h1> </center><br>"),
                                       shiny::HTML("<h5>In this section you can investigate your personal favorite artists
                                                   based on Spotify's metrics, and use my own algorithms to get recommendations based on
                                                   artists. There is a lot of genre based grouping that Spotify does somewhat arbitrarily it appears
                                                   and I filter that out and do algorithmic groupings and k means clustering
                                                   based on lyricism and sounds.</h5>")
                                ),
                                column(3)
                                
                            ),
                            
                            fluidRow(
                                
                                style = "height:50px;"),
                            
                            # PAGE BREAK
                            tags$hr(),
                            
                            fluidRow(
                                column(3),
                                column(6,
                                       shiny::HTML("<br><br><center> <h1>Album Data</h1> </center><br>"),
                                       shiny::HTML("<h5>In this section you can explore specific albums in great detail, once
                                                   again using Spotify's metrics, as well as my own personal ones generated for 
                                                   getting an idea of what really makes you like the art being made. There is also a tool for
                                                   manipulating album covers that is pretty fun.</h5>")
                                ),
                                column(3)
                            ),
                            fluidRow(style = "height:25px;"
                            )
                            
                   ), # Closes the first tabPanel called "Home"
                   
                   tabPanel("PERSONAL DATA", value = "personal_data", useShinyjs(),
                            fluidRow(
                            shinydashboard::box(id = "personal_box",
                                                width = 12, 
                                                title = HTML("<h1><center><font size=14>Click to Allow Spotify Authorization</font></center></h1>"),
                                                tags$br(),
                                                column(6, align="center", offset = 3,
                                                  actionButton(
                                                    inputId = "spotify_authorize",
                                                    label = "Authorize",
                                                    # color = "#1ed760", # spotify color
                                                    style = "color: #fff; background-color: #1ed760; border-color: #2e6da4"
                                                    # color = "primary",
                                                    # style = "fill",
                                                    # size = "md" # medium size
                                                    # onclick = "window.open('https://accounts.spotify.com/authorize', '_blank')"
                                                  ),
                                                  tags$style(type='text/css', "#button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                                                ),
                                                tags$br()
                                                # textOutput("text")
                                  )),
                            
                            gt_output(outputId = "table")
                            
                            # conditionalPanel(condition = "exists(top50tracks_gt) == T",
                            #                  gt_output(outputId = "table"))
                            
                            
                            ),  # Closes the second tabPanel called "PERSONAL DATA"
                   
                   ## ARTIST DATA STUFF
                   
                   tabPanel("ARTIST", value = "artist_data",
                            material_page(title = "",
                                          background_color = '#828282',
                                          tags$head(tags$link(rel = 'icon', type = 'image/png', href = 'green_music_note.png'),
                                                    tags$title('Sentify')),
                                          useShinyjs(),
                                          tags$script(jscode),
                                          tags$style(appCSS),
                                          tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
                                                    tags$head(includeScript('www/ga.js')),
                                                    tags$head(includeScript('www/hotjar.js'))),
                                          material_row(
                                            material_column(
                                              align = 'center',
                                              width = 3,
                                              material_card(
                                                title = HTML("<h2>Generate a Plot Based on an Artist</h2>"),
                                                depth = 4,
                                                material_text_box('artist_search', "Enter an artist's name", color = 'black'),
                                                  conditionalPanel("input.artist_search !== ''", 
                                                                    material_dropdown('select_artist', 
                                                                                      'Choose an artist from these matches on Spotify', 
                                                                                      '', 
                                                                                      color = '#1ed760')),
                                                uiOutput('select_artist_ui')
                                              )
                                            ),
                                            material_column(
                                              width = 9,
                                              uiOutput('artist_plot')
                                            )
                                          )
                            )
                          ),  # Closes About tab
                   
                   tabPanel("ALBUM", value = "album_data")
                   
)

)