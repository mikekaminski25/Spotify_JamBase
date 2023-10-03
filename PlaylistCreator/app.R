#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
if ("package:spotifyr" %in% search()) {
  detach("package:spotifyr", unload = TRUE)
}

app_df <- read_csv("PlaylistCreator/app_df.csv") %>% select(-1)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range", "Select Date Range:", start = Sys.Date(), end = Sys.Date() %m+% months(1)),
      selectInput("venue_filter", "Select Venue:", choices = c("All", unique(app_df$venue)), multiple = TRUE),
      
      # Add an action button to trigger dataframe generation
      actionButton("generate_button", "Generate Playlist")
    ),
    mainPanel(
      dataTableOutput("date_artist_table"),
      dataTableOutput("artist_song_table"),  # Add this line for the new table
      # Your other UI elements can go here
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    if ("All" %in% input$venue_filter) {
      upcoming_shows_filtered <- subset(app_df, show_date >= input$date_range[1] & show_date <= input$date_range[2])
    } else {
      upcoming_shows_filtered <- subset(app_df, show_date >= input$date_range[1] & show_date <= input$date_range[2] & venue %in% input$venue_filter)
    }
    return(upcoming_shows_filtered)
  })
  
  selected_data <- reactive({
    data <- filtered_data()
    data %>%
      distinct(artist, show_date)
  })
  
  output$date_artist_table <- renderDataTable({
    selected_data()
  })
  
  selected_artist_data <- reactive({
    # Filter app_df based on the selected artists and date range
    data <- filtered_data()
    selected_artists <- selected_data()$artist
    data[data$artist %in% selected_artists, c("artist", "song", "track_uri")]
  })
  
  output$artist_song_table <- renderDataTable({
    selected_artist_data()
  })
  
  observeEvent(input$generate_button, {
    # Get user-selected values
    date_range1 <- input$date_range[1]
    date_range2 <- input$date_range[2]
    venue_name <- input$venue_filter
    
    # Use selected_artist_data() for filtering
    playlist_songs <- selected_artist_data()
    
    # Authentication and playlist creation logic
    Sys.setenv(SPOTIFY_CLIENT_ID = '9b5eb134223f4cb5a0c4834fa5742d9e')
    Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b742be8a81ba469794bd3146dac1d080')
    
    access_token <- tinyspotifyr::get_spotify_access_token()
    
    # playlist_name <- paste(paste(venue_name, ":", sep = ""),
    #                        format(as.Date(date_range1, format = "%d/%m/%Y"), "%b %d"),
    #                        "-",
    #                        format(as.Date(date_range2, format = "%d/%m/%Y"), "%b %d"),
    #                        sep = " ")
    playlist_name <- paste('Next30:', venue_name, sep = " ")
                          
    
    # Check if the playlist exists
    my_playlists <- tinyspotifyr::get_my_playlists(limit = 50)
    playlist_logical <- (my_playlists$name == playlist_name)
    
    # Find or create the playlist
    if (sum(playlist_logical) > 0) {
      ind <- which(playlist_logical)
      dr <- my_playlists[ind, ]
    } else {
      dr <- create_playlist('mikekaminski25', playlist_name, public = TRUE,
                            description = paste('Top 5 songs on Spotify for bands playing at',
                                                venue_name,
                                                'between',
                                                format(as.Date(date_range1, format = "%d/%m/%Y"), "%b %d"),
                                                'and',
                                                format(as.Date(date_range2, format = "%d/%m/%Y"), "%b %d"), sep = ' '))
    }
    
    playlist_uri <- playlist_songs$track_uri
    
    batch_size <- 100
    for (i in seq(1, length(playlist_uri), by = batch_size)) {
      batch <- playlist_uri[i:min(i + batch_size - 1, length(playlist_uri))]
      add_tracks_to_playlist(dr$id, batch)
    }
    
    # You can add a confirmation message here or any other desired behavior
  })
}

shinyApp(ui = ui, server = server)
