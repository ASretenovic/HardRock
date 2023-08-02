# libraries loading -----------------------------------------------------------

# install.packages("rvest")
library(rvest)

#install.packages("magrittr")
library(magrittr)

# install.packages("stringr")
library(stringr)

# install.packages("dplyr")
library(dplyr)





#########################################################################################
#########################################################################################
#                                   AC/DC
########################################################################################
#######################################################################################





########################################################################################
# loading initial data about Song titles, Albums and Writers and Years
########################################################################################

# define data source url (Wikipedia)
url <- "https://en.wikipedia.org/wiki/List_of_songs_recorded_by_AC/DC"

# load html page 
page <- read_html(url)

# load all the tables from the page
tables <- page %>% html_nodes("table.wikitable.sortable")

# select first table and convert it to data frame
table <- tables[[1]]
df <-  html_table(table, fill = TRUE)  
str(df)

# remove last column
df$Refs. <- NULL

# change column names
colnames(df) <- c("Song_Title", "Writers", "Album_Name", "Year_of_First_Release")

# remove "" from Song_Title column
df$Song_Title <-  gsub("\"", "", df$Song_Title)

# add column Single
df$Single <- "No"
str(df)

# find rows that contain † value in the column Song_Title and mark that rows as Singles
rows_with_plus <- grep("†", df$Song_Title)
df$Single[rows_with_plus] <- "Yes"
df$Single <- as.factor(df$Single)

# remove † value from the Song_Title column
df$Song_Title <-  gsub("†", "", df$Song_Title)

df$Song_Title <- sub("\\[.*","",df$Song_Title)

str(df)

# Format Writers column so that names are separated with ","
df$Writers <- sapply(df$Writers, function(x){gsub("(?<=[a-z])(?=[A-Z])", ", ", x, perl = TRUE)})


# Cleaning and standardizing the album names --------------------

length(unique(df$Album_Name))
unique(df$Album_Name)

# Album "If You Want Blood You've Got It"
rows_album <- grepl("If You Want Blood You've Got It", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "If You Want Blood You've Got It"

# Albym "High Voltage"
rows_album <- grepl("voltage", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "High Voltage"

# album "T.N.T."
rows_album <- grepl("T.N.T.", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "T.N.T."

# album "Let There Be Rock"
rows_album <- grepl("Let There Be Rock", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Let There Be Rock"

# album "Powerage"
rows_album <- grepl("Powerage", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Powerage"

# Album "Higway to Hell"
rows_album <- grepl("Highway to Hell", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "HighWay to Hell"

# album "Back in Black"
rows_album <- grepl("Back in Black", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Back in Black"

# Album "For Those About to Rock We Salute You" 
rows_album <- grepl("For Those About to Rock We Salute You", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "For Those About to Rock We Salute You"

# Album "Flick of the Switch
rows_album <- grepl("Flick of the Switch", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Flick of the Switch"

# Album "Dirty Deeds Done Dirt Cheap"
rows_album <- grepl("Dirty Deeds Done Dirt Cheap", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Dirty Deeds Done Dirt Cheap"

# album "Fly on the Wall"
rows_album <- grepl("Fly on the Wall", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Fly on the Wall"

# Album "Live at River Plate"
rows_album <- grepl("river plate", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Live at River Plate"

# Album "AC/DC Live"
rows_album <- grepl("AC/DC Live", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "AC/DC Live"

# Album "BackTracks"
rows_album <- grepl("Backtracks", df$Album_Name, ignore.case = TRUE)
df$Album_Name[rows_album] <- "Backtracks"

# Unreleased tracks
rows_album <- grepl("Unreleased", df$Album_Name, ignore.case = TRUE)
# Unreleased tracks will be deleted
df$Album_Name[rows_album] <- NA
df <- df[complete.cases(df$Album_Name),]

# B-side tracks
rows_album <- grepl("B-side", df$Album_Name, ignore.case = TRUE)
# B-side tracks will be deleted
df$Album_Name[rows_album] <- NA
df <- df[complete.cases(df$Album_Name),]

# add column Band
df$Band <- "AC/DC"

# switch positions of the columns Band and Writers
df <- df[,c(1,6,3,4,5,2)]



#######################################################################################
# loading all albums to get data about Song duration and Track number
#######################################################################################

# adding columns Track_Number and Song_Duration
df$Track_Number <- NA
df$Song_Duration <- NA

# links to the album data

# Albums: High Voltage, T.N.T. , Dirty deeds done dirt cheap, Let There Be Rock, Powerage,
# Highway to Hell, Back in Black, For Those About to Rock We Salute You, FLick of the Switch
# Fly on the Wall, Blow up your video, The Razors Edge,Ballbreaker, Stiff Upper Lip
# Black Ice, Rock or Bust, Power Up, If You Want Blood You've Got It, AC/DC Live,
# Live at River Plate, Who Made Who,Backtracks
albums_links <- c("https://www.allmusic.com/album/high-voltage-mw0000188976",
                  "https://www.allmusic.com/album/tnt-mw0000599239",
                  "https://www.allmusic.com/album/dirty-deeds-done-dirt-cheap-mw0000649804",
                  "https://www.allmusic.com/album/let-there-be-rock-mw0000188896",
                  "https://www.allmusic.com/album/powerage-mw0000194999",
                  "https://www.allmusic.com/album/highway-to-hell-mw0000649805",
                  "https://www.allmusic.com/album/back-in-black-mw0000188862",
                  "https://www.allmusic.com/album/for-those-about-to-rock-we-salute-you-mw0000188895",
                  "https://www.allmusic.com/album/flick-of-the-switch-mw0000188975",
                  "https://www.allmusic.com/album/fly-on-the-wall-mw0000192737",
                  "https://www.allmusic.com/album/blow-up-your-video-mw0000198970",
                  "https://www.allmusic.com/album/the-razors-edge-mw0000690074",
                  "https://www.allmusic.com/album/ballbreaker-mw0000175703",
                  "https://www.allmusic.com/album/stiff-upper-lip-mw0000053121",
                  "https://www.allmusic.com/album/black-ice-mw0000797626",
                  "https://www.allmusic.com/album/rock-or-bust-mw0002762127#:~:text=Rock%20or%20Bust%2C%20the%20group's,AC%2FDC%20have%20ever%20released.",
                  "https://www.allmusic.com/album/power-up-mw0003437002",
                  "https://www.allmusic.com/album/if-you-want-blood-youve-got-it-mw0000192738#:~:text=If%20You%20Want%20Blood%20You've%20Got%20It%20Review&text=Few%20others%20could%20match%20the,and%20memorable%2C%20riffs%20and%20grooves.",
                  "https://www.allmusic.com/album/live-mw0000616074",
                  "https://www.allmusic.com/album/live-at-river-plate-mw0002423204",
                  "https://www.allmusic.com/album/who-made-who-mw0000649806",
                  "https://www.allmusic.com/album/backtracks-mw0001777691"
)


# processing data related to albums 
for (i in 1:length(albums_links)) {
  page <- read_html(albums_links[i])
  album <-  html_table(page, fill = TRUE)[[1]]
  
  # select valid columns and rename them
  album <- album[,c(2,3,5)]
  colnames(album) <- c("Track Number","Title","Song Duration")
  album[album == ""] <- NA
  
  # cleaning column Title
  album$Title <- gsub("\\s+", " ",album$Title)
  album$Title <- gsub("\n", "", album$Title) 
  album$"Title" <- gsub("/.*", "", album$"Title")
  
  # removing Composer values from Title column
  composers <- c("Bon Scott", "Angus Young", "Malcolm Young", "Chuck Berry", "Brian Johnson")
  for (composer in composers) {
    album$Title <- gsub(composer, "", album$Title)
  }
  
  # select all song titles from the album
  songs <- album$Title
  
  # iterate through each song title from the album and find Track Number i Song Duration data
  for (i in 1:length(songs)) {
    indeks <- which(tolower(gsub("\\s+", "", df$Song_Title))== tolower(gsub("\\s+", "", songs[i])))
    df[indeks,]$Track_Number <- album[i,]$`Track Number`
    df[indeks,]$Song_Duration <- album[i, ]$`Song Duration`
  }
  
}

# imam 14 pesama za koje mi fali Song_Duration i Track_Number
sum(is.na(df$Track_Number))



#########################################################################################
# load data about record labels
#########################################################################################

# link to the record label data 
url <- "https://www.allmusic.com/artist/ac-dc-mn0000574772/discography"

# load record label data and select valid columns
page <- read_html(url)
record_label <-  html_table(page, fill = TRUE)[[1]]
record_label <- record_label[-c(1,2,6,7,8)]

# rename some albums so that names match with df$Album_Name
record_label$Album <-  gsub("The Razor's Edge", "The Razors Edge", record_label$Album)
record_label$Album <-  gsub("Live", "AC/DC Live", record_label$Album)

# add Record_Label column
df$Record_Label <- NA

# iterate through each album title and find Record Label
for (i in 1:length(record_label$Album)) {
  indeks <- which(tolower(gsub("\\s+", "", df$Album_Name))== tolower(gsub("\\s+", "", record_label$Album[i])))
  df[indeks,]$Record_Label<- record_label[i,]$Label
}



########################################################################################
# Add audio features from Spotify playlist
########################################################################################

#install.packages("spotifyr")
library(spotifyr)

# set API keys to authenticate the R session with Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = "e1707a9784094b098e6c302f4a6222b9")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "058710aee06347fea415a46f5f2bc883")

# generate access token
access_token <- get_spotify_access_token()

# playlist url
playlist_url <- "https://open.spotify.com/playlist/4cL9rp2NoV2lcIbiAe3t58"

# extract playlist ID from playlist url
playlist_id <- sub("^.+/([[:alnum:]]+)$", "\\1", playlist_url)

# initialize an empty data frame to store all tracks from the playlist
all_tracks <- NULL

# set the starting offset
offset <- 0
# set the number of tracks to be fetched in a single API call
limit <- 50

# a repeat loop to fetch tracks page by page 
repeat {
  tracks <- get_playlist_tracks(playlist_id, limit = limit, offset = offset)
  
  # loop exit condition
  if (length(tracks) == 0) {
    break
  }
  
  tracks_df <- as.data.frame(tracks)
  all_tracks <- bind_rows(all_tracks, tracks_df)
  
  # update the offset to fetch the next page
  offset <- offset + limit
}

# extracting track IDs and track names from the 'all_tracks'
track_ids <- all_tracks$track.id
track_name <- all_tracks$track.name

# create an empty list to store audio features for all tracks
all_audio_features <- list()

# iterating through each track ID to fetch audio features
for (track_id in track_ids) {
  
  audio_feature <- get_track_audio_features(track_id)
  
  track <- get_track(track_id)
  track_name <- track$name
  
  audio_feature$Track_ID <- track_id
  audio_feature$Track <- track_name
  all_audio_features[[track_id]] <- audio_feature
}


all_audio_features_df <- do.call(rbind, all_audio_features)

# adding a new column Row to the all_audio_features_df
all_audio_features_df$Row <- seq_len(nrow(all_audio_features_df))

# cleaning Track column
all_audio_features_df$Track <- str_to_upper(all_audio_features_df$Track)
all_audio_features_df$Track <- sub(" -.*", "", all_audio_features_df$Track)
all_audio_features_df$Track <- trimws(all_audio_features_df$Track)

# creating new columns with NA values in df
df$Key <- NA
df$Mode <- NA
df$Loudness <- NA
df$Tempo <- NA

df$Acousticness <- NA
df$Danceability <- NA
df$Energy <- NA
df$Instrumentalness <- NA
df$Liveness <- NA
df$Speechiness <- NA
df$Valence <- NA

songs <- all_audio_features_df$Track

# ooping through each song and adding attributes to that song in df
for (i in 1:length(all_audio_features_df$id)) {
  
  #f inding the row index in the main data frame df based on song title
  indeks <- which(str_to_upper(trimws(df$Song_Title)) == songs[i])
  
  df[indeks,]$Key <- all_audio_features_df[i,]$key
  df[indeks,]$Mode <- all_audio_features_df[i,]$mode
  df[indeks,]$Loudness <- all_audio_features_df[i,]$loudness
  df[indeks,]$Tempo <- all_audio_features_df[i,]$tempo
  
  df[indeks,]$Acousticness <- all_audio_features_df[i,]$acousticness
  df[indeks,]$Danceability <- all_audio_features_df[i,]$danceability
  df[indeks,]$Energy <- all_audio_features_df[i,]$energy
  df[indeks,]$Instrumentalness <- all_audio_features_df[i,]$instrumentalness
  df[indeks,]$Liveness <- all_audio_features_df[i,]$liveness
  df[indeks,]$Speechiness <- all_audio_features_df[i,]$speechiness
  df[indeks,]$Valence <- all_audio_features_df[i,]$valence
  
}


########################################################################################
# Add data about genres, styles, moods and themes
########################################################################################

# define all the pages links
pages_url <- c("https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all",
               "https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all/2",
               "https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all/3",
               "https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all/4",
               "https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all/5",
               "https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all/6",
               "https://www.allmusic.com/artist/ac-dc-mn0000574772/songs/all/7"
)

# create an empty vector to store song links
all_songs_links <- c()

# loop through each page url and extract song links
for(i in 1:length(pages_url)){
  page <- read_html(pages_url[i])
  
  # extract the table data containing song links
  table_data <- page %>%
    html_nodes("table")
  
  tab <- table_data %>%
    html_nodes("tbody") 
  
  tab1 <- tab  %>%
    html_nodes("div.title")
  
  tab2 <- tab1 %>%
    html_nodes("a") 
  
  href_content <- tab2 %>%
    html_attr("href")
  
  # append the extracted song links to the 'all_songs_links' vector
  all_songs_links <- c(all_songs_links, href_content)
}

# delete invalid links
all_songs_links[grepl("^/song/", all_songs_links)] <- NA
all_songs_links[grepl("/artist/", all_songs_links)] <- NA
all_songs_links <- all_songs_links[complete.cases(all_songs_links)]

# remove duplicate songs
all_songs_links <- all_songs_links[seq(2, length(all_songs_links), by = 2)]
all_songs_links

# create data frame to store attributes
song_data <- data.frame(
  Song_Title = character(),
  Genres = character(),
  Styles = character(),
  Moods = character(),
  Themes = character(),
  stringsAsFactors = FALSE
)


# extract attributes for every song
for(i in 1:length(all_songs_links)){
  # loading content page
  song_page_html <- read_html(all_songs_links[i])
  
  # retrieving song title
  song_title <- ""
  song_title <- song_page_html %>%
    html_nodes("h1.song-title") %>% html_text()
  
  # clean the string to extract only the title and add song_title to dataset
  song_title <- gsub("^\\s+|\\s+$", "", song_title)
  song_title <- gsub("\\n", "", song_title)
  
  song_data[i,]$Song_Title <- song_title
  
  
  # adding Genres -------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_genres div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  attribute
  
  # clean the string attribute containing genres
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Genres <- cleaned_string
  
  # adding Styles --------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_styles div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  attribute
  
  # clean the string attribute containing styles
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Styles <- cleaned_string
  
  # adding Moods ---------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_moods div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  
  # clean the string attribute containing moods
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Moods <- cleaned_string
  
  # adding Themes -------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_themes div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  
  # clean the string attribute containing themes
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Themes <- cleaned_string
  
}

# replace "" fields with NA
song_data$Styles[song_data$Styles == ""] <- NA
song_data$Genres[song_data$Genres == ""] <- NA
song_data$Moods[song_data$Moods == ""] <- NA
song_data$Themes[song_data$Themes == ""] <- NA

# extract the rows with values in Styles, Genres, Themes or Moods
song_data <- song_data[apply(song_data[, c("Styles", "Genres", "Themes", "Moods")], 1, function(x) any(!is.na(x))), ]

# remove duplicates
duplicates <- duplicated(song_data$Song_Title)
song_data <- song_data[!duplicates, ]

# add Genres, Styles, Moods and Themes to df
df <- left_join(df, song_data, by = "Song_Title")



########################################################################################
# Add data about UK Charts performance
########################################################################################

# define the URL of the page containing data about UK charts
url <- "https://www.officialcharts.com/artist/16970/ac-dc/"

# read the HTML content of the webpage
page <- read_html(url)

# extract all 'div' elements with class "chart-content"
page %>%
  html_nodes("div.chart-content") %>%
  head()

# extract the first 'div' element with class "chart-content"
first_chart_content <- page %>%
  html_nodes("div.chart-content") %>%
  head(1)

# extract all 'div' elements with class "chart-items" from first_chart_content
first_chart_items <- first_chart_content %>%
  html_nodes("div.chart-items")

# extract all 'div' elements with class "chart-item"
first_chart_item <- first_chart_content %>%
  html_nodes("div.chart-item")

# extract all 'div' elements with class "chart-item-content"
first_chart_item_content <- first_chart_content %>%
  html_nodes("div.chart-item-content")

# extract song position and number of weeks
first_chart_item_content_desc <- first_chart_content %>%
  html_nodes("div.description") %>%
  html_text()

# format first_chart_item_content_desc
first_chart_item_content_desc <- gsub("AC/DC", ", ", first_chart_item_content_desc)

# Extract debut dates on the UK Charts for each song
first_chart_item_content_date <- first_chart_content %>%
  html_nodes("time.date") %>%
  html_text()

# merge song positions, number of weeks and debut dates
uk_charts_data <- paste(first_chart_item_content_desc, first_chart_item_content_date, sep = ", ")
uk_charts_data <- strsplit(uk_charts_data, ", ")

# convert uk_charts_data to data frame and format
df_british_charts <- data.frame(matrix(unlist(uk_charts_data), ncol = 4, byrow = TRUE))

# set column names and format
colnames(df_british_charts) <- c("Song_Title", "Peak", "Weeks", "Date")
df_british_charts$Peak <- str_replace(df_british_charts$Peak, "Peak: ", "")
df_british_charts$Weeks <- str_replace(df_british_charts$Weeks, "Weeks: ", "")

# ad uk charts data to df ----------------

# add columns British_Charts, UK_Debut_Date, UK_Peak_Pos and UK_Chart_Weeks
df$British_Charts <- 'No'
df$UK_Debut_Date <- NA
df$UK_Peak_Pos <- NA
df$UK_Chart_Weeks <- NA

# extract song titles from the df_british_charts
songs <- df_british_charts$Song_Title

# loop through each song title and add uk charts data to df
for (i in 1:length(songs)) {
  # find the index of the song title in the df 
  indeks <- which(str_to_upper((trimws(df$Song_Title))) == songs[i])
  
  df[indeks,]$British_Charts <- 'Yes'
  df[indeks,]$UK_Debut_Date <- df_british_charts[i,]$Date
  df[indeks,]$UK_Peak_Pos <- df_british_charts[i,]$Peak
  df[indeks,]$UK_Chart_Weeks <- df_british_charts[i,]$Weeks
}




########################################################################################
# Add data about US Charts performance
########################################################################################


# extract Billboard chart history for AC/DC from the given URL
url <- "https://www.billboard.com/artist/ac-dc/"
page <- read_html(url)

# extract the table containing Billboard chart history items
table <- page %>%
  html_nodes("div.artist-chart-history-items") %>%
  head(1)

# extract song titles 
titles <- table %>%
  html_nodes("h3.artist-chart-row-title") %>%
  html_text()

# extract debut dates
debut_dates <- table %>%
  html_nodes("span.artist-chart-row-debut-date a") %>%
  html_text()

# extract peak positions
peak_positions <- table %>%
  html_nodes("span.artist-chart-row-peak-pos") %>%
  html_text()

# extract weeks on chart
weeks_on_chart <- table %>%
  html_nodes("span.artist-chart-row-week-on-chart") %>%
  html_text()

# create a dataframe to store us charts data
df_billboard <- data.frame(
  Song_Title = titles,
  US_Debut_Date = debut_dates,
  US_Peak_Pos = peak_positions,
  US_Chart_Weeks = weeks_on_chart
)

# format columns
df_billboard <- as.data.frame(apply(df_billboard,2,function(x){gsub("\\s+", " ", x)}))
df_billboard <- as.data.frame(apply(df_billboard,2,function(x){trimws(x)}))


# add columns related to Billboard 200 information in df
df$US_100 <- 'No'
df$US_Debut_Date <- NA
df$US_Peak_Pos <- NA
df$US_Chart_Weeks <- NA

# extract song titles from the df_billboard
songs <- df_billboard$Song_Title

# loop through each song title and add data about US charts to df
for (i in 1:length(songs)) {
  # find the index of the song title in the df 
  indeks <- which(str_to_upper((trimws(df$Song_Title))) == str_to_upper(songs[i]))
  
  df[indeks,]$US_100 <- 'Yes'
  df[indeks,]$US_Debut_Date <- df_billboard[i,]$US_Debut_Date
  df[indeks,]$US_Peak_Pos <- df_billboard[i,]$US_Peak_Pos
  df[indeks,]$US_Chart_Weeks <- df_billboard[i,]$US_Chart_Weeks
}



########################################################################################
# Add data about International charts performance
########################################################################################

# load the Wikipedia page containing AC/DC discography and read the HTML content
url  <- "https://en.wikipedia.org/wiki/AC/DC_discography#Singles"
page <- read_html(url)

# extract table from the HTML page using an XPath expression
table <- page %>% html_nodes(xpath = "/html/body/div[2]/div/div[3]/main/div[3]/div[3]/div[1]/table[7]/tbody") %>% html_table(fill = TRUE) %>% .[[1]]

# remove unnecessary columns and rows
table
table[,c(10,11,12,14)] <- NULL
table <- table[-1,]

# rename columns
colnames(table) <- c("Year","Song_Title","AUS","CAN","GER","NLD","NZ","SWE","SWI", "Certifications")
table$Song_Title <-  gsub("\"", "", table$Song_Title)

# add new columns to the df
df$Year <- NA
df$AUS <- NA
df$CAN <- NA
df$GER <- NA
df$NLD <- NA
df$NZ <- NA
df$SWE <- NA
df$SWI <- NA
df$Certifications <- NA

# extract song titles from the table
songs <- table$Song_Title

# loop through each song title and add data about International charts to df
for (i in 1:length(table$Song_Title)) {
  # find the index of the song title in the df 
  indeks <- which((trimws(df$Song_Title)) == songs[i])
  
  df[indeks,]$Year <- table[i,]$Year
  df[indeks,]$AUS <- table[i,]$AUS
  df[indeks,]$CAN <- table[i,]$CAN
  df[indeks,]$GER <- table[i,]$GER
  df[indeks,]$NLD <- table[i,]$NLD
  df[indeks,]$NZ <- table[i,]$NZ
  df[indeks,]$SWE <- table[i,]$SWE
  df[indeks,]$SWI <- table[i,]$SWI
  df[indeks,]$Certifications <- table[i,]$Certifications
}

df[3,]$Year <- NA

# formating charts data
df[,26:28] <- data.frame(apply(df[,26:28], 2,function(x) ifelse(is.na(x), "-", x)))
df[,30:41] <- data.frame(apply(df[,30:41], 2,function(x) ifelse(is.na(x), "-", x)))
df[, 26:41] <- lapply(df[, 26:41], function(x) gsub("—", "-", x))
df$Certifications[df$Certifications == ""] <- '-'


acdc <- df



###################################################################################################################
###################################################################################################################
##                                          Iron Maiden
###################################################################################################################
###################################################################################################################


########################################################################################
# loading initial data about Song titles, Albums and Writers and Years
########################################################################################

df <- data.frame()

# define data source url (Wikipedia)
url <- "https://en.wikipedia.org/wiki/List_of_songs_recorded_by_Iron_Maiden"

# load html page 
page <- read_html(url)

# load all the tables from the page
tables <- page %>% html_nodes("table.wikitable.sortable")

# select first table and convert it to data frame
table <- tables[[1]]
df <-  html_table(table, fill = TRUE)  
str(df)

# remove last column
df$Ref. <- NULL

# change column names
colnames(df) <- c("Song_Title", "Writers", "Album_Name", "Year_of_First_Release")

# remove "" from Song_Title column
df$Song_Title <-  gsub("\"", "", df$Song_Title)

# remove "" from Album_Name column
df$Album_Name <-  gsub( "\\\\", "", df$Album_Name)
df$Album_Name <-  gsub("\"", "", df$Album_Name)

# add column Single
df$Single <- "No"
str(df)

# find rows that contain † value in the column Song_Title and mark that rows as Singles
rows_with_plus <- grep("†", df$Song_Title)
df$Single[rows_with_plus] <- "Yes"
df$Single <- as.factor(df$Single)

# remove † value from the Song_Title column
df$Song_Title <-  gsub("†", "", df$Song_Title)

df$Song_Title <- sub("\\[.*","",df$Song_Title)

str(df)

#remove ‡ value from Wruters column
df$Writers<-  gsub("‡", "", df$Writers)

df$Writers <- sub("\\[.*","",df$Writers)

str(df)

# Format Writers column so that names are separated with ","
df$Writers <- sapply(df$Writers, function(x){gsub("(?<=[a-z])(?=[A-Z])", ", ", x, perl = TRUE)})


# Cleaning and standardizing the album names --------------------

length(unique(df$Album_Name))
unique(df$Album_Name)

# add column Band
df$Band <- "Iron Maiden"

# switch positions of the columns Band and Writers
df <- df[,c(1,6,3,4,5,2)]



#######################################################################################
# loading all albums to get data about Song duration and Track number
#######################################################################################

# adding columns Track_Number and Song_Duration
df$Track_Number <- NA
df$Song_Duration <- NA

# links to the album data

# Albums: Iron Maiden, Killers, The Number of the Beast, Piece of Mind, Powerslave, 
# The X factor, Fear of the Dark, Somewhere in time, Virtual XI, No Prayer for the Dying,
# A Matter of Life and Death, Brave New World, Seventh Son of a Seventh Son, 
# Running Free, Can I Play With Madness, Fear of the Dark, Dance to Death, 
# The Final Frontier, Be Quick or Be Dead, Brave New World, The Book of Souls,
# The Trooper, Senjitsu, Different World, From Here to Eternity, Flight of Icarus
# Woman in Uniform, Sranger in Strange Land, Aces High, Rainmaker, @ Minutes To Midnight, 
# Wasted Years, Re-Machined: A Tribute to Deep Purple's Machine Head, Run to the Hills, Beast of the Beast

albums_links <- c("https://www.allmusic.com/album/iron-maiden-mw0000198284",
                  "https://www.allmusic.com/album/killers-mw0000601464",
                  "https://www.allmusic.com/album/the-number-of-the-beast-mw0000044718",
                  "https://www.allmusic.com/album/piece-of-mind-mw0000045853",
                  "https://www.allmusic.com/album/powerslave-mw0000190289",
                  "https://www.allmusic.com/album/the-x-factor-mw0000645867",
                  "https://www.allmusic.com/album/fear-of-the-dark-mw0000073138",
                  "https://www.allmusic.com/album/somewhere-in-time-mw0000650285",
                  "https://www.allmusic.com/album/virtual-xi-mw0000597969",
                  "https://www.allmusic.com/album/no-prayer-for-the-dying-mw0000204748",
                  "https://www.allmusic.com/album/a-matter-of-life-and-death-mw0000542236",
                  "https://www.allmusic.com/album/brave-new-world-mw0000605881",
                  "https://www.allmusic.com/album/seventh-son-of-a-seventh-son-mw0000195384",
                  "https://www.allmusic.com/album/running-free-mw0001890505",
                  "https://www.allmusic.com/album/can-i-play-with-madness-mw0000976622",
                  "https://www.allmusic.com/album/fear-of-the-dark-mw0000073138",
                  "https://www.allmusic.com/album/dance-of-death-mw0000598895",
                  "https://www.allmusic.com/album/the-final-frontier-mw0002011126",
                  "https://www.allmusic.com/album/be-quick-or-be-dead-live-mw0002309866",
                  "https://www.allmusic.com/album/brave-new-world-mw0000605881",
                  "https://www.allmusic.com/album/the-book-of-souls-mw0002855793",
                  "https://www.allmusic.com/album/the-trooper-mw0000711438",
                  "https://www.allmusic.com/album/senjutsu-mw0003559950",
                  "https://www.allmusic.com/album/different-world-2-track-single--mw0001515298",
                  "https://www.allmusic.com/album/from-there-to-eternity-mw0000078140",
                  "https://www.allmusic.com/album/flight-of-icarus-mw0000974202",
                  "https://www.allmusic.com/album/women-in-uniform-mw0001230984",
                  "https://www.allmusic.com/album/stranger-in-a-strange-land-mw0000907623",
                  "https://www.allmusic.com/album/aces-high-mw0000857941",
                  "https://www.allmusic.com/album/rainmaker-mw0000464168",
                  "https://www.allmusic.com/album/2-minutes-to-midnight-mw0000973210",
                  "https://www.allmusic.com/album/wasted-years-mw0002747177",
                  "https://www.allmusic.com/album/re-machined-a-tribute-to-deep-purples-machine-head-mw0002410805",
                  "https://www.allmusic.com/album/run-to-the-hills-mw0000532086",
                  "https://www.allmusic.com/album/the-best-of-the-beast-mw0000082114"
)


unique(df$Writers)


# rename some songs so that titles match
# Alexander the Great (356-323 B.C.)
df[9,]$Song_Title <- "Alexander the Great (356-323 B.C.)"

# processing data related to albums 
for (i in 1:length(albums_links)) {
  page <- read_html(albums_links[i])
  album <-  html_table(page, fill = TRUE)[[1]]
  
  # select valid columns and rename them
  album <- album[,c(2,3,5)]
  colnames(album) <- c("Track Number","Title","Song Duration")
  album[album == ""] <- NA
  
  # cleaning column Title
  album$Title <- gsub("\\s+", " ",album$Title)
  album$Title <- gsub("\n", "", album$Title) 
  album$"Title" <- gsub("/.*", "", album$"Title")
  
  # removing Composer values from Title column
  composers <- c("Steve Harris", "Bruce Dickinson", "Blaze Bayley", "Janick Gers",
                 "Adrian Smith", "Dave Murray", "Phil Mogg", "Paul Rodgers", "Andy Fraser",
                 "Thijs van Leer", "Jan Akkerman", "Ronnie Montrose", "Pete Townshend",
                 "Steve Barnacle", "Derek O'Neil", "Iron Maiden", "Del Bromham", 
                 "Jimmy Page", "John Paul Jones", "John Bonham", "Ian Anderson",
                 "Clive Burr", "Michael Schenker", "Burke Shelley", "Tony Bourge",
                 "Ronnie Montrose", "Steve Barnacle", "Derek O'Neil", "David Murray")
  
  for (composer in composers) {
    album$Title <- gsub(composer, "", album$Title)
  }
  
  # select all song titles from the album
  songs <- album$Title
  
  # iterate through each song title from the album and find Track Number i Song Duration data
  for (i in 1:length(songs)) {
    indeks <- which(tolower(gsub("\\s+", "", df$Song_Title))== tolower(gsub("\\s+", "", songs[i])))
    df[indeks,]$Track_Number <- album[i,]$`Track Number`
    df[indeks,]$Song_Duration <- album[i, ]$`Song Duration`
  }
  
}

# getting song duration and track number data for some songs from Wikipedia
albums_links <- c(
  "https://en.wikipedia.org/wiki/Senjutsu_(album)#Track_listing",
  "https://en.wikipedia.org/wiki/The_Book_of_Souls#Track_listing",
  "https://en.wikipedia.org/wiki/Death_on_the_Road#Track_listing"
)

for(i in 1:3){
  page <- read_html(albums_links[i])
  
  album <- page %>%
    html_nodes(".track-listing") %>%
    html_table(fill = TRUE) %>%
    .[[2]]
  
  # select valid columns and rename them
  album <- album[-6,c(1,2,4)]
  colnames(album) <- c("Track Number","Title","Song Duration")
  album[album == ""] <- NA
  
  
  # cleaning column Title
  album$Title <- gsub("\"", "", album$Title)
  album$Title <- gsub("\\s+", " ",album$Title)
  album$Title <- gsub("\n", "", album$Title) 
  album$"Title" <- gsub("/.*", "", album$"Title")
  
  
  album$`Track Number` <- gsub("\\.", "", album$`Track Number`)
  album$`Track Number` <- as.numeric(album$`Track Number`)
  
  # select all song titles from the album
  songs <- album$Title
  
  # iterate through each song title from the album and find Track Number i Song Duration data
  for (i in 1:length(songs)) {
    indeks <- which(tolower(gsub("\\s+", "", df$Song_Title))== tolower(gsub("\\s+", "", songs[i])))
    df[indeks,]$Track_Number <- album[i,]$`Track Number`
    df[indeks,]$Song_Duration <- album[i, ]$`Song Duration`
  }
}


#########################################################################################
# load data about record labels
#########################################################################################

# link to the record label data 
url <- "https://www.allmusic.com/artist/iron-maiden-mn0000098465/discography"

# load record label data and select valid columns
page <- read_html(url)
record_label <-  html_table(page, fill = TRUE)[[1]]
record_label <- record_label[-c(1,2,6,7,8)]

# add Record_Label column
df$Record_Label <- NA

# iterate through each album title and find Record Label
for (i in 1:length(record_label$Album)) {
  indeks <- which(tolower(gsub("\\s+", "", df$Album_Name))== tolower(gsub("\\s+", "", record_label$Album[i])))
  df[indeks,]$Record_Label<- record_label[i,]$Label
}



########################################################################################
# Add audio features from Spotify playlist
########################################################################################

library(spotifyr)

# set API keys to authenticate the R session with Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = "e1707a9784094b098e6c302f4a6222b9")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "058710aee06347fea415a46f5f2bc883")

# generate access token
access_token <- get_spotify_access_token()

# playlist url
playlist_url <- "https://open.spotify.com/playlist/1VoTGn2zY5fLFX14RrL6qG"

# extract playlist ID from playlist url
playlist_id <- sub("^.+/([[:alnum:]]+)$", "\\1", playlist_url)

# initialize an empty data frame to store all tracks from the playlist
all_tracks <- NULL

# set the starting offset
offset <- 0
# set the number of tracks to be fetched in a single API call
limit <- 50

# a repeat loop to fetch tracks page by page 
repeat {
  tracks <- get_playlist_tracks(playlist_id, limit = limit, offset = offset)
  
  # loop exit condition
  if (length(tracks) == 0) {
    break
  }
  
  tracks_df <- as.data.frame(tracks)
  all_tracks <- bind_rows(all_tracks, tracks_df)
  
  # update the offset to fetch the next page
  offset <- offset + limit
}

# extracting track IDs and track names from the 'all_tracks'
track_ids <- all_tracks$track.id
track_name <- all_tracks$track.name

# create an empty list to store audio features for all tracks
all_audio_features <- list()

# iterating through each track ID to fetch audio features
for (track_id in track_ids) {
  
  audio_feature <- get_track_audio_features(track_id)
  
  track <- get_track(track_id)
  track_name <- track$name
  
  audio_feature$Track_ID <- track_id
  audio_feature$Track <- track_name
  all_audio_features[[track_id]] <- audio_feature
}


all_audio_features_df <- do.call(rbind, all_audio_features)

# adding a new column Row to the all_audio_features_df
all_audio_features_df$Row <- seq_len(nrow(all_audio_features_df))

# cleaning Track column
all_audio_features_df$Track <- toupper(all_audio_features_df$Track)
all_audio_features_df$Track <- sub(" -.*", "", all_audio_features_df$Track)
all_audio_features_df$Track <- trimws(all_audio_features_df$Track)

# creating new columns with NA values in df
df$Key <- NA
df$Mode <- NA
df$Loudness <- NA
df$Tempo <- NA

df$Acousticness <- NA
df$Danceability <- NA
df$Energy <- NA
df$Instrumentalness <- NA
df$Liveness <- NA
df$Speechiness <- NA
df$Valence <- NA

songs <- all_audio_features_df$Track

# looping through each song and adding attributes to that song in df
for (i in 1:length(all_audio_features_df$id)) {
  
  #finding the row index in the main data frame df based on song title
  indeks <- which(toupper(trimws(df$Song_Title)) == songs[i])
  
  df[indeks,]$Key <- all_audio_features_df[i,]$key
  df[indeks,]$Mode <- all_audio_features_df[i,]$mode
  df[indeks,]$Loudness <- all_audio_features_df[i,]$loudness
  df[indeks,]$Tempo <- all_audio_features_df[i,]$tempo
  
  df[indeks,]$Acousticness <- all_audio_features_df[i,]$acousticness
  df[indeks,]$Danceability <- all_audio_features_df[i,]$danceability
  df[indeks,]$Energy <- all_audio_features_df[i,]$energy
  df[indeks,]$Instrumentalness <- all_audio_features_df[i,]$instrumentalness
  df[indeks,]$Liveness <- all_audio_features_df[i,]$liveness
  df[indeks,]$Speechiness <- all_audio_features_df[i,]$speechiness
  df[indeks,]$Valence <- all_audio_features_df[i,]$valence
  
}



########################################################################################
# Add data about genres, styles, moods and themes
########################################################################################

# define all the pages links
pages_url <- c("https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/2",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/3",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/4",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/5",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/6",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/7",
               "https://www.allmusic.com/artist/iron-maiden-mn0000098465/songs/all/8"
               
)

# create an empty vector to store song links
all_songs_links <- c()

# loop through each page url and extract song links
for(i in 1:length(pages_url)){
  page <- read_html(pages_url[i])
  
  # extract the table data containing song links
  table_data <- page %>%
    html_nodes("table")
  
  tab <- table_data %>%
    html_nodes("tbody") 
  
  tab1 <- tab  %>%
    html_nodes("div.title")
  
  tab2 <- tab1 %>%
    html_nodes("a") 
  
  href_content <- tab2 %>%
    html_attr("href")
  
  # append the extracted song links to the 'all_songs_links' vector
  all_songs_links <- c(all_songs_links, href_content)
}

# delete invalid links
all_songs_links[grepl("^/song/", all_songs_links)] <- NA
all_songs_links[grepl("/artist/", all_songs_links)] <- NA
all_songs_links <- all_songs_links[complete.cases(all_songs_links)]


# create data frame to store attributes
song_data <- data.frame(
  Song_Title = character(),
  Genres = character(),
  Styles = character(),
  Moods = character(),
  Themes = character(),
  stringsAsFactors = FALSE
)


# extract attributes for every song
for(i in 1:length(all_songs_links)){
  # loading content page
  song_page_html <- read_html(all_songs_links[i])
  
  # retrieving song title
  song_title <- ""
  song_title <- song_page_html %>%
    html_nodes("h1.song-title") %>% html_text()
  
  # clean the string to extract only the title and add song_title to dataset
  song_title <- gsub("^\\s+|\\s+$", "", song_title)
  song_title <- gsub("\\n", "", song_title)
  
  song_data[i,]$Song_Title <- song_title
  
  
  # adding Genres -------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_genres div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  attribute
  
  # clean the string attribute containing genres
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Genres <- cleaned_string
  
  # adding Styles --------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_styles div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  attribute
  
  # clean the string attribute containing styles
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Styles <- cleaned_string
  
  # adding Moods ---------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_moods div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  
  # clean the string attribute containing moods
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Moods <- cleaned_string
  
  # adding Themes -------------------------------------------------------
  tab <- song_page_html %>%
    html_nodes("div.song_themes div.middle") %>%
    head(1)
  
  attribute <- tab %>%
    html_nodes("a") %>% html_text()
  
  # clean the string attribute containing themes
  cleaned_string <- gsub("\"|\\s*\\(\\d+\\)", "", attribute, perl = TRUE)
  cleaned_string <- paste(cleaned_string, collapse = ", ")
  song_data[i,]$Themes <- cleaned_string
  
}

# replace "" fields with NA
song_data$Styles[song_data$Styles == ""] <- NA
song_data$Genres[song_data$Genres == ""] <- NA
song_data$Moods[song_data$Moods == ""] <- NA
song_data$Themes[song_data$Themes == ""] <- NA

song_data <- song_data[complete.cases(song_data$Genres),]

# extract the rows with values in Styles, Genres, Themes or Moods
song_data <- song_data[apply(song_data[, c("Styles", "Genres", "Themes", "Moods")], 1, function(x) any(!is.na(x))), ]

# remove duplicates
duplicates <- duplicated(song_data$Song_Title)
song_data <- song_data[!duplicates, ]

# add Genres, Styles, Moods and Themes to df
df <- left_join(df, song_data, by = "Song_Title")
