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

