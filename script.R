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

