# Learn-SpotifyR
SpotifyR is library for R that makes possible the crawl of interesting metadata from artist's songs from Spotify.

## Click here to watch the execution and explanation of the code
[![Learn SpotifyR - Greek Audio and Subs - English Subs](https://i.imgur.com/FjFexq9.jpg)](https://youtu.be/IQQFeKs5eWo)

## In a nutshell

#### 1) install.packages('spotifyr')

#### 2) library(spotifyr)

#### 3) Sys.setenv(SPOTIFY_CLIENT_ID = ' Your own client id from spotify dashboard')
####  Sys.setenv(SPOTIFY_CLIENT_SECRET = 'Your own client secret from spotify dashboard')
####  access_token <- get_spotify_access_token()
   
#### 4) artist.audio.features <- get_artist_audio_features('Name of an artist') 

Then you can do some cool music data analysis
