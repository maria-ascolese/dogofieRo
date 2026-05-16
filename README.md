# dogofieRo

> *Just wanted to be a part of the whole R-themed names shenanigans.*

An analysis of the Club Dogo discography using the R packages `spotifyr`, `geniusr`, and `tidytext`. Audio features from Spotify, lyrics from Genius, text analysis with tidy principles.

---

## What this is

Club Dogo is one of the most influential Italian rap groups of the 2000s. This project uses their discography as a playground for combining audio data (tempo, energy, valence, danceability) with lyric-level text analysis (word frequency, sentiment, tf-idf by album) in R.

---

## The geniusr problem — and the fix

Getting the lyrics required patching `geniusr`, which broke when Genius updated its HTML class names. The fix lives in [`R/get_lyrics_fixed.R`](R/get_lyrics_fixed.R).

This was inspired by the community thread on the geniusr GitHub:
[**ewenme/geniusr · Issue #17** — get_lyrics_id issue](https://github.com/ewenme/geniusr/issues/17)

Several people contributed patches over time as Genius kept changing its markup. My specific contribution was identifying that the `artist` XPath selector also needed updating — not just `song` and `lyrics` — replacing:

```r
# broken
session %>% html_nodes(xpath = '//a[contains(@class, "SongHeaderdesktop__Artist")]')
```

with:

```r
# fixed
session %>% html_nodes(xpath = '//a[contains(@class, "HeaderArtistAndTracklistdesktop__ListArtists")]')
```

The file `R/get_lyrics_fixed.R` packages this into a clean, documented function with a `patch_geniusr()` helper that overrides the internal function in the `geniusr` namespace, so all downstream wrappers (`get_lyrics_url()`, `get_lyrics_id()`, etc.) work without modification.

---

## Usage

```r
library(spotifyr)
library(geniusr)
library(tidytext)
library(dplyr)
library(ggplot2)

source("R/get_lyrics_fixed.R")

# Patch geniusr before using any lyrics functions
patch_geniusr()

# Get Club Dogo discography from Spotify
dogo <- get_artist_audio_features("Club Dogo")

# Get lyrics for a track
lyrics <- get_lyrics_url("https://genius.com/Club-dogo-lultima-notte-lyrics")
```

---

## Stack

| Package | Purpose |
|---|---|
| `spotifyr` | Spotify audio features via API |
| `geniusr` | Lyrics scraping from Genius |
| `tidytext` | Tidy text analysis, tf-idf, sentiment |
| `dplyr` / `tidyr` | Data wrangling |
| `ggplot2` | Visualization |

---

## Setup

**Spotify credentials** — create an app at [developer.spotify.com](https://developer.spotify.com) and set:
```r
Sys.setenv(SPOTIFY_CLIENT_ID = "your_client_id")
Sys.setenv(SPOTIFY_CLIENT_SECRET = "your_client_secret")
```

**Genius credentials** — get a token at [genius.com/api-clients](https://genius.com/api-clients) and set:
```r
Sys.setenv(GENIUS_API_TOKEN = "your_token")
```

Both can be stored permanently in `.Renviron` — never commit them to the repo.

---

## Topics

`r` · `rstats` · `tidytext` · `nlp` · `spotify` · `genius` · `music-analysis` · `italian` · `text-mining`
