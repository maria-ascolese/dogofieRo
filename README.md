### dogofieRo (just wanted to be a part of the whole R-themed names shenanigans)

In order to use the R package `geniusr`, I had to update the function `get_lyrics`, which controls the functions that take the form [get_lyrics_xxx()](https://ewenme.github.io/geniusr/articles/geniusr.html).

Here's the updated function:
```{r}
get_lyrics <- function (session) {
  lyrics <-  session %>% html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]')
  song <-  session %>% html_nodes(xpath = '//span[contains(@class, "SongHeaderdesktop__")]') %>% html_text(trim = TRUE) 
  artist <-  session %>% html_nodes(xpath = '//a[contains(@class, "SongHeaderdesktop__Artist")]') %>% html_text(trim = TRUE)
  artist <-  session %>% html_nodes(xpath = '//a[contains(@class, "HeaderArtistAndTracklistdesktop__Artist")]') %>% html_text(trim = TRUE)
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()
  lyrics <- html_text(lyrics, trim = TRUE)
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)
  if (is_empty(lyrics)) {
    return(tibble(line = NA, section_name = NA, section_artist = NA, 
                  song_name = song, artist_name = artist))
  }
  section_tags <- nchar(gsub(pattern = "\\[.*\\]", "", lyrics)) == 0
  sections <- geniusr:::repeat_before(lyrics, section_tags)
  sections <- gsub("\\[|\\]", "", sections)
  sections <- strsplit(sections, split = ": ", fixed = TRUE)
  section_name <- sapply(sections, "[", 1)
  section_artist <- sapply(sections, "[", 2)
  section_artist[is.na(section_artist)] <- artist
  tibble(line = lyrics[!section_tags], section_name = section_name[!section_tags], 
         section_artist = section_artist[!section_tags], song_name = song, 
         artist_name = artist)
}
```
