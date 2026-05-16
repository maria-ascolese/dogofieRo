# get_lyrics_fixed.R
# ──────────────────────────────────────────────────────────────────────────────
# Patched version of geniusr::get_lyrics()
#
# The original function from the geniusr package breaks when Genius updates
# its HTML class names. This version uses updated XPath selectors that work
# with the current Genius.com markup (as of early 2024).
#
# If it stops working again, check the class names directly in the page source
# at genius.com and update the xpath strings accordingly.
#
# Dependencies: geniusr, rvest, xml2, dplyr, purrr
# ──────────────────────────────────────────────────────────────────────────────


#' Patched get_lyrics function for geniusr
#'
#' Replaces geniusr::get_lyrics() with updated XPath selectors.
#' Call this once after loading geniusr to override the broken function,
#' then use get_lyrics_url() and other geniusr wrappers as normal.
#'
#' @param session An rvest session object pointing to a Genius song page.
#' @return A tibble with columns: line, section_name, section_artist,
#'   song_name, artist_name.
#'
#' @examples
#' \dontrun{
#' library(geniusr)
#' source("R/get_lyrics_fixed.R")
#'
#' # Override the internal function
#' assignInNamespace("get_lyrics", get_lyrics, ns = "geniusr")
#'
#' # Now use geniusr as normal
#' lyrics <- get_lyrics_url("https://genius.com/Club-dogo-lultima-notte-lyrics")
#' }
get_lyrics <- function(session) {

  lyrics <- session |>
    rvest::html_nodes(xpath = '//div[contains(@class, "Lyrics__Container")]')

  song <- session |>
    rvest::html_nodes(
      xpath = '//h1[contains(@class, "SongHeaderdesktop__Title")]'
    ) |>
    rvest::html_text(trim = TRUE)

  artist <- session |>
    rvest::html_nodes(
      xpath = '//div[contains(@class, "HeaderArtistAndTracklistdesktop__ListArtists")]'
    ) |>
    rvest::html_text(trim = TRUE)

  # Replace <br> tags with newlines before extracting text
  xml2::xml_find_all(lyrics, ".//br") |>
    xml2::xml_add_sibling("p", "\n")

  xml2::xml_find_all(lyrics, ".//br") |>
    xml2::xml_remove()

  lyrics <- rvest::html_text(lyrics, trim = TRUE)
  lyrics <- unlist(strsplit(lyrics, split = "\n"))
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)

  # Return empty tibble if no lyrics found
  if (purrr::is_empty(lyrics)) {
    return(
      dplyr::tibble(
        line           = NA_character_,
        section_name   = NA_character_,
        section_artist = NA_character_,
        song_name      = song,
        artist_name    = artist
      )
    )
  }

  # Parse section tags (e.g. [Chorus], [Verse 1: Guè])
  section_tags <- nchar(gsub(pattern = "\\[.*\\]", "", lyrics)) == 0
  sections     <- geniusr:::repeat_before(lyrics, section_tags)
  sections     <- gsub("\\[|\\]", "", sections)
  sections     <- strsplit(sections, split = ": ", fixed = TRUE)

  section_name   <- sapply(sections, "[", 1)
  section_artist <- sapply(sections, "[", 2)

  # Fall back to main artist when section artist is not specified
  section_artist[is.na(section_artist)] <- artist

  dplyr::tibble(
    line           = lyrics[!section_tags],
    section_name   = section_name[!section_tags],
    section_artist = section_artist[!section_tags],
    song_name      = song,
    artist_name    = artist
  )
}


#' Override geniusr's internal get_lyrics with the patched version
#'
#' Call this once at the top of your script, after library(geniusr).
#' It patches the function in the geniusr namespace so all downstream
#' wrappers (get_lyrics_url, get_lyrics_id, etc.) use the fixed version.
#'
#' @return Invisible NULL. Called for its side effect.
#' @export
patch_geniusr <- function() {
  assignInNamespace("get_lyrics", get_lyrics, ns = "geniusr")
  message("geniusr::get_lyrics patched successfully.")
  invisible(NULL)
}
