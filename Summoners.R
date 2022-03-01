Packages <- c("tidyverse", "httr", "jsonlite", "lubridate")
lapply(Packages, library, character.only = TRUE)
options("scipen" = 10, "digits" = 2)

api_key <- "RGAPI-XXXX"
#
summonerInfo <- function(name, region, start, count) {
  summoner_url <- paste0("https://", region, ".api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", api_key, sep = "", collapse = "")

  summoner <- httr::GET(summoner_url) %>%
    httr::content(., as = 'text') %>%
    jsonlite::fromJSON(.)

  americas <- c("br1", "na1", "la1", "la2", "oc1")
  europe <- c("euw1", "eune1")
  asia <- c("kr", "tr1", "jp1", "ru")

  if (region %in% americas) {
    region <- "americas"
  }else if (region %in% europe) {
    region <- "europe"
  }else if (region %in% asia) {
    region <- "asia"
  }else {
    return("BG, wrong region.")
  }

  match_history <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/by-puuid/", summoner$puuid, "/ids?start=", start, "&count=", count, "&api_key=", api_key) %>%
    httr::GET(.) %>%
    httr::content(.$id, as = 'text') %>%
    jsonlite::fromJSON(.)

  closer <- function(match_id) {
    link <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/", match_id, "?api_key=", api_key, sep = "", collapse = "") %>%
      httr::GET(.) %>%
      httr::content(., as = 'text') %>%
      jsonlite::fromJSON(.)
    return(link)
  }

  closer_match_history_info <- lapply(match_history, closer)

  return(list(closer_match_history_info = closer_match_history_info, summonerInfo = summoner))
}

Agurin <- summonerInfo('Agurin', 'euw1', 0, 20)

# 60032