Packages <- c("tidyverse", "httr", "jsonlite", "lubridate")
lapply(Packages, library, character.only = TRUE)
options("scipen"=100, "digits"=4)

api_key <- "RGAPI-XXXX"

name <- "Agurin"
region <- "euw1"
m_url <- paste0("https://", region, ".api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", api_key, sep = "", collapse = "")

m_content <- httr::GET(m_url) %>%
  httr::content(., as = 'text') %>%
  jsonlite::fromJSON(.)

print(m_content$puuid)

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

start <- 0
count <- 20

match_history <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/by-puuid/", m_content$puuid, "/ids?start=", start, "&count=", count, "&api_key=", api_key) %>%
  httr::GET(.) %>%
  httr::content(.$id, as = 'text') %>%
  jsonlite::fromJSON(.)

closer <- function (match_id){
  link <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/", match_id, "?api_key=", api_key, sep = "", collapse = "") %>%
    httr::GET(.) %>%
    httr::content(., as = 'text') %>%
    jsonlite::fromJSON(.)
  return(link)
}

closer_match_history_info <- lapply(match_history,closer)

#gameEnd(Creation) - milliseconds
#gameDuration - seconds

end_time <- c[[1]]$info$gameCreation/1000
end_time
seconds_to_period(end_time)
as.POSIXct(end_time, origin="1970-01-01")

summonerInfo <- function(name, region) {
  m_url <- paste0("https://", region, ".api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", api_key, sep = "", collapse = "")

  summoner <- httr::GET(m_url) %>%
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

  match_history <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/by-puuid/", m_content$puuid, "/ids?start=", start, "&count=", count, "&api_key=", api_key) %>%
    httr::GET(.) %>%
    httr::content(.$id, as = 'text') %>%
    jsonlite::fromJSON(.)

  closer <- function (match_id){
    link <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/", match_id, "?api_key=", api_key, sep = "", collapse = "") %>%
      httr::GET(.) %>%
      httr::content(., as = 'text') %>%
      jsonlite::fromJSON(.)
    return(link)
  }

  closer_match_history_info <- lapply(match_history,closer)


  return(closer_match_history_info) #not gonna work like this at the end
}

summonerInfo('Agurin', 'euw1')
