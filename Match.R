summonerMatches <- function(name, region, start, count) {
  if (start < count) {
    m_url <- paste0("https://", region, ".api.riotgames.com/lol/summoner/v4/summoners/by-name/", name, "?api_key=", api_key, sep = "", collapse = "")
    puuid <- httr::GET(m_url) %>%
      httr::content(r_result, as = 'text') %>%
      jsonlite::fromJSON(.) %>%
      .$puuid

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

    m_url <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/by-puuid/", puuid, "/ids?start=", start, "&count=", count, "&api_key=", api_key, sep = "", collapse = "")

    m_content <- httr::GET(m_url) %>%
      httr::content(., as = 'text') %>%
      jsonlite::fromJSON(.)

    dplyr::glimpse(m_content)
  }
  else {
    print("start match-id must be lower than end")
  }
}

closer_match_info <- function (match_id, region, timeline){
  if(region != "europe" || "americas" || "asia"){
    m_url <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/", match_id, if(timeline) "/timeline", "?api_key=", api_key, sep = "", collapse = "")

    m_content <- httr::GET(m_url) %>%
      httr::content(., as = 'text') %>%
      jsonlite::fromJSON(.)

    # print(m_url)
    dplyr::glimpse(m_content)
  }else{
    print("BG, wrong region(here are only three - europe, asia, americas)")
  }
}

summonerMatches('Agurin', 'euw1', 0, 20)
closer_match_info("EUW1_5729711315", "europe", FALSE)