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
closer_match_info <- function (match_id, region, timeline){ #must be updated soon
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

#gameEnd(Creation) - milliseconds
#gameDuration - seconds
closer_match_history_info[[1]]$info$frames$events[[3]] %>% names()
end_time <- closer_match_history_info[[1]]$info$frames$timestamp / 1000
end_time
seconds_to_period(end_time) %>% round(2)
as.POSIXct(end_time, origin = "1970-01-01")

matchHistoryGraphs <- function (player){
  name <- player$summonerInfo$name
  info <- player$closer_match_history_info[[1]]$info$participants

  sumId <- match(name,info$summonerName)


  gg <- info[[1]][[sumId]]

  return(list(gg,names(info)))
}

count(info)
Agurin$closer_match_history_info[[1]]$info$participants[[1]][[4]]

matchHistoryGraphs(Agurin)
