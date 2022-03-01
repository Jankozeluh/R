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

matchHistoryStats <- function (player){
  name <- player$summonerInfo$name
  closer_m <- player$closer_match_history_info#[[1]]$info$participants

  matchstats_individual <- function (clm){
    sumId <- match(name,clm$info$participants$summonerName)
      part <- clm$info$participants
      slice <- slice(part,sumId)
    return(slice)
  }

  lap <- lapply(closer_m,matchstats_individual)

  return(list(match_stats=lap))
}

winRate <- function (individual){
  un <- individual$match_stats

  counting <- function (match){
    nm <- match %>%  select(win) %>% as.integer()
    return(nm)
  }
  vec_gw <- lapply(un,counting) %>% unlist()

  wr <- (sum(vec_gw)/length(un))*100
  message <- paste0("Winrate of ", un[[1]]$summonerName, " is ", wr, "% over last ", length(un), " games.")

  return(message)
}

getStat <- function (individual,stat){
  gt <- individual$match_stats

  stats <- function (match){
    nm <- match %>%  select(stat)
    return(nm)
  }
  vec_gw <- lapply(gt,stats) %>% unlist() %>% Reduce(c,.)

  print(paste0("The stat u choosed (", stat, ") over last ", length(gt), " games is in the variable."))

  return(vec_gw)
}



Agurin_individual <- matchHistoryStats(Agurin)
Agurin_individual$match_stats[[1]]$summonerName %>% length()

winRate(Agurin_individual)
championName <- getStat(Agurin_individual, "championName")
assists <- getStat(Agurin_individual, "assists")

str(assists)
summary(assists)
summary(championName)

# ncol(info)
# sumId <- 4
# ggs <- Agurin$closer_match_history_info[[1]]$info$participants
#
# full <- function (part){
#   print(part[[sumId]])
# }
#
# Agurin$closer_match_history_info %>% length()
#
# info %>% slice()
#
# nn <- ggs %>% slice(., sumId)  # how is it so easy
#
# ggs$challenges$deathsByEnemyChamps[[4]]
# nn$challenges$deathsByEnemyChamps
#
# bbb <- lapply(ggs,full)

# for (i in count(ggs[[1]][[1]])){
#   print(ggs[[i]][[sumId]])
# }

# mm[[1]][[1]]$
# mm[[1]] %>% group_by(summonerName)

# capture.output(mm, file = "agurin.csv")

# write.csv(mm[[1]],paste0(getwd(),"/Agurin.csv"))
# library("DataExplorer")
# DataExplorer doesnt work in DataSpell for some reason
# matchHistoryTimelineGraphs <- function (){
#
# }