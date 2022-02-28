# https://euw1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5
# https://euw1.api.riotgames.com/lol/league/v4/grandmasterleagues/by-queue/RANKED_SOLO_5x5
# https://euw1.api.riotgames.com/lol/league/v4/masterleagues/by-queue/RANKED_SOLO_5x5

# https://euw1.api.riotgames.com/lol/league-exp/v4/entries/RANKED_SOLO_5x5/CHALLENGER/I?page=1
# https://euw1.api.riotgames.com/lol/league-exp/v4/entries/RANKED_SOLO_5x5/GRANDMASTER/I?page=1
# https://euw1.api.riotgames.com/lol/league-exp/v4/entries/RANKED_SOLO_5x5/MASTER/I?page=1

url <- "https://euw1.api.riotgames.com/lol/league-exp/v4/entries/RANKED_SOLO_5x5/CHALLENGER/I?page=1&api_key="
# ll <- "https://euw1.api.riotgames.com/lol/league/v4/challengerleagues/by-queue/RANKED_SOLO_5x5/?api_key="
jj <- paste0(url, api_key)


content <- httr::GET(jj) %>%
  httr::content(., as = 'text') %>%
  jsonlite::fromJSON(.)

tibble(content$summonerName,content$summonerId)

content$veteran

mm <- paste0("https://", region, ".api.riotgames.com/lol/match/v5/matches/by-puuid/", content$summonerId[1],"/ids?start=0&count=20&api_key=", api_key) %>%
  httr::GET(.) %>%
  httr::content(.$id, as = 'text') %>%
  jsonlite::fromJSON(.)

names(content)
# bg - i need to call another url to get the puuid, cuz the league-exp endpoint return only summonerId an summonerName
