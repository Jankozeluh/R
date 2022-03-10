# gameEnd(Creation) - milliseconds
#gameDuration - seconds
# closer_match_history_info[[1]]$info$frames$events[[3]] %>% names()
# end_time <- closer_match_history_info[[1]]$info$frames$timestamp / 1000
# end_time
# seconds_to_period(end_time) %>% round(2)
# as.POSIXct(end_time, origin = "1970-01-01")

#export without challenges and perks
matchHistoryStats <- function(player, timeline = FALSE, export = FALSE) {
  name <- player$summonerInfo$name
  closer_m <- player$closer_match_history_info #[[1]]$info$participants

  matchstats_individual <- function(clm) {
    sumId <- match(name, clm$info$participants$summonerName)
    part <- clm$info$participants
    slice <- slice(part, sumId)
    return(slice)
  }

  lap <- lapply(closer_m, matchstats_individual)

  if (export == TRUE) {
    if (timeline == TRUE) {
      # add the timeline to csv part and clean the data just a little bit
      print("neeh")
    }else{
      exportableMatch <- function(match) {
        return(match[-c(4, 57)])
      }
      export <- rbindlist(lapply(lap, exportableMatch))
      write.csv(export, paste0(getwd(), "/export/", export$summonerName[[1]], ".csv"), row.names = TRUE)
    }
    print("Just exported to csv file(also in variable)")
  }

  return(list(match_stats = lap))
}

winRate <- function(individual) {
  un <- individual$match_stats

  counting <- function(match) {
    nm <- match %>% select(win) %>% as.integer()
    return(nm)
  }

  vec_gw <- lapply(un, counting) %>% unlist()

  wr <- (sum(vec_gw) / length(un)) * 100
  message <- paste0("Winrate of ", un[[1]]$summonerName, " is ", wr, "% over last ", length(un), " games.")

  return(message)
}

getStat <- function(individual, stat) {
  gt <- individual$match_stats

  stats <- function(match) {
    nm <- match %>% select(all_of(stat))
    return(nm)
  }

  vec_gw <- lapply(gt, stats) %>% unlist() %>% Reduce(c, .)

  print(paste0("The stat u choosed (", stat, ") over last ", length(gt), " games is in the variable."))

  return(vec_gw)
}

champsPlayed <- function(individual) {
  st <- individual$match_stats

  champ <- function(match) {
    nm <- match %>% select(championName)
    return(nm)
  }

  champPer <- lapply(st, champ) %>%
    unlist() %>%
    factor() %>%
    summary() %>%
    sort(decreasing = TRUE)

  print("All played champions and their frequency are in the variable.")
  return(list(champsOcc = champPer))
}

summonersFromMatch <- function(match) {
  info <- match$info$participants
  return(list(summonners = info$summonerName, champions = info$championName))
}

summonersFromMatchHistory <- function(player) {
  matchHist <- player$closer_match_history_info

  info <- function(match) {
    summoner <- match$info$participants$summonerName
    champ <- match$info$participants$championName
    return(list(summoner = summoner, champion = champ))
  }

  done <- lapply(matchHist, info)

  return(done)
}


# match <- summonersFromMatch(Agurin$closer_match_history_info[[1]])
# matchH <- summonersFromMatchHistory(Agurin)
#
# Agurin_individual <- matchHistoryStats(Agurin)
# Agurin_individual$match_stats[[1]]$summonerName %>% length()
#
# champ <- function (match){
#   nm <- match %>%  select(championName)
#   return(nm)
# }
# (dd <- lapply(Agurin_individual$match_stats,champ) %>% unlist() %>% factor() %>% table())# %>% max())
#
# matchChamps <- Agurin$closer_match_history_info[[1]]$info$participants$championName
# matchSummoners <- Agurin$closer_match_history_info[[1]]$info$participants$summonerName
# c(matchChamps,matchSummoners)
#
# winRate(Agurin_individual)
# championName <- getStat(Agurin_individual, "championName")
# assists <- getStat(Agurin_individual, "assists")
# goldEarned <- getStat(Agurin_individual, "goldEarned")
#
# str(assists)
# summary(assists)
# summary(championName)
#
# sum(championName=="Khazix")
#
# hist(goldEarned, main = "earned golds")
#
# c_occurrences  <- champsPlayed(Agurin_individual)
#
# Peng <- summonerInfo(matchH[[17]]$summoner[8])

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