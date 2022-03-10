Agurin <- summonerInfo('Agurin', 'euw1', 0, 20)
Agurin_individual <- matchHistoryStats(Agurin)

winRate(Agurin_individual)
championName <- getStat(Agurin_individual, "championName")
assists <- getStat(Agurin_individual, "assists")
goldEarned <- getStat(Agurin_individual, "goldEarned")

firstMatch <- summonersFromMatch(Agurin$closer_match_history_info[[1]])
matchH <- summonersFromMatchHistory(Agurin)

cPlayed <- champsPlayed(Agurin)

gg <- matchHistoryStats(Agurin,TRUE)
# %>% unlist() %>% tibble()
match <- gg$match_stats[[1]]
match2 <- gg$match_stats[[2]]

match <- match[,-c(4,57)]
match2 <- match2[, -c(4,57)]

mmfinal <- rbind(match,match2)

gg$summonerName[[1]]

write.csv(mmfinal,paste0(getwd(),"/",Agurin$summonerInfo$name,".csv"),row.names = TRUE, col.names = TRUE)


