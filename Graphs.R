library()

goldose <- closer_match_history_info[[1]]$info$frames$participantFrames$`1`$currentGold

hist(goldose)
hist(goldose, breaks=20, main="Neeh")

goldose
tim <- closer_match_history_info[[1]]$info$frames$timestamp / 1000 / 60 #minutes

tim
# %>%
# second()

neehh <- tibble(goldose,tim)

p <- ggplot(neehh, aes(x=tim, y=goldose)) +
  geom_line() +
  xlab("")

p

game1 <- closer_match_history_info[[1]]
timestampApi <- game1$info$frames$timestamp

participant1 <- game1$info$frames$participantFrames$`1`


participant1$minionsKilled
participant1$currentGold
participant1 %>% names()
timestampApi <- game$info$frames$timestamp


gameParticipantInfo <- function (player){
  xp <- player$xp
  crG <- player$currentGold
  ms <- player$minionsKilled
  return(player$participantId <- tibble(xp,crG,ms))
}

playersInfo <- game1$info$frames$participantFrames

playersInfoApplied <- lapply(playersInfo,gameParticipantInfo)

(j <- ggplot(playersInfoApplied$`1`, aes(x=timestampApi, y=xp)) +
  geom_line() +
  xlab(""))

