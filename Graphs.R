library('ggthemes')

goldose <- closer_match_history_info[[1]]$
  info$
  frames$
  participantFrames$
  `1`$
  currentGold
goldose

hist(goldose)
hist(goldose, breaks = 20, main = "Neeh")

tim <- closer_match_history_info[[1]]$
  info$
  frames$
  timestamp /
  1000 /
  60 #minutes
tim
# %>%
# second()

neehh <- tibble(goldose, tim)

p <- ggplot(neehh, aes(x = tim, y = goldose)) +
  geom_line() +
  xlab("")

p

game1 <- closer_match_history_info[[1]]
timestampApi <- game1$info$frames$timestamp

participant1 <- game1$info$frames$participantFrames$`1`

participant1$minionsKilled
participant1$currentGold
participant1 %>% names()
timestampApi <- game1$info$frames$timestamp / 1000 / 60

timestampApi

gameParticipantInfo <- function(player) {
  xp <- player$xp
  crG <- player$currentGold
  ms <- player$minionsKilled
  return(player$participantId <- tibble(xp, crG, ms))
}

playersInfo <- game1$info$frames$participantFrames

playersInfoApplied <- lapply(playersInfo, gameParticipantInfo)

playersInfoApplied$`1`
#   (p <- ggplot(playersInfoApplied$`1`,aes(x = timestampApi)) +
#   geom_point() +
#   geom_line() +
#   facet_wrap(vars(xp, crG, ms)))
#   labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents"))
ggplot(playersInfoApplied$`1`, aes(timestampApi, crG)) +
  geom_line() +
  facet_wrap(vars(xp))+
  theme_dark()
#Rnd - not working right now

(goldOvertime <- ggplot(playersInfoApplied$`1`, aes(x = timestampApi, y = crG)) +
  geom_line() +
  xlab(""))

(xpOverTime <- ggplot(playersInfoApplied$`1`, aes(x = timestampApi, y = xp)) +
  geom_line() +
  xlab(""))



